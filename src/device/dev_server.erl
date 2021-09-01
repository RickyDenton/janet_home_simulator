%% This module represents the server in the JANET device application used for interfacing
%% interfacing both with the JANET Simulator and its location's JANET Controller node

-module(dev_server).
-behaviour(gen_server).

-export([start_link/0,init/1,handle_call/3,handle_cast/2,handle_continue/2,handle_info/2]).  % gen_server Behaviour Callback Functions
-export([dev_reg_cli/3]).                                                                    % 'dev_server' controller registration client

%% This record represents the state of a dev_server gen_server
-record(devsrvstate,    
        {
		 dev_state,       % The device's current state (booting|connecting|online)
		 dev_id,          % The device's ID
		 loc_id,          % The device's location ID
		 mgr_pid,         % The PID of the device manager in the JANET Simulator node
		 devhandler_pid,  % The PID of the device handler in the JANET Controller node
		 devhandler_mon   % A reference used for monitoring the device handler assigned to the 'dev_server' in the controller node
		}).

% Maximum size of the buffer used for postponing device configuration updates towards the controller while not registered with it
-define(Max_handler_buffer_size, 100).  

%%====================================================================================================================================
%%                                                  GEN_SERVER CALLBACK FUNCTIONS                                                        
%%====================================================================================================================================

%% ============================================================ INIT ============================================================ %%
init(_) ->

 %% [TODO]: Trap Exit signals?
 %% process_flag(trap_exit,true),
 
 % Return the server initial state, where further initialization operations will be performed in the "handle_continue(Continue,State)"
 % callback function for parallelization purposes (and for allowing the dev_manager process to respond to the registration request)  
 {ok,#devsrvstate{dev_state = booting, _ = none},{continue,init}}.
 
 
%% ======================================================= HANDLE_CONTINUE ======================================================= %%  

%% Registers the JANET device node with its device manager in the JANET simulator node and spawns
%% the 'dev_reg_cli' process for registering the device with its location JANET Controller node
handle_continue(init,SrvState) when SrvState#devsrvstate.dev_state =:= booting ->

 % Retrieve the 'mgr_pid' environment variable
 {ok,MgrPid} = application:get_env(mgr_pid),
 
 % Attempt to register the JANET device node with its device
 % manager in the JANET simulator node by passing its PID
 %
 % NOTE: This registration request is performed synchronously since
 %       the device node's execution cannot continue if it fails 
 %
 ok = gen_server:call(MgrPid,{dev_reg,self()},10000),
 
 
 
 % Retrieve the device's state machine (supposedly) initial configuration
 %
 % NOTE: This operation must succeed, otherwise the
 %       device node's execution should not continue
 %
 %% [TODO]: Type hack for preventing non-fan devices from crashes, remove when all devices are ready
 %% ---------------------------------------------------------
 {ok,Type} = application:get_env(type),

 if
  Type =:= fan orelse Type =:= light orelse Type =:= door ->
  
   % Initialize the process dictionary variable used for buffering postponed configuration updates
   % for the device handler to the initial configuration and timestamp of the device's state machine
   {ok,{InitCfg,Timestamp}} = gen_statem:call(dev_statem,get_config,4500),
   put(handler_buffer,[{InitCfg,Timestamp}]);
  
  %% [TODO]: Remove
  true ->
	put(handler_buffer,[])
 end,
 %% ---------------------------------------------------------
 
 
 
 % Retrieve the device and location IDs from the
 % 'dev_id' and 'loc_id' environment variables
 {ok,Loc_id} = application:get_env(loc_id),
 {ok,Dev_id} = application:get_env(dev_id),

 % Spawn the 'dev_reg_cli' client for attempting to register the device with its controller node
 spawn_link(?MODULE,dev_reg_cli,[Loc_id,Dev_id,self()]),

 % Return the server initial state
 {noreply,#devsrvstate{dev_state = connecting, dev_id = Dev_id, loc_id = Loc_id, mgr_pid = MgrPid, _ = none}}.


%% ========================================================= HANDLE_CALL ========================================================= %%

%% SENDER:    The device node's manager in the JANET Simulator node OR
%%            the device's handler in the JANET Controller node 
%% WHEN:      -
%% PURPOSE:   Change the configuration of the device's state machine 
%% CONTENTS:  The requested new configuration of the device's state machine (whose validity is not checked for here)
%% MATCHES:   (always) (when the requests comes from the device manager in the JANET Simulator or the device handler in the JANET Controller)
%% ACTIONS:   1) Forward the configuration change command to the device's state machine and return its response to the caller
%%            2) If the configuration change update was successful, inform the other actor which did not perform the call
%%               of the updated device configuration and timestamp, storing it in the 'handler_buffer' process dictionary variable
%%               in case the call came from the JANET Simulator and the device is not registered within the controller 
%% ANSWER:    The reply of the device's state machine
%% NEW STATE: -
%%
handle_call({dev_config_change,NewCfg},{ReqPid,_},SrvState) when ReqPid =:= SrvState#devsrvstate.mgr_pid orelse 
                                                                 ReqPid =:= SrvState#devsrvstate.devhandler_pid ->
 
 % Forward the configuration change command to the device's state machine and collect its response
 CfgChangeRes = try gen_statem:call(dev_statem,{dev_config_change,NewCfg},4500)
 catch
  exit:{timeout,_} ->
  
   % dev_statem timeout
   {error,statem_timeout}
 end,
 
 % Depending on the result of the device configuration change
 case CfgChangeRes of
  {ok,{UpdatedCfg,Timestamp}} ->
 
   % If the device configuration update was successful, depending on where the request came from
   if
    ReqPid =:= SrvState#devsrvstate.mgr_pid ->
	
     % If the request came from the device's manager, attempt to forward
	 % the updated configuration and timestamp to the JANET Controller 
     push_or_store_ctr_update(SrvState#devsrvstate.dev_state,SrvState#devsrvstate.devhandler_pid,{UpdatedCfg,Timestamp});

 	ReqPid =:= SrvState#devsrvstate.devhandler_pid ->
	
	 % If instead the request came from the device's handler, forward
	 % the updated configuration and timestamp to the device manager 
     gen_server:cast(SrvState#devsrvstate.mgr_pid,{dev_config_update,self(),{UpdatedCfg,Timestamp}})
   end,
   
   % Respond to the caller
   {reply,{ok,{UpdatedCfg,Timestamp}},SrvState};
   
  _ -> 
  
   % Otherwise if the device configuration update was not successful
   % ('statem_timeout' included), just return the error to the caller
   {reply,CfgChangeRes,SrvState}
 end;
 
 
%% SENDER:    The device node's manager in the JANET Simulator node
%% WHEN:      (varies) [TODO]: Double-check
%% PURPOSE:   Execute a command on the device node and return the result of the operation
%% CONTENTS:  The Module, Function and ArgsList to be evaluated via apply()
%% MATCHES:   When the 'dev_server' is registered (and the request comes from the JANET Simulator)
%% ACTIONS:   Execute the required command via apply()
%% ANSWER:    The result of the apply() function
%% NEW STATE: -
%%
handle_call({dev_command,Module,Function,ArgsList},{ReqPid,_},SrvState) when    SrvState#devsrvstate.dev_state =/= booting
                                                                        andalso ReqPid =:= SrvState#devsrvstate.mgr_pid    ->
 % Execute the required command and return its result
 {reply,apply(Module,Function,ArgsList),SrvState};  



%% DEBUGGING PURPOSES [TODO]: REMOVE
handle_call(_,{ReqPid,_},SrvState) ->
 io:format("[dev_srv-~w]: <WARNING> Generic response issued to ReqPid = ~w~n",[SrvState#devsrvstate.dev_id,ReqPid]),
 {reply,gen_response,SrvState}.


%% ========================================================= HANDLE_CAST ========================================================= %% 

%% SENDER:    The device controller registration client ('dev_reg_cli' process)
%% WHEN:      When it successfully registered the device within the controller node
%% PURPOSE:   Inform the 'dev_server' of the successful registration
%% CONTENTS:  1) The PID of the device controller registration client ("security purposes")
%%            2) The PID of the 'ctr_devhandler' server assigned to the device in the controller node
%% MATCHES:   When the 'dev_server' is connecting (and the request comes from the JANET Device)
%% ACTIONS:   Create a monitor towards the 'ctr_devhandler' server in the controller node and
%%            inform the device manager in the JANET Simulator that the device is now online
%% NEW STATE: Update the device state to 'online' and set the 'devhandler_pid' and 'devhandler_mon' variables
%%
handle_cast({reg_success,DevRegCli,Devhandler_Pid},SrvState) when SrvState#devsrvstate.dev_state =:= connecting andalso node(DevRegCli) =:= node() ->

 % Old logging
 % io:format("[dev_server-~w]: Registration Success (devhandler PID = ~w)~n",[SrvState#devsrvstate.dev_id,Devhandler_Pid]),

 % Create a monitor towards the handler assigned to the device in the controller node
 %
 % NOTE: If not fired this monitor is not explicitly deleted by the 'dev_server' process via the "demonitor(MonRef)"
 %       BIF, even if supposedly it is automatically released when the process terminates (the documentation is unclear) 
 %
 Mon_Ref = monitor(process,Devhandler_Pid),

 % Inform the device manager that the device is now online
 gen_server:cast(SrvState#devsrvstate.mgr_pid,{dev_srv_state_update,online,self()}),
 
 % Retrieve all postponed device configuration updates stored in the 'handler_buffer' process dictionary
 % variable (clearing it in the process) and forward them to the assigned handler in the controller node
 push_postponed_cfg_updates(erase(handler_buffer),Devhandler_Pid),
 
 % Update the device state to 'online' and set the 'devhandler_pid' and 'dev_handler_ref' variables
 {noreply,SrvState#devsrvstate{dev_state = online, devhandler_pid = Devhandler_Pid, devhandler_mon = Mon_Ref}}; 
 

%% SENDER:    The device's 'dev_statem' process
%% WHEN:      When the device's state machine configuration changes
%% PURPOSE:   Propagate the updated device configuration to the device manager in
%%            the JANET Simulator and the device handler in the JANET Controller
%% CONTENTS:  1) The updated configuration of the device's state machine
%%            2) The timestamp of the updated configuration
%% MATCHES:   (always)
%% ACTIONS:   1) Forward the updated device configuration and timestamp
%%               to the device manager in the JANET Simulator
%%            2) If registered within its controller, forward the updated device 
%%               configuration and timestamp to the device handler, otherwise
%%               store them in the 'handler_buffer' process dictionary variable 
%% NEW STATE: -
%%
handle_cast({dev_config_update,{UpdatedCfg,Timestamp}},SrvState) ->

 % Forward the updated device configuration and its timestamp to the device manager in the JANET Simulator node
 gen_server:cast(SrvState#devsrvstate.mgr_pid,{dev_config_update,self(),{UpdatedCfg,Timestamp}}),
 
 % If the device is registered within the controller node, forward it the updated device configuration
 % and timestamp, otherwise store them in the 'handler_buffer' process dictionary variable 
 push_or_store_ctr_update(SrvState#devsrvstate.dev_state,SrvState#devsrvstate.devhandler_pid,{UpdatedCfg,Timestamp}),

 % Keep the server state
 {noreply,SrvState};


%% --------- STUB
handle_cast(reset,State) -> % Resets the server State
 {noreply,State}.


%% ========================================================= HANDLE_INFO ========================================================= %%  

%% SENDER:    The Erlang Run-Time System (ERTS)
%% WHEN:      When the monitored 'ctr_devhandler' process on the controller node terminates
%% PURPOSE:   Inform of the 'ctr_devhandler' process termination
%% CONTENTS:  1) The monitor reference the notification refers to
%%            2) The PID of the process that was monitored (the 'ctr_devhandler' process)
%%            3) The reason for the monitored process's termination
%% MATCHES:   When the device node is online (dev_state = 'online')
%% ACTIONS:   1) If Reason =:= 'noproc' log the event (it should not happen)
%%            2) Inform the device manager in the JANET Simulator that the device is no longer online
%%            3) Respawn the 'dev_reg_cli' client for attempting to re-register the device with the controller 
%% ANSWER:    -
%% NEW STATE: Update the 'dev_state' to 'connecting' and reset the 'devhandler_pid' and 'devhandler_mon' variables
%%
handle_info({'DOWN',MonRef,process,Devhandler_pid,Reason},SrvState) when MonRef =:= SrvState#devsrvstate.devhandler_mon,
                                                                         Devhandler_pid =:= SrvState#devsrvstate.devhandler_pid,
																		 SrvState#devsrvstate.dev_state =:= online ->
 
 % If Reason =:= 'noproc', which is associated to the fact that the 'ctr_regserver' passed a non-existing
 % "Devhandler_pid" or the devhandler process died before the monitor could be established, log the error
 if
  Reason =:= noproc ->
   io:format("[dev_server-~w]: <WARNING> The assigned 'ctr_devhandler' process does not exist~n",[SrvState#devsrvstate.dev_id]);
  true ->
   ok
 end,   
 
 % Inform the device manager that the device is no longer online
 gen_server:cast(SrvState#devsrvstate.mgr_pid,{dev_srv_state_update,connecting,self()}),
 
 % Respawn the 'dev_reg_cli' client for attempting to re-register the device with the controller node
 spawn_link(?MODULE,dev_reg_cli,[SrvState#devsrvstate.loc_id,SrvState#devsrvstate.dev_id,self()]),
 
 % Update the 'dev_state' to 'connecting' and reset the 'devhandler_pid' and 'devhandler_mon' variables
 {noreply,SrvState#devsrvstate{dev_state = connecting, devhandler_pid = none, devhandler_mon = none}}.


%%====================================================================================================================================
%%                                                    PRIVATE HELPER FUNCTIONS
%%==================================================================================================================================== 

%% DESCRIPTION:  Body function of the homonymous process spawned by the 'dev_server'
%%               for attempting to register the device with its location controller
%%
%% ARGUMENTS:    - Loc_id:    The device's location ID
%%               - Dev_id:    The device's ID
%%               - DevSrvPid: The PID of the device's 'dev_server' process where to return the PID of the handler 
%%                            assigned to the device on the controller node when the registration is successful
%%
%% RETURNS:      - ok -> The registration was successful, and the PID of the handler assigned to the device
%%                       on the controller node was returned to the 'dev_server' process via a cast()
%%
dev_reg_cli(Loc_id,Dev_id,DevSrvPid) ->
 
 % Sleep for 1 second (this is to account the initial delay when the JANET
 % Simulator is started as well as a interval between consecutive attempts)
 timer:sleep(1000),
 
 % Attempt to register the device to the location controller via a synchronous request, catching possible exceptions
 RegResult = try gen_server:call({ctr_regserver,utils:str_to_atom("ctr-" ++ integer_to_list(Loc_id) ++ "@localhost")},{reg_request,Dev_id,DevSrvPid},1500)
 catch
  exit:{timeout,_} ->
  
   % Timeout on the controller node
   {error,ctr_timeout};
  
  exit:Error ->
  
   % Generic exit exception
   Error
 end,
 
 % Old logging
 % io:format("[dev_reg_cli-~w]: Registration Result:~w~n",[Dev_id,RegResult]),
 
 case RegResult of
 
  % If a timeout occured, re-attempt the registration with no further delay
  {error,ctr_timeout} ->
   dev_reg_cli(Loc_id,Dev_id,DevSrvPid);
   
  % If the registration was successful, return to the 'dev_server' process the
  % PID of the handler assigned to the device on the controller node via a cast()
  {ok,Devhandler_Pid} ->
   gen_server:cast(DevSrvPid,{reg_success,self(),Devhandler_Pid});

  % If a generic error occured, delay the execution for an
  % additional time interval before re-attempting the registration   
  _ ->
   timer:sleep(1000),
   dev_reg_cli(Loc_id,Dev_id,DevSrvPid) 
 end.


%% DESCRIPTION:  If the device node is registered within its controller, forwards it an updated device configuration
%%               and timestamp, otherwise stores them in the 'handler_buffer' process dictionary variable
%%
%% ARGUMENTS:    - DevState:               The current device state (connecting|online)
%%               - Devhandler_pid:         The PID of the device handler in the controller node, if any
%%               - {UpdatedCfg,Timestamp}: The updated device configuration and timestamp
%%
%% RETURNS:      - ok -> Device state update and timestamp forwarded to the controller
%%                       OR stored in the 'handler_buffer' process dictionary variable
%%
push_or_store_ctr_update(DevState,Devhandler_pid,{UpdatedCfg,Timestamp}) ->

 % Check if the 'dev_server' is registered within the controller node
 case DevState of
  online ->
  
   % If it is registered, forward the updated device configuration
   % and its timestamp to the device handler in the controller
   gen_server:cast(Devhandler_pid,{dev_config_update,self(),{UpdatedCfg,Timestamp}});
   
  connecting ->

   % Otherwise, retrieve the contents of the
   % 'handler_buffer' process dictionary variable
   HandlerBuffer = get(handler_buffer),
   if
    length(HandlerBuffer) =:= ?Max_handler_buffer_size ->
	 
	 % If the buffer has reached its maximum size, drop its
	 % last element and append it the new configuration update
	 [_|T] = HandlerBuffer,
	 put(handler_buffer,T ++ [{UpdatedCfg,Timestamp}]);
	 
    true ->
	
	 % Otherwise just append it the new configuration update
	 put(handler_buffer,HandlerBuffer ++ [{UpdatedCfg,Timestamp}])
   end,

   % Logging purposes
   %% [TODO]: REMOVE
   {ok,Dev_id} = application:get_env(dev_id),
   io:format("[dev_srv-~w]: handler_buffer: ~w~n",[Dev_id,get(handler_buffer)])
 end,
 ok.


%% DESCRIPTION:  Forwards all postponed device configuration updates to the device handler in the controller node
%%
%% ARGUMENTS:    - [{UpdatedCfg,Timestamp}]: The list of postponed device configuration updates
%%               - Devhandler_pid:           The PID of the device handler in the controller node
%%
%% RETURNS:      - ok -> All postponed device configuration updates were forwarded to the device handler
%%
push_postponed_cfg_updates([],_) ->

 % Reset the 'handler_buffer' process dictionary variable to the empty list
 put(handler_buffer,[]),
 
 % Return
 ok;
 
push_postponed_cfg_updates([{UpdatedCfg,Timestamp}|NextCfgUpdate],Devhandler_pid) ->

 % Push the current configuration update
 gen_server:cast(Devhandler_pid,{dev_config_update,self(),{UpdatedCfg,Timestamp}}),
 
 % Proceed with the next configuration update
 push_postponed_cfg_updates(NextCfgUpdate,Devhandler_pid).


%%====================================================================================================================================
%%                                                         START FUNCTION                                                        
%%==================================================================================================================================== 

%% Called by its 'sup_jdev' supervisor during the JANET Device boot
start_link() ->
 gen_server:start_link({local,?MODULE},?MODULE,[],[]).  % The spawned process is also registered locally under the 'dev_server' name