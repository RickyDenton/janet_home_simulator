%% This module represents the communication server in the JANET device application, which interfaces both
%% with its 'dev_manager' on the JANET Simulator node and its 'ctr_devhandler' on its JANET Controller node

-module(dev_server).
-behaviour(gen_server).

-export([start_link/0,init/1,handle_call/3,handle_cast/2,handle_continue/2,handle_info/2]).  % gen_server Behaviour Callback Functions

%% This record represents the state of a 'dev_server' gen_server
-record(devsrvstate,    
        {
		 dev_state,       % The device's current state (booting|connecting|online)
		 devsrv_pid,      % The PID of the dev_server process
		 dev_id,          % The device's ID
		 loc_id,          % The device's location ID
		 ctr_hostname,    % The name of the host where the location controller node is deployed
		 mgr_pid,         % The PID of the device manager in the JANET Simulator node
		 devhandler_pid,  % The PID of the device handler in the JANET Controller node
		 devhandler_mon,  % A reference used for monitoring the device handler assigned to the 'dev_server' in the controller node
		 cfg_backlog      % The backlog of configuration updates to be sent to the Controller once paired with it
		}).


% Maximum size of the backlog used for postponing device configuration
% updates towards the controller while not paired with it ('cfg_backlog')
-define(Max_cfg_backlog_size,100).  

%%====================================================================================================================================
%%                                                  GEN_SERVER CALLBACK FUNCTIONS                                                        
%%====================================================================================================================================

%% ============================================================ INIT ============================================================ %%
init(_) ->
 
 % Return the server initial state, where further initialization operations will be performed in the "handle_continue(Continue,State)"
 % callback function for parallelization purposes (and for allowing the 'dev_manager' process to respond to the registration request)  
 {ok,#devsrvstate{dev_state = booting, _ = none},{continue,init}}.
 
 
%% ======================================================= HANDLE_CONTINUE ======================================================= %%  

%% Registers the JANET device node with its device manager in the JANET simulator node and spawns
%% the 'ctr_pairer' process for pairing the device with its locaiton's JANET Controller node
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
 % and store it in the configuration updates backlog so to immediately
 % forward it to the controller as soon as the device pairs with it
 %
 % NOTE: This operation must succeed, otherwise the
 %       device node's execution should not continue
 %
 {ok,{InitCfg,Timestamp}} = gen_statem:call(dev_statem,get_config,4500),
 
 % Retrieve the device ID, the location ID, and the host name where the
 % location controller is deployed in from the environment variables
 {ok,Loc_id} = application:get_env(loc_id),
 {ok,Dev_id} = application:get_env(dev_id),
 {ok,CtrHostName} = application:get_env(ctr_hostname),

 % Get the current process ID
 DevSrvPID = self(),

 % Spawn the 'ctr_pairer' client for attempting to pair the device with its controller node
 proc_lib:spawn_link(fun() -> ctr_pairer(Loc_id,Dev_id,DevSrvPID,CtrHostName) end),

 % Return the server initial state
 {noreply,#devsrvstate{dev_state = connecting, devsrv_pid = DevSrvPID, dev_id = Dev_id, loc_id = Loc_id, ctr_hostname = CtrHostName, mgr_pid = MgrPid, cfg_backlog = [{InitCfg,Timestamp}], _ = none}}.


%% ========================================================= HANDLE_CALL ========================================================= %%

%% DEV_CONFIG_CHANGE
%% -----------------
%% SENDER:    The device manager in the JANET Simulator node OR the device handler in the JANET Controller node 
%% WHEN:      -
%% PURPOSE:   Change the configuration of the device's state machine (i.e. issue a command to it) 
%% CONTENTS:  The desired new configuration of the device's state machine (whose validity is not checked for here)
%% MATCHES:   (always) (if the request comes from the device manager in the JANET Simulator or the device handler in the JANET Controller)
%% ACTIONS:   1) If the configuration command was successful, attempt in the first place to forward the updated device configuration and 
%%               timestamp to the actor (JANET Simulator or JANET Controller) which did not issue the request, possibly storing it in
%%               the configuration backlog if the request came from the JANET Simulator and the device is not paired with its controller 
%%            2) Return the result of the configuration command (updated configuration and timestamp or error) to the caller
%% ANSWER:    The reply of the device's state machine
%% NEW STATE: If the configuration command was successful, possibly update the configuration updates backlog
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
 
 % Depending on the result of the device configuration change command
 case CfgChangeRes of
  {ok,{UpdatedCfg,Timestamp}} ->
 
   % If it was successful, attempt in the first place to push the updated configuration and
   % timestamp to the actor (the JANET Simulator or the controller device handler) that did
   % not request it, obtaining the new value of the configuration updates backlog
   NewCfgBacklog = 
   if
    ReqPid =:= SrvState#devsrvstate.mgr_pid ->

    % If the request came from the device manager in the JANET Simulator attempt to forward the
	% configuration update and timestamp to the JANET Controller (which is performed only if the device
	% is currently paired with it), obtaining the updated value of the configuration updates backlog
    push_or_store_ctr_update(SrvState#devsrvstate.dev_state,SrvState#devsrvstate.cfg_backlog,
                             SrvState#devsrvstate.devhandler_pid,{UpdatedCfg,Timestamp},SrvState#devsrvstate.dev_id);

 	ReqPid =:= SrvState#devsrvstate.devhandler_pid ->
	
	 % If instead the request came from the JANET Controller, directly forward the updated configuration
	 % and timestamp to the JANET Simulator (being the device and simulator necessarily connected), and
	 % keep the same configuration updates backlog
     gen_server:cast(SrvState#devsrvstate.mgr_pid,{dev_config_update,self(),{UpdatedCfg,Timestamp}}),
	 SrvState#devsrvstate.cfg_backlog
   end,

   % Return the updated configuration and timestamp to the
   % caller, and possibly update the "cfg_backlog" state variable
   {reply,{ok,{UpdatedCfg,Timestamp}},SrvState#devsrvstate{cfg_backlog = NewCfgBacklog}};
   
  _ -> 
  
   % Otherwise if the device configuration change was not successful
   % ('statem_timeout' included), just return the error to the caller
   {reply,CfgChangeRes,SrvState}
 end;


%% DEV_COMMAND
%% ----------- 
%% SENDER:    The device manager in the JANET Simulator node
%% WHEN:      -
%% PURPOSE:   Evaluate a function in the device node and return its result
%% CONTENTS:  The "Module", "Function" and "ArgsList" to be evaluated via apply()
%% MATCHES:   When the 'dev_server' is registered (and the request comes from the JANET Simulator)
%% ACTIONS:   Execute the required function via apply()
%% ANSWER:    The result of the apply() function
%% NEW STATE: -
%%
handle_call({dev_command,Module,Function,ArgsList},{ReqPid,_},SrvState) when    SrvState#devsrvstate.dev_state =/= booting
                                                                        andalso ReqPid =:= SrvState#devsrvstate.mgr_pid    ->
 % Execute the required function and return its result
 {reply,apply(Module,Function,ArgsList),SrvState};  


%% Unexpected call
handle_call(Request,From,SrvState=#devsrvstate{dev_id = Dev_id}) ->
  
 % Report that an unexpected call was received by this gen_server
 io:format("[dev_srv-~w]: <WARNING> Unexpected call (Request = ~p, From = ~p, SrvState = ~p)~n",[Dev_id,Request,From,SrvState]),

 % Reply with a stub message and keep the SrvState
 {reply,unsupported,SrvState}.
 

%% ========================================================= HANDLE_CAST ========================================================= %% 

%% PAIR_SUCCESS
%% ------------ 
%% SENDER:    The device's 'ctr_pairer' client
%% WHEN:      When it has successfully paired the device with the controller node
%% PURPOSE:   Inform the 'dev_server' of the successful pairing
%% CONTENTS:  1) The PID of the device's 'ctr_pairer' client ("security purposes")
%%            2) The PID of the handler assigned to the device in the controller node
%% MATCHES:   When the 'dev_server' is connecting (and the request comes from the JANET Device)
%% ACTIONS:   1) Create a monitor towards the device handler in the controller node
%%            2) Push any deferred configuration updates in the backlog to the controller and clear it
%%            3) Inform the device manager in the JANET Simulator node that the device is now online
%% NEW STATE: Update the device state to 'online', clear the configuration updates
%%            backlog and set the 'devhandler_pid' and 'devhandler_mon' variables
%%
handle_cast({pair_success,PairerPID,Devhandler_Pid},SrvState) when SrvState#devsrvstate.dev_state =:= connecting andalso node(PairerPID) =:= node() ->

 % Create a monitor towards the handler assigned to the device in the controller node
 %
 % NOTE: If not fired this monitor is NOT explicitly deleted by the 'dev_server' process via the "demonitor(MonRef)"
 %       BIF, even if supposedly it is automatically released when the process terminates (the documentation is unclear) 
 %
 Mon_Ref = monitor(process,Devhandler_Pid),

 % Inform the device manager that the device is now online
 gen_server:cast(SrvState#devsrvstate.mgr_pid,{dev_srv_state_update,online,self()}),
 
 % Depending on whether there are deferred configuration
 % updates in the backlog to be sent to the controller
 case SrvState#devsrvstate.cfg_backlog of
  
  % If there are none, do nothing
  [] ->
   ok;
 
  % If there are, send their list to the device handler in the controller node  
  CfgBacklog -> 
   gen_server:cast(Devhandler_Pid,{dev_config_update,self(),CfgBacklog})
 end,
 
 % Update the device state to 'online', clear the configuration updates
 % backlog and set the 'devhandler_pid' and 'dev_handler_ref' variables
 {noreply,SrvState#devsrvstate{dev_state = online, devhandler_pid = Devhandler_Pid, devhandler_mon = Mon_Ref, cfg_backlog = []}}; 
 

%% DEV_CONFIG_UPDATE
%% -----------------
%% SENDER:    The device's 'dev_statem' process
%% WHEN:      When the device's state machine configuration changes
%% PURPOSE:   Propagate the updated device configuration to the device manager in
%%            the JANET Simulator and the device handler in the JANET Controller
%% CONTENTS:  1) The updated configuration of the device's state machine
%%            2) The timestamp of the updated configuration
%% MATCHES:   (always)
%% ACTIONS:   1) Forward the updated device configuration and timestamp
%%               to the device manager in the JANET Simulator
%%            2) If the device is paired with its controller, forward the
%%               updated configuration and timestamp to its device handler,
%%               otherwise store them in the configuration updates backlog
%% NEW STATE: Possibly update the 'cfg_backlog' state variable
%%
handle_cast({dev_config_update,{UpdatedCfg,Timestamp}},SrvState) ->

 % Forward the updated device configuration and timestamp to the device manager in the JANET Simulator node
 gen_server:cast(SrvState#devsrvstate.mgr_pid,{dev_config_update,self(),{UpdatedCfg,Timestamp}}),
 
 % Attempt to forward the updated device configuration and timestamp to the device handler in
 % the controller node, obtaining the updated value of the "cfg_backlog" server state variable
 NewCfgBacklog = push_or_store_ctr_update(SrvState#devsrvstate.dev_state,SrvState#devsrvstate.cfg_backlog,
                                          SrvState#devsrvstate.devhandler_pid,{UpdatedCfg,Timestamp},SrvState#devsrvstate.dev_id),

 % Possibly update the 'cfg_backlog' state variable
 {noreply,SrvState#devsrvstate{cfg_backlog = NewCfgBacklog}};
 

%% Unexpected cast
handle_cast(Request,SrvState=#devsrvstate{dev_id = Dev_id}) ->

 % Report that this gen_server should not receive cast requests
 io:format("[dev_srv-~w]: <WARNING> Unexpected cast (Request = ~w, SrvState = ~w)~n",[Dev_id,Request,SrvState]),

 % Keep the SrvState
 {noreply,SrvState}.
 
 
%% ========================================================= HANDLE_INFO ========================================================= %%  

%% CONTROLLER NODE DOWN
%% --------------------
%% SENDER:    The Erlang Run-Time System (ERTS)
%% WHEN:      When the monitored 'ctr_devhandler' process in the controller node terminates
%% PURPOSE:   Inform that the 'ctr_devhandler' process has terminated
%% CONTENTS:  1) The Reference associated with the monitor
%%            2) The PID of the process that was monitored (the 'ctr_devhandler' process)
%%            3) The Reason for the monitored process termination
%% MATCHES:   When the device node is online (dev_state = 'online')
%% ACTIONS:   1) If Reason =:= 'noproc' log the error (it should not happen)
%%            2) Inform the device manager in the JANET Simulator that the device is no longer online
%%            3) Respawn the 'ctr_pairer' client for attempting to re-pair the device with the controller 
%% ANSWER:    -
%% NEW STATE: Update the device state to 'connecting' and clear the 'devhandler_pid' and 'devhandler_mon' variables
%%
handle_info({'DOWN',MonRef,process,Devhandler_pid,Reason},SrvState) when MonRef =:= SrvState#devsrvstate.devhandler_mon,
                                                                         Devhandler_pid =:= SrvState#devsrvstate.devhandler_pid,
																		 SrvState#devsrvstate.dev_state =:= online ->
 
 % If Reason =:= 'noproc', which is associated to the fact that the 'ctr_pairserver' passed a non-existing
 % "Devhandler_pid" (or the devhandler process died before the monitor could be established), log the error
 if
  Reason =:= noproc ->
   io:format("[dev_server-~w]: <WARNING> The assigned 'ctr_devhandler' process does not exist~n",[SrvState#devsrvstate.dev_id]);
  true ->
   ok
 end,   
 
 % Inform the device manager that the device is no longer online
 gen_server:cast(SrvState#devsrvstate.mgr_pid,{dev_srv_state_update,connecting,self()}),
 
 % Respawn the 'ctr_pairer' client for attempting to re-pair the device with the controller node
 proc_lib:spawn_link(fun() -> ctr_pairer(SrvState#devsrvstate.loc_id,SrvState#devsrvstate.dev_id,SrvState#devsrvstate.devsrv_pid,SrvState#devsrvstate.ctr_hostname) end),
 
 % Update the device state to 'connecting' and reset the 'devhandler_pid' and 'devhandler_mon' variables
 {noreply,SrvState#devsrvstate{dev_state = connecting, devhandler_pid = none, devhandler_mon = none}}.


%%====================================================================================================================================
%%                                                    PRIVATE HELPER FUNCTIONS
%%==================================================================================================================================== 

%% DESCRIPTION:  Body function of the homonymous process spawned by the 'dev_server'
%%               for attempting to pair the device with its location controller
%%
%% ARGUMENTS:    - Loc_id:      The device's location ID
%%               - Dev_id:      The device's ID
%%               - DevSrvPid:   The PID of its parent 'dev_server' process
%%               - CtrHostName: The name of the host where the location controller node is deployed
%%
%% RETURNS:      - ok -> The pairing was successful, and the PID of the handler assigned to the device
%%                       in the controller node was returned to the 'dev_server' process via a cast()
%%
ctr_pairer(Loc_id,Dev_id,DevSrvPid,CtrHostName) ->
 
 % Sleep for 1 second (this is to account the initial delay when the JANET
 % Simulator is started as well as an interval between consecutive attempts)
 timer:sleep(1000),
 
 % Attempt to pair the device to the location controller via a synchronous request, catching possible exceptions
 PairingResult = try gen_server:call({ctr_pairserver,list_to_atom("ctr-" ++ integer_to_list(Loc_id) ++ "@" ++ CtrHostName)},{pair_request,Dev_id,DevSrvPid},1500)
 catch
  exit:{timeout,_} ->
  
   % Timeout on the controller node
   {error,ctr_timeout};
  
  exit:Error ->
  
   % Generic exit exception
   Error
 end,
 
 % Logging Purposes
 % io:format("[ctr_pairer-~w]: Pairing Result:~w~n",[Dev_id,PairingResult]),
 
 case PairingResult of
 
  % If a timeout occured, re-attempt the pairing with no further delay
  {error,ctr_timeout} ->
   ctr_pairer(Loc_id,Dev_id,DevSrvPid,CtrHostName);
   
  % If the pairing was successful, return the 'dev_server' parent process the
  % PID of the handler assigned to it in the controller node via a cast()
  {ok,Devhandler_Pid} ->
   gen_server:cast(DevSrvPid,{pair_success,self(),Devhandler_Pid});
 
  % If a generic error occured, delay the pairing
  % attempt of a set interval before retrying   
  _ ->
   timer:sleep(1000),
   ctr_pairer(Loc_id,Dev_id,DevSrvPid,CtrHostName) 
 end.


%% DESCRIPTION:  If the device node is paired with its controller, forwards it an updated device configuration
%%               and timestamp, otherwise stores them in the configuration updates backlog state variable
%%
%% ARGUMENTS:    - DevState:               The current device state ('connecting'|'online')
%%               - CfgBacklog:             The current value of the configuration updates backlog
%%               - Devhandler_pid:         The PID of the device handler in the controller node, if any
%%               - {UpdatedCfg,Timestamp}: The updated device configuration and timestamp to be forwarded or stored
%%               - Dev_id:                 The device ID (logging purposes)
%%
%% RETURNS:      - NewCfgBacklog -> The new value of the configuration updates backlog
%%
push_or_store_ctr_update(online,CfgBacklog,Devhandler_pid,{UpdatedCfg,Timestamp},_Dev_id) ->

 % If the 'dev_server is paired with the controller node, forward it the
 % updated device configuration and timestamp as a single-element list
 gen_server:cast(Devhandler_pid,{dev_config_update,self(),[{UpdatedCfg,Timestamp}]}),
   
 % Keep the same configuration updates backlog
 CfgBacklog;

push_or_store_ctr_update(connecting,CfgBacklog,_Devhandler_pid,{UpdatedCfg,Timestamp},Dev_id) ->

 % If the device is NOT paired with the controller node, depending on
 % whether the maximum size of the device updates backlog has been reached
 if
  length(CfgBacklog) =:= ?Max_cfg_backlog_size ->

   % If it has been reached, print a warning message
   % informing that the oldest device update is being dropped
   io:format("[dev_server-~w]: <WARNING> The maximum size of the device configuration updates backlog has been reached (~w), the oldest update is being dropped~n",[Dev_id,?Max_cfg_backlog_size]),
  
   % Drop the first (oldest) update in the backlog and appent it the new configuration update
   [_OldestUpdate|OtherUpdates] = CfgBacklog,
   OtherUpdates ++ [{UpdatedCfg,Timestamp}];
   
  true ->
  
   % Otherwise simply append the new configuration in the backlog
   CfgBacklog ++ [{UpdatedCfg,Timestamp}]
 end.


%%====================================================================================================================================
%%                                                         START FUNCTION                                                        
%%==================================================================================================================================== 

%% Called by its 'sup_jdev' supervisor during the JANET Device boot
start_link() ->
 gen_server:start_link({local,?MODULE},?MODULE,[],[]).  % The spawned process is also registered locally under the 'dev_server' name