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
 
 % Retrieve the PID of the device's state
 % machine via the local naming registry
 case whereis(dev_statem) of
  undefined ->
  
   % The device state machine not being registered in the local naming registry is a
   % fatal error, and dev_server, along with the entire device node, must be shut down
   {stop,dev_statem_not_registered,booting};
   
  Dev_statem_pid ->
  
   % If the PID of the device state machine was successfully retrieved, attempt
   % to register the JANET device node with its device manager in the simulator
   % node by passing the PIDs of the 'dev_statem' and of the 'dev_server'
   %
   % NOTE: This registration request is performed synchronously since
   %       the controller node's execution cannot continue if it fails 
   gen_server:call(MgrPid,{dev_reg,self(),Dev_statem_pid},10000),

   % Retrieve the device and location IDs from the
   % 'dev_id' and 'loc_id' environment variables
   {ok,Loc_id} = application:get_env(loc_id),
   {ok,Dev_id} = application:get_env(dev_id),

   % Spawn the 'dev_reg_cli' client for attempting to register the device with the controller node
   spawn_link(?MODULE,dev_reg_cli,[Loc_id,Dev_id,self()]),

   % Return the server initial state
   {noreply,#devsrvstate{dev_state = connecting, dev_id = Dev_id, loc_id = Loc_id, mgr_pid = MgrPid, _ = none}}
 end.


%% ========================================================= HANDLE_CALL ========================================================= %%

%% SENDER:    The device node's manager
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
 io:format("[devsrv-~w]: <WARNING> Generic response issued to ReqPid = ~w~n",[SrvState#devsrvstate.dev_id,ReqPid]),
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
 
 % Update the device state to 'online' and set the 'devhandler_pid' and 'dev_handler_ref' variables
 {noreply,SrvState#devsrvstate{dev_state = online, devhandler_pid = Devhandler_Pid, devhandler_mon = Mon_Ref}}; 
 

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


%%====================================================================================================================================
%%                                                         START FUNCTION                                                        
%%==================================================================================================================================== 

%% Called by its 'sup_jdev' supervisor during the JANET Device boot
start_link() ->
 gen_server:start_link({local,?MODULE},?MODULE,[],[]).