%% This module represents a device's manager in the JANET Simulator application %%

-module(dev_manager).
-behaviour(gen_server).

-export([start_link/3,init/1,terminate/2,handle_call/3,handle_cast/2,handle_continue/2,handle_info/2]).  % gen_server Behaviour Callback Functions

-include("sim_mnesia_tables_definitions.hrl").  % Janet Simulator Mnesia Tables Records Definitions

%% This record represents the state of a dev_manager gen_server
-record(devmgrstate,    
        {
		 dev_state,     % The state of the managed device node
		 dev_node,      % The reference to the managed node
		 dev_srv_pid,   % The PID of the device's 'dev_server' process
		 dev_srv_mon,   % A reference used for monitoring the device's 'dev_server' process (and consequently the node)
		 dev_id,        % The device's ID
		 loc_id,        % The device's location ID
		 ctr_hostname   % The name of the host where the location controller node is deployed
		}).

%%====================================================================================================================================
%%                                                  GEN_SERVER CALLBACK FUNCTIONS                                                        
%%====================================================================================================================================

%% ============================================================ INIT ============================================================ %%
init({Dev_id,Loc_id,CtrHostName}) ->

 % Trap exit signals so to allow cleanup operations when terminating (terminate(Reason,SrvState) callback function)
 process_flag(trap_exit,true),
 
 % Register the manager in the 'devmanager' table
 {atomic,ok} = mnesia:transaction(fun() -> mnesia:write(#devmanager{dev_id=Dev_id,loc_id=Loc_id,mgr_pid=self(),status="BOOTING"}) end),
 
 % Return the server initial state, where the initialization of the device node will continue in
 % the "handle_continue(start_device_node,State)" callback function for parallelization purposes  
 {ok,#devmgrstate{dev_state = booting, dev_id = Dev_id, loc_id = Loc_id, ctr_hostname = CtrHostName, _ = none},{continue,start_device_node}}.
 
 
%% ======================================================= HANDLE_CONTINUE ======================================================= %%

%% Initializes the device's node (called right after the 'init' callback
%% function and should the controller node go down (DEVICE NODE DOWN)
handle_continue(start_device_node,SrvState) ->
 
 %% ---------------- Device Node Configuration Parameters Definition ---------------- %%
 
 % Retrieve the Dev_id and the Loc_id and convert them to strings
 Dev_id = SrvState#devmgrstate.dev_id,
 Loc_id = SrvState#devmgrstate.loc_id,
 Dev_id_str = integer_to_list(Dev_id),
 Loc_id_str = integer_to_list(Loc_id),
 
 % Retrieve the device record
 {ok,DeviceRecord} = db:get_record(device,Dev_id),
 
 % Retrieve the device's type and configuration
 Type = DeviceRecord#device.type,
 Config = DeviceRecord#device.config,
 
 %% ------------------------------ Device Node Creation ------------------------------ %% 
 
 % Prepare the Host, Name and Args parameters of the controller's node
 NodeHost = utils:get_effective_hostname(DeviceRecord#device.hostname),
 NodeName = "dev-" ++ Dev_id_str,
 NodeArgs = "-setcookie " ++ Loc_id_str ++ " -connect_all false -kernel net_ticktime 20 -env ERL_LIBS _build/default/lib/",
 
 % Set the cookie for allowing the Janet Simulator to connect with the device's node
 % NOTE: The use of atoms is required by the erlang:set_cookie BIF  
 erlang:set_cookie(list_to_atom("dev-" ++ Dev_id_str ++ "@" ++ NodeHost),list_to_atom(Loc_id_str)),
 
 % Attempt to start the device node and link it to the
 % manager for a predefined maximum number of attempts
 case utils:start_link_node(NodeHost,NodeName,NodeArgs,"dev_mgr",Dev_id,"device") of
  {error,Reason} ->
  
   % If an error persists in starting the device node, report it and stop the manager
   io:format("[dev_mgr-~w]: <ERROR> Device node could not be started (reason = ~p), stopping the manager~n",[Dev_id,Reason]),
   
   % Stop with reason 'normal' to prevent the 'sup_loc' supervisor from reattempting to respawn the manager
   % (which would be useless in the current situation given the persistent error in starting its device node) 
   {stop,normal,SrvState};
   
  {ok,Node} ->
  
   % Otherwise if the device node was successfully started and linked
   % with the manager, launch on it the Janet Device application
   ok = rpc:call(Node,jdev,run,[Dev_id,Loc_id,self(),Type,Config,SrvState#devmgrstate.ctr_hostname]),
 
   % Set the 'dev_node' state variable and wait for the
   % registration request of the device's dev_server process
   {noreply,SrvState#devmgrstate{dev_node = Node}}
 end.
  
 
%% ========================================================= HANDLE_CALL ========================================================= %% 

%% DEV_REG
%% -------
%% SENDER:    The device's 'dev_server' process
%% WHEN:      During the 'dev_server' initialization (handle_continue(init,_))
%% PURPOSE:   Device registration request
%% CONTENTS:  The dev_server's PID
%% MATCHES:   When the device is booting (and the request comes from the spawned device node)
%% ACTIONS:   Create a monitor towards the device's 'dev_server' process and
%%            update the device's state to "CONNECTING" in the 'devmanager' table
%% ANSWER:    'ok' (the device registration was successful)
%% NEW STATE: Update the device state to 'connecting and set the 'dev_srv_pid' and 'dev_srv_mon' variables
%%
%% NOTE:      This message can be received only once, since if the dev_server process crashes the entire
%%            device node is shut down by its application master (The JANET Device application is permanent)
%%
handle_call({dev_reg,DevSrvPid},{DevSrvPid,_},SrvState) when SrvState#devmgrstate.dev_state =:= booting andalso node(DevSrvPid) =:= SrvState#devmgrstate.dev_node ->

 % Create a monitor towards the device's 'dev_server' process
 MonRef = monitor(process,DevSrvPid),

 % Update the device node state to "CONNECTING" in the 'devmanager' table
 {atomic,ok} = mnesia:transaction(fun() -> mnesia:write(#devmanager{dev_id=SrvState#devmgrstate.dev_id,loc_id=SrvState#devmgrstate.loc_id,mgr_pid=self(),status="CONNECTING"}) end),
 
 % Log that the device has successfully booted
 io:format("[dev_mgr-~w]: Device node successfully booted~n",[SrvState#devmgrstate.dev_id]),
 
 % Confirm the device registration and update the 'dev_srv_pid','dev_srv_mon' and the 'dev_state' state variables
 {reply,ok,SrvState#devmgrstate{dev_state = connecting, dev_srv_pid = DevSrvPid, dev_srv_mon = MonRef}}; 


%% DEV_CONFIG_CHANGE
%% -----------------
%% SENDER:    The simulation controller (the user)
%% WHEN:      -
%% PURPOSE:   Change the state machine configuration in the managed device
%% CONTENTS:  The requested new configuration of the device's state machine (whose validity is not checked for here)
%% MATCHES:   When the device has booted and its 'dev_server' is registered, otherwise an
%%            error message is returned (and the request comes from the JANET Simulator node)
%% ACTIONS:   1) Forward the configuration change command to the 'dev_server' via a synchronous call
%%               2.1) If the 'dev_server' does not respond in a predefined timeout, return the error to the caller
%%               2.2) If an error is returned by the 'dev_server', propagate it to the caller
%%               2.3) If the configuration update was successful, update the
%%                    device's configuration in the Mnesia device table, where
%%                    3.1) If the transaction was successful, return the caller the 'dev_server' response
%%                    3.2) if the transaction was NOT successful, return its aborted Reason to the caller
%% ANSWER:    See above
%% NEW STATE: -
%%
handle_call({dev_config_change,NewCfg},{CommPid,_},SrvState) when SrvState#devmgrstate.dev_srv_pid =/= none andalso 
                                                                  SrvState#devmgrstate.dev_state =/= booting andalso
									     	    				  node(CommPid) =:= node() ->
							
 % Forward the configuration change command to the device's 'dev_server', waiting for its response up to a predefined timeout
 CfgChangeRes = try gen_server:call(SrvState#devmgrstate.dev_srv_pid,{dev_config_change,NewCfg},4500)
 catch
  exit:{timeout,_} ->
  
   % dev_server timeout
   {error,dev_timeout}
 end,
 
 % Depending on the result of the device configuration change
 case CfgChangeRes of
  {error, Reason} ->
   
   % In case of error ('dev_timeout' included), report it to the caller
   {reply,{error,Reason},SrvState};
   
  {ok,{UpdatedCfg,Timestamp}} ->
  
   % If the device returned the updated device configuration and timestamp, attempt to push it in the Mnesia 'device' table
   %% [NOTE]: This probably is not necessary, but it never knows
   case db:update_dev_config(SrvState#devmgrstate.dev_id,UpdatedCfg,Timestamp) of
	 
    {error,Reason} ->
       
	 % If there was an error in updating the device configuration and timestamp, return it
     {reply,{error,{mnesia,Reason}},SrvState};
		
	ok ->
	  
	 % Otherwise return the user the updated device's configuration and timestamp
	 {reply,{ok,{UpdatedCfg,Timestamp}},SrvState}
  
   end
 end;

% This clauses matches if attempting to send a 'dev_config_change' command to a device that is still booting
handle_call({dev_config_change,_},{_,_},SrvState) ->

 % Reply that the device is still booting
 {reply,{error,dev_booting},SrvState};


%% DEV_COMMAND
%% -----------
%% SENDER:    The simulation controller (the user)
%% WHEN:      -
%% PURPOSE:   Execute a command on the device's node via its 'dev_server' and return the result of the operation
%% CONTENTS:  The Module, Function and ArgsList to be evaluated by the 'dev_server' via apply()
%% MATCHES:   When the device has booted and its 'dev_server' is registered, otherwise an
%%            error message is returned (and the request comes from the JANET Simulator node)
%% ACTIONS:   Forward the command to the 'dev_server' via a synhcronous call and return its response
%% ANSWER:    The answer of the 'dev_server', consisting in the result of the specified command
%% NEW STATE: -
%%
handle_call({dev_command,Module,Function,ArgsList},{CommPid,_},SrvState) when SrvState#devmgrstate.dev_srv_pid =/= none andalso
                                                                              SrvState#devmgrstate.dev_state =/= booting andalso
																			  node(CommPid) =:= node() ->

 % Forward the command to the device 'dev_server', waiting for its response up to a predefined timeout
 Res = try gen_server:call(SrvState#devmgrstate.dev_srv_pid,{dev_command,Module,Function,ArgsList},4800)
 catch
  exit:{timeout,_} ->
  
   % dev_server timeout
   {error,dev_timeout}
 end,
 
 % Return the 'dev_server' response (or the timeout expiration)
 {reply,Res,SrvState};  
 
% This clauses matches if attempting to send a 'dev_command' to a device that is still booting
handle_call({dev_command,_,_,_},{_,_},SrvState) ->

 % Reply that the device is still booting
 {reply,{error,dev_booting},SrvState}; 
 

%% Unexpected call
handle_call(Request,From,SrvState=#devmgrstate{dev_id=Dev_id}) ->
 
 % Report that an unexpected call was received by this gen_server
 io:format("[dev_mgr-~w]: <WARNING> Unexpected call (Request = ~p, From = ~p, SrvState = ~p)~n",[Dev_id,Request,From,SrvState]),

 % Reply with a stub message and keep the SrvState
 {reply,unsupported,SrvState}.
 

%% ========================================================= HANDLE_CAST ========================================================= %% 

%% DEV_SRV_STATE_UPDATE
%% --------------------
%% SENDER:    The device's 'dev_server' process
%% WHEN:      When the 'dev_server' changes states from 'connecting' to 'online' or viceversa
%% PURPOSE:   Inform the device manager of the device state change
%% CONTENTS:  1) The PID of the device's 'dev_server' ("security purposes")
%%            2) The updated device state ('online' or 'connecting')
%% MATCHES:   After the device has booted (and the request comes from the device's 'dev_server' process)
%% ACTIONS:   Update accordingly the device's status in the 'devmanager' table
%% NEW STATE: Update accordingly the 'dev_state'
%%
handle_cast({dev_srv_state_update,online,DevSrvPid},SrvState) when SrvState#devmgrstate.dev_state =/= booting andalso DevSrvPid =:= SrvState#devmgrstate.dev_srv_pid ->

 % Update the device node state to "ONLINE" in the 'devmanager' table
 {atomic,ok} = mnesia:transaction(fun() -> mnesia:write(#devmanager{dev_id=SrvState#devmgrstate.dev_id,loc_id=SrvState#devmgrstate.loc_id,mgr_pid=self(),status="ONLINE"}) end),

 % Update the 'dev_state' to 'online'
 {noreply,SrvState#devmgrstate{dev_state = online}};

handle_cast({dev_srv_state_update,connecting,DevSrvPid},SrvState) when SrvState#devmgrstate.dev_state =/= booting andalso DevSrvPid =:= SrvState#devmgrstate.dev_srv_pid ->

 % Update the device node state to "CONNECTING" in the 'devmanager' table
 {atomic,ok} = mnesia:transaction(fun() -> mnesia:write(#devmanager{dev_id=SrvState#devmgrstate.dev_id,loc_id=SrvState#devmgrstate.loc_id,mgr_pid=self(),status="CONNECTING"}) end),

 % Update the 'dev_state' to 'connecting'
 {noreply,SrvState#devmgrstate{dev_state = connecting}};
 
 
%% DEV_CONFIG_UPDATE
%% ----------------- 
%% SENDER:    The device's 'dev_server' process
%% WHEN:      When the device's state machine configuration changes
%% PURPOSE:   Inform the device manager of the updated state machine configuration
%% CONTENTS:  1) The PID of the device's 'dev_server' ("security purposes")
%%            2) The updated configuration of the device's state machine
%%            3) The timestamp of the updated configuration
%% MATCHES:   After the device has booted (and the request comes from the device's 'dev_server' process)
%% ACTIONS:   Push the updated device configuration and timestamp in the 'device' table
%% NEW STATE: -
%%
handle_cast({dev_config_update,DevSrvPid,{UpdatedCfg,Timestamp}},SrvState) when SrvState#devmgrstate.dev_state =/= booting andalso DevSrvPid =:= SrvState#devmgrstate.dev_srv_pid ->

 % Push the updated device configuration and timestamp in the 'device' table
 db:update_dev_config(SrvState#devmgrstate.dev_id,UpdatedCfg,Timestamp),
 
 % Keep the server state
 {noreply,SrvState};


%% Unexpected cast
handle_cast(Request,SrvState=#devmgrstate{dev_id=Dev_id}) ->
 
 % Report that an unexpected cast was received by this gen_server
 io:format("[dev_mgr-~w]: <WARNING> Unexpected cast (Request = ~p, SrvState = ~w)~n",[Dev_id,Request,SrvState]),

 % Keep the SrvState
 {noreply,SrvState}. 
 
 
%% ========================================================= HANDLE_INFO ========================================================= %%  

%% DEVICE NODE DOWN
%% ----------------
%% SENDER:    The Erlang Run-Time System (ERTS)
%% WHEN:      When the monitored 'dev_server' process on the device 
%%            node (and thus the device node itself) terminates
%% PURPOSE:   Inform the manager of the device node termination
%% CONTENTS:  1) The monitor reference the notification refers to
%%            2) The PID of the process that was monitored (the 'dev_server' process)
%%            3) The reason for the monitored process's termination
%% MATCHES:   (always) (when the monitor reference and the PID of the monitored process match the ones in the server's state)
%% ACTIONS:   1) Report the Reason for the device node termination
%%            2) Update the device state in the 'devmanager' table to "BOOTING"
%%            3) Attempt to restart the the device node via the handle_continue(start_device_node,SrvState) callback
%% ANSWER:    -
%% NEW STATE: Update the server 'dev_state' to 'booting' and reset all other state variables apart from the
%%            device ID (dev_id), its location ID (loc_id) and the hostname of its controller node (ctr_hostname)
%%
handle_info({'DOWN',MonRef,process,DevSrvPid,Reason},SrvState) when MonRef =:= SrvState#devmgrstate.dev_srv_mon, DevSrvPid =:= SrvState#devmgrstate.dev_srv_pid ->

 % Report the Reason for the device node termination
 case Reason of
 
  % The 'dev_server' crashed before the monitor could be
  % established (or was never spawned in the first place)
  noproc ->
   io:format("[dev_mgr-~w]: <ERROR> The device's 'dev_server' process was not found, attempting to restart the device node~n",[SrvState#devmgrstate.dev_id]);
 
  % Connection with the controller's host was lost
  noconnection ->
   io:format("[dev_mgr-~w]: <WARNING> Lost connection with the device node's host, attempting to restart the device node~n",[SrvState#devmgrstate.dev_id]);
 
  % Unknown termination reason
  UnknownReason ->
   io:format("[dev_mgr-~w]: <WARNING> Device node terminated with unknown reason \"~w\", attempting to restart it~n",[SrvState#devmgrstate.dev_id,UnknownReason])
 end,
 
 % Explicitly stop the device node for preventing reconnection
 % attempts from the 'slave' library should it come back online
 slave:stop(SrvState#devmgrstate.dev_node),
 
 % Update the device node state to "BOOTING" in the 'devmanager' table
 {atomic,ok} = mnesia:transaction(fun() -> mnesia:write(#devmanager{dev_id=SrvState#devmgrstate.dev_id,loc_id=SrvState#devmgrstate.loc_id,mgr_pid=self(),status="BOOTING"}) end),
 
 % Update the server 'dev_state' to 'booting', reset all other state variables apart from the device ID, location ID and the hostname
 % of the controller node, and attempt to restart the controller node via the handle(start_controller_node,SrvState) callback
 {noreply,#devmgrstate{dev_state = booting, dev_id = SrvState#devmgrstate.dev_id, loc_id = SrvState#devmgrstate.loc_id,
                        ctr_hostname = SrvState#devmgrstate.ctr_hostname, _ = none},{continue,start_device_node}}.
				   

%% ========================================================== TERMINATE ========================================================== %% 

%% Called when:
%%
%% 1) The manager is asked to shutdown by its 'sup_loc' supervisor (Reason = 'shutdown')
%% 2) The managed device node crashes (Reason = CrashReason)
%% 3) The 'dev_server' process on the managed device node stops (Reason = 'device_node_stopped')
%%
terminate(_,SrvState) ->
 
 % If still active, remove the monitor towards the device's 'dev_server'
 % process, also flushing possible notifications from the message queue  
 if
  is_reference(SrvState#devmgrstate.dev_srv_mon) ->
   demonitor(SrvState#devmgrstate.dev_srv_mon,[flush]);
  true ->
   ok
 end,
 
 % Update the device node state as "STOPPED" and deregister the manager's PID from the 'devmanager' table
 {atomic,ok} = mnesia:transaction(fun() -> mnesia:write(#devmanager{dev_id=SrvState#devmgrstate.dev_id,loc_id=SrvState#devmgrstate.loc_id,mgr_pid='-',status="STOPPED"}) end),
 
 % Retrieve the value of the 'janet_stopping' environment variable
 {ok,JANETStopping} = application:get_env(janet_simulator,janet_stopping),
 
 if
 
  % If the JANET Simulator is not stopping and the device was
  % booted, print a message reporting that the its node is stopping
  JANETStopping =:= false andalso SrvState#devmgrstate.dev_state =/= booting ->
   io:format("[dev_mgr-~w]: Device node stopped~n",[SrvState#devmgrstate.dev_id]);
 
  % In all other cases, print nothing
  true ->
   ok
 end.

 %% NOTE: At this point, if is still running, being it linked to the manager the device node is automatically terminated 

 
%%====================================================================================================================================
%%                                                         START FUNCTION                                                        
%%====================================================================================================================================

%% Called by its 'sup_loc' location supervisor whenever a new device is started in the location, which may happen:
%%  - At boot time by the location devices' initializer      (locs_devs_init)
%%  - At run time when a new device is added in the location (db:add_device(Dev_id,Name,{Loc_id,Subloc_id},Type))
start_link(Dev_id,Loc_id,CtrHostName) ->
 gen_server:start_link(?MODULE,{Dev_id,Loc_id,CtrHostName},[]).
