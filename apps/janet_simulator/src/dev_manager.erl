%% This module represents a device's manager in the JANET Simulator application %%

-module(dev_manager).
-behaviour(gen_server).

-export([start_link/3,init/1,terminate/2,handle_call/3,handle_cast/2,handle_continue/2,handle_info/2]).  % gen_server Behaviour Callback Functions

-include("sim_mnesia_tables_definitions.hrl").  % Janet Simulator Mnesia Tables Records Definitions

%% This record represents the state of a dev_manager gen_server
-record(devmgrstate,    
        {
		 dev_state,       % The state of the managed device node
		 dev_node,        % The reference to the managed node
		 dev_srv_pid,     % The PID of the device's 'dev_server' process
		 dev_srv_mon,     % A reference used for monitoring the device's 'dev_server' process (and consequently the node)
		 dev_id,          % The device's ID
		 loc_id,          % The device's location ID
		 ctr_hostname,    % The name of the host where the location controller node is deployed
		 nodestarter_pid  % The PID of the 'node_starter' client process used by the manager for starting the device node
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

%% START_DEVICE_NODE
%% -----------------
%% WHEN:      Right after the 'init' and the 'handle_info' DEVICE NODE DOWN callbacks
%% PURPOSE:   Attempt to start the device node and link it to the manager
%% ACTIONS:   1) Retrieve the device node's configuration
%%            2) Spawn a 'node_starter' client process for attempting
%%               to start the device node and link it with the manager
%% NEW STATE: Set the 'nodestarter_pid' state variable to the PID of the 'node_starter' process
%%  
handle_continue(start_device_node,SrvState=#devmgrstate{loc_id = Loc_id, dev_id = Dev_id}) ->
 
 % Retrieve the device record
 {ok,DeviceRecord} = db:get_record(device,Dev_id),
 
 % Retrieve the name of the nodes host where the device must be deployed in
 NodeHost = utils:get_effective_hostname(DeviceRecord#device.hostname),
 
 % Define the device node name by concatenating
 % its device ID to the constant "dev-" string
 Dev_id_str = integer_to_list(Dev_id),
 NodeName = "dev-" ++ Dev_id_str,
 
 % Convert the device location ID to string
 Loc_id_str = integer_to_list(Loc_id),
  
 % Prepare the device node VM arguments as follows:
 %
 % - Set its cookie to its location ID                               ("-setcookie Loc_id")
 % - Disable transitive connections between nodes                    ("-connect_all false")
 % - Align its net_kernel ticktime to the one of the JANET Simulator ("-kernel net_ticktime 20")
 % - If the node is to be deployed on the localhost, set its         ("-env ERL_LIBS _build/default/lib")
 %   $ERL_LIBS environment variable to the "_build/default/lib"
 %   directory to allow it to find the 'janet_device' resource
 %   file and the bytecode required for its operation
 %
 %   NOTE: Remote nodes are instead supposed to have the
 %         contents of such directory placed in the Erlang
 %         installation directory (default: "/usr/lib/erlang/lib")
 %
 NodeArgs =
 case utils:is_remote_host(NodeHost) of
 
  % Localhost node
  false ->
   "-setcookie " ++ Loc_id_str ++ " -connect_all false -kernel net_ticktime 20 -env ERL_LIBS _build/default/lib/";
   
  % Remote node
  true ->
   "-setcookie " ++ Loc_id_str ++ " -connect_all false -kernel net_ticktime 20"
 end, 
 
 % Set the cookie provided and expected by the JANET Simulator to the
 % device node to the one previously defined (its location ID as an atom)
 erlang:set_cookie(list_to_atom(NodeName ++ "@" ++ NodeHost),list_to_atom(Loc_id_str)),

 % Initialize the map containing information required to start the JANET Device application with:
 %
 % - The device type          (type)
 % - The device configuration (config)
 %
 AppInfo = #{type => DeviceRecord#device.type, config => DeviceRecord#device.config}, 
 
 % Retrieve the manager PID
 MgrPid = self(),

 % Spawn the 'node_starter' client process for attempting to start and link with the device node
 {ok,NodeStarterPID} = node_starter:spawn_link(NodeHost,NodeName,NodeArgs,AppInfo,Dev_id,"device",MgrPid,"dev_mgr"),
 
 % Set the 'nodestarter_pid' state variable to the PID of the 'node_starter' process and
 % await from it the result of the operation (NODE_START_SUCCESS or NODE_START_FAILURE)
 {noreply,SrvState#devmgrstate{nodestarter_pid = NodeStarterPID}}.


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

%% NODE_START_SUCCESS
%% ------------------
%% SENDER:    The manager 'node_starter' client process
%% WHEN:      When it has successfully started and linked the device node with the manager
%% PURPOSE:   Inform the manager that the device node has been started
%% CONTENTS:  1) The complete name of the device node()
%%            2) The "AppInfo" map passed by the START_DEVICE_NODE handle_continue() function
%%            3) The PID of the 'node_starter' process ("security purposes")
%% MATCHES:   (always) (when the device is booting and the PID of the 'node_starter' process matches the one in the server state)
%% ACTIONS:   Retrieve the required information and start the JANET Device
%%            application on the node via a remote procedure call (RPC)
%% NEW STATE: Update the 'dev_node' state variable to the complete name of the device node
%%
handle_cast({node_start_success,DevNode,AppInfo,NodeStarterPID},SrvState=#devmgrstate{dev_state = booting, loc_id = Loc_id, dev_id = Dev_id,
                                                                                       ctr_hostname = CtrHostName, nodestarter_pid = NodeStarterPID}) ->
 % Retrieve the device type and configuration from the "AppInfo" map
 #{type := Type, config := Config} = AppInfo,

 % Attempt to start the JANET Device application on the device node
 ok = rpc:call(DevNode,jdev,run,[Dev_id,Loc_id,self(),Type,Config,CtrHostName]),
 
 % Update the 'dev_node' state variable to the complete name of the device node
 % and await for the registration request of the device's 'dev_server' process
 {noreply,SrvState#devmgrstate{dev_node = DevNode}};


%% NODE_START_FAILURE
%% ------------------
%% SENDER:    The manager 'node_starter' client process
%% WHEN:      When it expires the attempts for starting and linking the device node with the manager
%% PURPOSE:   Inform the manager that the device node could not been started
%% CONTENTS:  1) The Reason of the last error occured in starting the node
%%            2) The PID of the 'node_starter' process ("security purposes")
%% MATCHES:   (always) (when the device is booting and the PID of the 'node_starter' process matches the one in the server state)
%% ACTIONS:   Report the reason of the last error occured in starting the node and inform that the manager will now stop
%% NEW STATE: -
%%
handle_cast({node_start_failure,FailReason,NodeStarterPID},SrvState=#devmgrstate{dev_state = booting, dev_id = Dev_id,
                                                                                  nodestarter_pid = NodeStarterPID}) ->
 				
 % Report the reason of the last error occured in starting the node and inform that the manager will now stop
 io:format("[dev_mgr-~w]: <ERROR> Failed to start the device node (reason = ~p), the manager will now stop~n",[Dev_id,FailReason]),
   
 % Stop the manager with reason 'normal' for preventing its 'sup_loc' supervisor from attempting to respawn
 % it (which would be useless given that in the current situation its managed node cannot be started) 
 {stop,normal,SrvState};
 

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
 
 % Update the server 'dev_state' to 'booting', reset all other state variables apart from the device ID, location ID and the
 % hostname of the controller node, and attempt to restart the device node via the handle(start_device_node,SrvState) callback
 {noreply,#devmgrstate{dev_state = booting, dev_id = SrvState#devmgrstate.dev_id, loc_id = SrvState#devmgrstate.loc_id,
                        ctr_hostname = SrvState#devmgrstate.ctr_hostname, _ = none},{continue,start_device_node}};
				   

%% NODE_STARTER PROCESS TERMINATED
%% -------------------------------
%% SENDER:    The Erlang Run-Time System (ERTS)
%% WHEN:      When the linked 'node_starter' process exits
%% PURPOSE:   Propagate the 'node_starter' exit signal to its linked manager process
%% CONTENTS:  1) The PID of the process that exited (the 'node_starter' process)
%%            2) The process exit Reason
%% MATCHES:   (always) (when the PID of the process that exited matches
%%                      the 'nodestarter_pid' in the server state)
%% ACTIONS:   A) If the 'node_starter' process exited with Reason =:= "normal",
%%               do nothing (it has completed its operations)
%%            B) If the 'node_starter' process exited with Reason =/= "normal",
%%               report the error stop the manager with such Reason
%% ANSWER:    -
%% NEW STATE: A) Clear the 'nodestarter_pid' variable
%%            B) -
%%
% Exit Reason =:= normal => clear the 'nodestarter_pid' variable
handle_info({'EXIT',NodeStarterPID,normal},SrvState=#devmgrstate{nodestarter_pid = NodeStarterPID}) ->
 {noreply,SrvState#devmgrstate{nodestarter_pid = none}};
 
% Exit Reason =/= normal => Report the error and stop the manager with such Reason
handle_info({'EXIT',NodeStarterPID,NodeStarterError},SrvState=#devmgrstate{nodestarter_pid = NodeStarterPID}) ->
 {stop,{node_starter_terminated,NodeStarterError},SrvState}.
 
 
%% ========================================================== TERMINATE ========================================================== %% 

%% Called when:
%%
%% 1) The manager is asked to shutdown by its 'sup_loc' supervisor (Reason = 'shutdown')
%% 2) The managed device node crashes (Reason = CrashReason)
%% 3) The 'dev_server' process on the managed device node stops (Reason = 'device_node_stopped')
%% 4) The 'node_starter' process failed to start the device node
%% 5) The 'node_starter' process exited with Reason =/= 'normal
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
