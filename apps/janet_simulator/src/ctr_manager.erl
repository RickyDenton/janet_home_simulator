%% This module represents a location controller's manager in the JANET Simulator application %%

-module(ctr_manager).
-behaviour(gen_server).

-export([start_link/1,init/1,terminate/2,handle_call/3,handle_cast/2,handle_continue/2,handle_info/2]).  % gen_server Behaviour Callback Functions

-include("sim_mnesia_tables_definitions.hrl").  % Janet Simulator Mnesia Tables Records Definitions

%% This record represents the state of a ctr_manager gen_server
-record(ctrmgrstate,    
        {
		 ctr_state,       % The state of the managed controller node ('booting' | 'connecting' | 'online')
		 ctr_node,        % The reference to the managed node
		 ctr_srv_pid,     % The PID of the controller's 'ctr_simserver' process
		 ctr_srv_mon,     % A reference used for monitoring the controller's 'ctr_simserver' process (and consequently the node)
		 loc_id,          % The controller's location ID
		 nodestarter_pid  % The PID of the 'node_starter' client process used by the manager for starting the controller node
		}).

%%====================================================================================================================================
%%                                                  GEN_SERVER CALLBACK FUNCTIONS                                                        
%%====================================================================================================================================

%% ============================================================ INIT ============================================================ %%
init(Loc_id) ->

 % Trap exit signals so to allow cleanup operations when terminating (terminate(Reason,SrvState) callback function)
 process_flag(trap_exit,true),
 
 % Register the manager in the 'ctrmanager' table
 {atomic,ok} = mnesia:transaction(fun() -> mnesia:write(#ctrmanager{loc_id=Loc_id,mgr_pid=self(),status="BOOTING"}) end),
 
 % Return the server initial state, where the initialization of the controller node will continue in
 % the "handle_continue(start_controller_node,State)" callback function for parallelization purposes 
 {ok,#ctrmgrstate{ctr_state = booting, loc_id = Loc_id, _ = none},{continue,start_controller_node}}.
 

%% ======================================================= HANDLE_CONTINUE ======================================================= %%
 
%% START_CONTROLLER_NODE
%% ---------------------
%% WHEN:      Right after the 'init' and the 'handle_info' CONTROLLER NODE DOWN callbacks
%% PURPOSE:   Attempt to start the controller node and link it to the manager
%% ACTIONS:   1) Retrieve the controller node's configuration
%%            2) Spawn a 'node_starter' client process for attempting to
%%               start the controller node and link it with the manager
%% NEW STATE: Set the 'nodestarter_pid' state variable to the PID of the 'node_starter' process
%%  
handle_continue(start_controller_node,SrvState=#ctrmgrstate{loc_id=Loc_id}) ->
 
 % Retrieve the controller location record
 {ok,LocationRecord} = db:get_record(location,Loc_id),
 
 % Retrieve the name of the nodes host where the controller must be deployed in
 NodeHost = utils:get_effective_hostname(LocationRecord#location.hostname),
 
 % Define the controller node name by concatenating
 % its location ID to the constant "loc-" string
 Loc_id_str = integer_to_list(Loc_id),
 NodeName = "ctr-" ++ Loc_id_str,
 
 % Prepare the controller node VM arguments as follows:
 %
 % - Set its cookie to its location ID                               ("-setcookie Loc_id")
 % - Disable transitive connections between nodes                    ("-connect_all false")
 % - Align its net_kernel ticktime to the one of the JANET Simulator ("-kernel net_ticktime 20")
 % - If the node is to be deployed on the localhost, set its         ("-env ERL_LIBS _build/default/lib")
 %   $ERL_LIBS environment variable to the "_build/default/lib"
 %   directory to allow it to find the 'janet_controller' resource
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
 % controller node to the one previously defined (its location ID as an atom)
 erlang:set_cookie(list_to_atom(NodeName ++ "@" ++ NodeHost),list_to_atom(Loc_id_str)),
 
 % Initialize the map containing information required to start the JANET Controller application with:
 %
 % - The port to be used by the controller's REST server (ctr_rest_port)
 % - The user the location belongs to                    (loc_user)      [REMOTE SERVER COMPATIBILITY]
 %
 AppInfo = #{ctr_rest_port => LocationRecord#location.port, loc_user => LocationRecord#location.user},
 
 % Retrieve the manager PID
 MgrPid = self(),

 % Spawn the 'node_starter' client process for attempting to start and link with the controller node
 {ok,NodeStarterPID} = node_starter:spawn_link(NodeHost,NodeName,NodeArgs,AppInfo,Loc_id,"controller",MgrPid,"ctr_mgr"),
 
 % Set the 'nodestarter_pid' state variable to the PID of the 'node_starter' process and
 % await from it the result of the operation (NODE_START_SUCCESS or NODE_START_FAILURE)
 {noreply,SrvState#ctrmgrstate{nodestarter_pid = NodeStarterPID}}.
 

%% ========================================================= HANDLE_CALL ========================================================= %% 

%% CTR_REG
%% -------
%% SENDER:    The controller's ctr_simserver
%% WHEN:      During the ctr_simserver initialization (handle_continue(init,_))
%% PURPOSE:   Controller registration request
%% CONTENTS:  The PID of the ctr_simserver
%% MATCHES:   When the controller is booting (and the request comes from the spawned controller node)
%% ACTIONS:   Create a monitor towards the controller's 'ctr_simserver' process and
%%            update the controller's state to "CONNECTING" in the 'ctrmanager' table
%% ANSWER:    'ok' (the controller registration was successful)
%% NEW STATE: Update the 'ctr_state' to 'connecting' and set the 'ctr_srv_pid' and 'ctr_srv_mon' variables
%%
%% NOTE:      This message can be received only once, since if the ctr_simserver process crashes the entire
%%            controller node is shut down by its application master (The Janet Controller application is permanent)
%%
handle_call({ctr_reg,CtrSrvPid},{CtrSrvPid,_},SrvState) when SrvState#ctrmgrstate.ctr_state =:= booting andalso node(CtrSrvPid) =:= SrvState#ctrmgrstate.ctr_node ->

 % Create a monitor towards the device's 'ctr_simserver' process
 MonRef = monitor(process,CtrSrvPid),
 
 % Update the controller node state to "CONNECTING" in the 'ctrmanager' table 
 {atomic,ok} = mnesia:transaction(fun() -> mnesia:write(#ctrmanager{loc_id=SrvState#ctrmgrstate.loc_id,mgr_pid=self(),status="CONNECTING"}) end),
 
 % Log that the controller has successfully booted
 io:format("[ctr_mgr-~w]: Controller node successfully booted~n",[SrvState#ctrmgrstate.loc_id]),
 
 % Confirm the controller registration and update the 'ctr_srv_pid' and the 'ctr_state' state variables
 {reply,ok,SrvState#ctrmgrstate{ctr_state = connecting, ctr_srv_pid = CtrSrvPid, ctr_srv_mon = MonRef}}; 
 

%% CTR_COMMAND
%% -----------
%% SENDER:    The simulation controller (the user)
%% WHEN:      -
%% PURPOSE:   Execute a command on the controller's node via its 'ctr_simserver' and return the result of the operation
%% CONTENTS:  The Module, Function and ArgsList to be evaluated by the 'ctr_simserver' via apply()
%% MATCHES:   When the controller has booted and its 'ctr_simserver' is registered, otherwise an
%%            error message is returned (and the request comes from the JANET Simulator node)
%% ACTIONS:   Forward the command to the 'ctr_simserver' via a synhcronous call and return its response
%% ANSWER:    The answer of the 'ctr_simserver', consisting in the result of the specified command
%% NEW STATE: -
%%
handle_call({ctr_command,Module,Function,ArgsList},{CommPid,_},SrvState) when SrvState#ctrmgrstate.ctr_srv_pid =/= none andalso
                                                                              SrvState#ctrmgrstate.ctr_state =/= booting andalso
																			  node(CommPid) =:= node() ->

 % Forward the command to the controller 'ctr_simserver', waiting for its response up to a predefined timeout
 Res = try gen_server:call(SrvState#ctrmgrstate.ctr_srv_pid,{ctr_command,Module,Function,ArgsList},4800)
 catch
  exit:{timeout,_} ->
  
   % ctr_simserver timeout
   {error,ctr_timeout}
 end,
 
 % Return the 'ctr_simserver' response (or the timeout expiration)
 {reply,Res,SrvState};  
 
% This clauses matches if attempting to send a 'ctr_command' to a controller that is still booting
handle_call({ctr_command,_,_,_},{_,_},SrvState) ->

 % Reply that the controller is still booting
 {reply,{error,ctr_booting},SrvState};  
 

%% SIM_COMMAND
%% -----------
%% SENDER:    The associated controller node's simulation server (ctr_simserver)
%% WHEN:      Following a command received via the REST interface
%% PURPOSE:   Execute a command on the simulator node and return the result of the operation
%% CONTENTS:  The Module, Function and ArgsList to be evaluated via apply()
%% MATCHES:   When the 'ctr_simserver' is registered (and the request comes from the JANET Controller)
%% ACTIONS:   Execute the required command via apply()
%% ANSWER:    The result of the apply() function
%% NEW STATE: -
%%
handle_call({sim_command,Module,Function,ArgsList},{ReqPid,_},SrvState) when SrvState#ctrmgrstate.ctr_srv_pid =/= none andalso
                                                                             node(ReqPid) =:= node(SrvState#ctrmgrstate.ctr_srv_pid) ->
 % Execute the required command and return its result
 {reply,apply(Module,Function,ArgsList),SrvState};
 
 
%% Unexpected call
handle_call(Request,From,SrvState=#ctrmgrstate{loc_id=Loc_id}) ->
 
 % Report that an unexpected call was received by this gen_server
 io:format("[ctr_mgr-~w]: <WARNING> Unexpected call (Request = ~p, From = ~p, SrvState = ~p)~n",[Loc_id,Request,From,SrvState]),

 % Reply with a stub message and keep the SrvState
 {reply,unsupported,SrvState}.
 
 
%% ========================================================= HANDLE_CAST ========================================================= %% 

%% NODE_START_SUCCESS
%% ------------------
%% SENDER:    The manager 'node_starter' client process
%% WHEN:      When it has successfully started and linked the controller node with the manager
%% PURPOSE:   Inform the manager that the controller node has been started
%% CONTENTS:  1) The complete name of the controller node()
%%            2) The "AppInfo" map passed by the START_CONTROLLER_NODE handle_continue() function
%%            3) The PID of the 'node_starter' process ("security purposes")
%% MATCHES:   (always) (when the controller is booting and the PID of the 'node_starter' process matches the one in the server state)
%% ACTIONS:   Retrieve the required information and start the JANET Controller
%%            application on the node via a remote procedure call (RPC)
%% NEW STATE: Update the 'ctr_node' state variable to the complete name of the controller node
%%
handle_cast({node_start_success,CtrNode,AppInfo,NodeStarterPID},SrvState=#ctrmgrstate{ctr_state = booting, loc_id = Loc_id,
                                                                                       nodestarter_pid = NodeStarterPID}) ->
 % Retrieve the port to be used by the controller REST server
 % and the user the location belongs to from the "AppInfo" map
 #{ctr_rest_port := CtrRESTPort, loc_user := Loc_user} = AppInfo,
   
 % Derive the initial contents of the controller
 % 'ctr_sublocation' and 'ctr_device' Mnesia tables
 {ok,CtrSublocTable,CtrDeviceTable} = prepare_ctr_tables(Loc_id),
 
 % Retrieve the remote REST server address and port
 {ok,RemoteRESTServerAddr} = application:get_env(remote_rest_server_addr),
 {ok,RemoteRESTServerPort} = application:get_env(remote_rest_server_port),

 % Attempt to start the JANET Controller application on the controller node
 ok = rpc:call(CtrNode,jctr,run,[Loc_id,CtrSublocTable,CtrDeviceTable,self(),CtrRESTPort,RemoteRESTServerAddr,RemoteRESTServerPort,Loc_user]),
 
 % Update the 'ctr_node' state variable to the complete name of the controller node
 % and await for the registration request of the controller's 'ctr_simserver' process
 {noreply,SrvState#ctrmgrstate{ctr_node = CtrNode}};


%% NODE_START_FAILURE
%% ------------------
%% SENDER:    The manager 'node_starter' client process
%% WHEN:      When it expires the attempts for starting and linking the controller node with the manager
%% PURPOSE:   Inform the manager that the controller node could not been started
%% CONTENTS:  1) The Reason of the last error occured in starting the node
%%            2) The PID of the 'node_starter' process ("security purposes")
%% MATCHES:   (always) (when the controller is booting and the PID of the 'node_starter' process matches the one in the server state)
%% ACTIONS:   Report the reason of the last error occured in starting the node and inform that the manager will now stop
%% NEW STATE: -
%%
handle_cast({node_start_failure,FailReason,NodeStarterPID},SrvState=#ctrmgrstate{ctr_state = booting, loc_id = Loc_id,
                                                                                  nodestarter_pid = NodeStarterPID}) ->
 				
 % Report the reason of the last error occured in starting the node and inform that the manager will now stop
 io:format("[ctr_mgr-~w]: <ERROR> Failed to start the controller node (reason = ~p), the manager will now stop~n",[Loc_id,FailReason]),
   
 % Stop the manager with reason 'normal' for preventing its 'sup_loc' supervisor from attempting to respawn
 % it (which would be useless given that in the current situation its managed node cannot be started) 
 {stop,normal,SrvState};


%% CTR_CONN_UPDATE
%% ---------------
%% SENDER:    The controller simulation server ('ctr_simserver') on behalf of the controller HTTP client ('ctr_httpclient')
%% WHEN:      When the connection state of the controller node towards the remote REST server changes
%% PURPOSE:   Update the controller connection state in the 'ctrmanager' table
%% CONTENTS:  1) The updated controller connection state ('connecting'|'online')
%%            2) The PID of the 'ctr_simserver' process ("security purposes")
%% MATCHES:   - (when the request comes from controller simulation server)
%% ACTIONS:   Update accordingly the controller connection state in the 'ctrmanager' table
%% NEW STATE: Update the controller state to the passed connection state
%%

% CONNECTING (connection with the remote REST server was lost)
handle_cast({ctr_conn_update,connecting,CtrSimSrvPid},SrvState=#ctrmgrstate{ctr_state=online,ctr_srv_pid=CtrSimSrvPid}) ->
																			 
 % Update the controller node state to "CONNECTING" in the 'ctrmanager' table 
 {atomic,ok} = mnesia:transaction(fun() -> mnesia:write(#ctrmanager{loc_id=SrvState#ctrmgrstate.loc_id,mgr_pid=self(),status="CONNECTING"}) end),													

 % Update the controller state variable to 'connecting'
 {noreply,SrvState#ctrmgrstate{ctr_state = connecting}};

% ONLINE (connection with the remote REST server has been established)
handle_cast({ctr_conn_update,online,CtrSimSrvPid},SrvState=#ctrmgrstate{ctr_state=connecting,ctr_srv_pid=CtrSimSrvPid}) ->
																			 
 % Update the controller node state to "ONLINE" in the 'ctrmanager' table 
 {atomic,ok} = mnesia:transaction(fun() -> mnesia:write(#ctrmanager{loc_id=SrvState#ctrmgrstate.loc_id,mgr_pid=self(),status="ONLINE"}) end),													

 % Update the controller state variable to 'online'
 {noreply,SrvState#ctrmgrstate{ctr_state = online}};


%% Unexpected cast
handle_cast(Request,SrvState=#ctrmgrstate{loc_id=Loc_id}) ->
 
 % Report that an unexpected cast was received by this gen_server
 io:format("[ctr_mgr-~w]: <WARNING> Unexpected cast (Request = ~p, SrvState = ~w)~n",[Loc_id,Request,SrvState]),

 % Keep the SrvState
 {noreply,SrvState}. 


%% ========================================================= HANDLE_INFO ========================================================= %%  

%% CONTROLLER NODE DOWN
%% --------------------
%% SENDER:    The Erlang Run-Time System (ERTS)
%% WHEN:      When the monitored 'ctr_simserver' process on the controller
%%            node (and thus the controller node itself) terminates
%% PURPOSE:   Inform the manager of the controller node termination
%% CONTENTS:  1) The monitor reference the notification refers to
%%            2) The PID of the process that was monitored (the 'ctr_simserver' process)
%%            3) The reason for the monitored process's termination
%% MATCHES:   (always) (when the monitor reference and the PID of the monitored process match the ones in the server's state)
%% ACTIONS:   1) Report the Reason for the controller node termination
%%            2) Update the controller state in the 'ctrmanager' table to "BOOTING"
%%            3) Attempt to restart the the controller node via the handle_continue(start_controller_node,SrvState) callback
%% ANSWER:    -
%% NEW STATE: Update the server 'ctr_state to 'booting' and reset all other state variables apart from the location ID (loc_id)
%%
handle_info({'DOWN',MonRef,process,CtrSrvPid,Reason},SrvState) when MonRef =:= SrvState#ctrmgrstate.ctr_srv_mon, CtrSrvPid =:= SrvState#ctrmgrstate.ctr_srv_pid ->

 % Report the Reason for the controller node termination
 case Reason of
 
  % The 'ctr_simserver' crashed before the monitor could be
  % established (or was never spawned in the first place)
  noproc ->
   io:format("[ctr_mgr-~w]: <ERROR> The controller's 'ctr_simserver' process was not found, attempting to restart the controller node~n",[SrvState#ctrmgrstate.loc_id]);
 
  % Connection with the controller's host was lost
  noconnection ->
   io:format("[ctr_mgr-~w]: <WARNING> Lost connection with the controller node's host, attempting to restart the controller node~n",[SrvState#ctrmgrstate.loc_id]);
 
  % Unknown termination reason
  UnknownReason ->
   io:format("[ctr_mgr-~w]: <WARNING> Controller node terminated with unknown reason \"~w\", attempting to restart it~n",[SrvState#ctrmgrstate.loc_id,UnknownReason])
 end,
 
 % Explicitly stop the controller node for preventing reconnection
 % attempts from the 'slave' library should it come back online
 slave:stop(SrvState#ctrmgrstate.ctr_node),
 
 % Update the controller node state to "BOOTING" in the 'ctrmanager' table 
 {atomic,ok} = mnesia:transaction(fun() -> mnesia:write(#ctrmanager{loc_id=SrvState#ctrmgrstate.loc_id,mgr_pid=self(),status="BOOTING"}) end),	
 
 % Update the server 'ctr_state' to 'booting', reset all other state variables apart from the location ID
 % and attempt to restart the controller node via the handle(start_controller_node,SrvState) callback
 {noreply,#ctrmgrstate{ctr_state = booting, loc_id = SrvState#ctrmgrstate.loc_id, _ = none},{continue,start_controller_node}};


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
handle_info({'EXIT',NodeStarterPID,normal},SrvState=#ctrmgrstate{nodestarter_pid = NodeStarterPID}) ->
 {noreply,SrvState#ctrmgrstate{nodestarter_pid = none}};
 
% Exit Reason =/= normal => Report the error and stop the manager with such Reason
handle_info({'EXIT',NodeStarterPID,NodeStarterError},SrvState=#ctrmgrstate{nodestarter_pid = NodeStarterPID}) ->
 {stop,{node_starter_terminated,NodeStarterError},SrvState}.
 

%% ========================================================== TERMINATE ========================================================== %% 

%% Called when:
%%
%% 1) The manager is asked to shutdown by its 'sup_loc' supervisor (Reason = 'shutdown')
%% 2) The managed controller node crashes (Reason = CrashReason)
%% 3) The 'ctr_simserver' process on the managed controller node stops (Reason = 'controller_node_stopped')
%% 4) The 'node_starter' process failed to start the controller node
%% 5) The 'node_starter' process exited with Reason =/= 'normal
%%
terminate(_,SrvState) ->
 
 % If still active, remove the monitor towards the controller's 'ctr_simserver'
 % process, also flushing possible notifications from the message queue  
 if
  is_reference(SrvState#ctrmgrstate.ctr_srv_mon) ->
   demonitor(SrvState#ctrmgrstate.ctr_srv_mon,[flush]);
  true ->
   ok
 end,
 
 % Update the controller node state as "STOPPED" and deregister the manager's PID from the 'ctrmanager' table
 {atomic,ok} = mnesia:transaction(fun() -> mnesia:write(#ctrmanager{loc_id=SrvState#ctrmgrstate.loc_id,mgr_pid='-',status="STOPPED"}) end),
 
 % Retrieve the value of the 'janet_stopping' environment variable
 {ok,JANETStopping} = application:get_env(janet_simulator,janet_stopping),
 
 if
 
  % If the JANET Simulator is not stopping and the controller was
  % booted, print a message reporting that its node is stopping
  JANETStopping =:= false andalso SrvState#ctrmgrstate.ctr_state =/= booting ->
   io:format("[ctr_mgr-~w]: Controller node stopped~n",[SrvState#ctrmgrstate.loc_id]);
 
  % In all other cases, do not report anything
  true -> 
   ok
 end.

 %% NOTE: At this point, if is still running, being it linked to the manager the controller node is automatically terminated 


%%====================================================================================================================================
%%                                                    PRIVATE HELPER FUNCTIONS
%%==================================================================================================================================== 

%% Derives the initial contents of a controller's 'ctr_sublocation' and 'ctr_device' tables (NODE_START_SUCCESS helper function)
prepare_ctr_tables(Loc_id) ->
 F = fun() ->
 
      % Retrieve the records of the location's sublocations  
	  LocSublocsRecords = mnesia:match_object(#sublocation{sub_id = {Loc_id,'_'}, _ = '_'}),
	  
	  % Retrieve the records of the location's devices
      LocDevicesRecords = mnesia:match_object(#device{sub_id = {Loc_id,'_'}, _ = '_'}),
	  
	  % Return the two lists of records
	  {LocSublocsRecords,LocDevicesRecords}
     end,
	 
 % Retrieve the records of all sublocations and devices in the location	 
 {atomic,{LocSublocsRecords,LocDevicesRecords}} = mnesia:transaction(F),
 
 % Derive the initial contents of the controller's 'ctr_sublocation' table {subloc_id,devlist}
 CtrSublocTable = [ {element(2,Subloc#sublocation.sub_id),Subloc#sublocation.devlist} || Subloc <- LocSublocsRecords ],
 
 % Derive the initial non-null contents of the controller's 'ctr_device' table {dev_id,subloc_id,type,-,-,-}
 CtrDeviceTable = [ {Dev#device.dev_id,element(2,Dev#device.sub_id),Dev#device.type} || Dev <- LocDevicesRecords ],
 
 % Return the initial contents of both tables
 {ok,CtrSublocTable,CtrDeviceTable}.


%%====================================================================================================================================
%%                                                         START FUNCTION                                                        
%%====================================================================================================================================

%% Called by its 'sup_loc' location supervisor whenever a new location management tree is created, which may happen:
%%  - At boot time by the locations' tree boot initializer     (locs_init:spawn_sup_loc([Loc_id|NextLoc_Id]))
%%  - At run time when a new location is added to the database (db:add_location(Loc_id,Name,User,Port))
start_link(Loc_id) ->
 gen_server:start_link(?MODULE,Loc_id,[]).