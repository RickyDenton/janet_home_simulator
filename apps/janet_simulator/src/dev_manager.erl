%% This module represents a device's manager in the JANET Simulator application %%

-module(dev_manager).
-behaviour(gen_server).

-export([start_link/2,init/1,terminate/2,handle_call/3,handle_cast/2,handle_continue/2,handle_info/2]).  % gen_server Behaviour Callback Functions

-include("sim_mnesia_tables_definitions.hrl").  % Janet Simulator Mnesia Tables Records Definitions

%% This record represents the state of a dev_manager gen_server
-record(devmgrstate,    
        {
		 dev_state,       % The state of the managed device node
		 dev_node,        % The reference to the managed node
		 dev_srv_pid,     % The PID of the device's 'dev_server' process
		 dev_srv_mon,     % A reference used for monitoring the device's 'dev_server' process (and consequently the node)
		 dev_id,          % The device's ID
		 loc_id           % The device's location ID
		}).

%%====================================================================================================================================
%%                                                  GEN_SERVER CALLBACK FUNCTIONS                                                        
%%====================================================================================================================================

%% ============================================================ INIT ============================================================ %%
init({Dev_id,Loc_id}) ->

 % Trap exit signals so to allow cleanup operations when terminating (terminate(Reason,SrvState) callback function)
 process_flag(trap_exit,true),
 
 % Register the manager in the 'devmanager' table
 {atomic,ok} = mnesia:transaction(fun() -> mnesia:write(#devmanager{dev_id=Dev_id,loc_id=Loc_id,mgr_pid=self(),status="BOOTING"}) end),
 
 % Return the server initial state, where the initialization of the device node will continue
 % in the "handle_continue(Continue,State)" callback function for parallelization purposes  
 {ok,#devmgrstate{dev_state = booting, dev_id = Dev_id, loc_id = Loc_id, _ = none},{continue,init}}.
 
 
%% ======================================================= HANDLE_CONTINUE ======================================================= %%

%% Initializes the device's node (called right after the 'init' callback function)
handle_continue(init,SrvState) ->
 
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
 
 % Set the cookie for allowing the Janet Simulator to connect with the device's node
 % NOTE: The use of atoms is required by the erlang:set_cookie BIF  
 erlang:set_cookie(list_to_atom("dev-" ++ Dev_id_str ++ "@localhost"),list_to_atom(Loc_id_str)),
 
 % Prepare the Host, Name and Args parameters of the controller's node
 NodeHost = "localhost",
 NodeName = "dev-" ++ Dev_id_str,
 NodeArgs = "-setcookie " ++ Loc_id_str ++ " -connect_all false -pa _build/default/lib/janet_device/ebin/ _build/default/lib/janet_simulator/ebin/",
 
 % Instantiate the controller's node and link it to the manager
 {ok,Node} = slave:start_link(NodeHost,NodeName,NodeArgs),

 % Launch the Janet Device application on the controller node
 ok = rpc:call(Node,jdev,run,[Dev_id,Loc_id,self(),Type,Config]),
 
 % Set the 'dev_node' state variable and wait for the
 % registration request of the device's dev_server process
 {noreply,SrvState#devmgrstate{dev_node = Node}}.
  
 
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
%% WHEN:      When the monitored 'dev_server' process on the device node terminates
%% PURPOSE:   Inform of the 'dev_server' process termination
%% CONTENTS:  1) The monitor reference the notification refers to
%%            2) The PID of the process that was monitored (the 'dev_server' process)
%%            3) The reason for the monitored process's termination
%% MATCHES:   (always) (when the monitor reference and the PID of the monitored process match the ones in the server's state)
%% ACTIONS:   If Reason =:= 'noproc' log the event (it should not happen), and stop the device manager
%% ANSWER:    -
%% NEW STATE: Stop the server (reason = 'device_node_stopped')
%%
handle_info({'DOWN',MonRef,process,DevSrvPid,Reason},SrvState) when MonRef =:= SrvState#devmgrstate.dev_srv_mon, DevSrvPid =:= SrvState#devmgrstate.dev_srv_pid ->

 % If Reason =:= 'noproc', which is associated to the fact that the 'dev_server' never existed
 % in the first place or crashed before the monitor could be established, log the error
 if
  Reason =:= noproc ->
   io:format("[devrmgr-~w]: <WARNING> The device node's 'dev_server' process does not exist~n",[SrvState#devmgrstate.dev_id]);
  true ->
   ok
 end,
 
 % Stop the device manager (reason = 'device_node_stopped')
 {stop,device_node_stopped,SrvState}.


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
 
 % Note that if still running the device node is
 % also terminated, being it linked to its manager
 ok.
 

%%====================================================================================================================================
%%                                                         START FUNCTION                                                        
%%====================================================================================================================================

%% Called by its 'sup_loc' location supervisor whenever a new device is started in the location, which may happen:
%%  - At boot time by the location devices' initializer      (locs_devs_init)
%%  - At run time when a new device is added in the location (db:add_device(Dev_id,Name,{Loc_id,Subloc_id},Type))
start_link(Dev_id,Loc_id) ->
 gen_server:start_link(?MODULE,{Dev_id,Loc_id},[]).
