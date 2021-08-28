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
		 dev_statem_pid,  % The PID of the device's state machine
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
 
 % Check the record validity
 %% [TODO]: This is probably is not required, since if the record is corrupted the function crashes anyway (and it just checks for it to be in the form {device,...}
 % true = is_record(DeviceRecord,device),
 
 % Retrieve the device's type and configuration
 Type = DeviceRecord#device.type,
 Config = DeviceRecord#device.config,
 
 %% ------------------------------ Device Node Creation ------------------------------ %% 
 
 % Set the cookie for allowing the Janet Simulator to connect with the device's node
 % NOTE: The use of atoms is required by the erlang:set_cookie BIF  
 erlang:set_cookie(utils:str_to_atom("dev-" ++ Dev_id_str ++ "@localhost"),utils:str_to_atom(Loc_id_str)),
 
 % Prepare the Host, Name and Args parameters of the controller's node
 NodeHost = "localhost",
 NodeName = "dev-" ++ Dev_id_str,
 NodeArgs = "-setcookie " ++ Loc_id_str ++ " -connect_all false -pa ebin/",
 
 % Instantiate the controller's node and link it to the manager
 {ok,Node} = slave:start_link(NodeHost,NodeName,NodeArgs),

 % Launch the Janet Device application on the controller node
 ok = rpc:call(Node,jdev,run,[Dev_id,Loc_id,self(),Type,Config]),
 
 % Set the dev_node in the server state and wait for the
 % registration request of the device's dev_server process
 {noreply,SrvState#devmgrstate{dev_node = Node}}.
  
 
%% ========================================================= HANDLE_CALL ========================================================= %% 

%% ----------------------------------------------------------- BOOTING ----------------------------------------------------------- %%

%% SENDER:    The device's 'dev_server' process
%% WHEN:      During the 'dev_server' initialization (handle_continue(init,_))
%% PURPOSE:   Device registration request
%% CONTENTS:  The dev_server and dev_statem PIDs
%% MATCHES:   When the device is booting (and the request comes from the spawned device node)
%% ACTIONS:   Create a monitor towards the device's 'dev_server' process and
%%            update the device's state to "CONNECTING" in the 'devmanager' table
%% ANSWER:    'ok' (the device registration was successful)
%% NEW STATE: Update the device state to 'connecting and set the 'dev_srv_pid', 'dev_statem_pid' and 'dev_srv_mon' variables
%%
%% NOTE:      This message can be received only once, since if the dev_server process crashes the entire
%%            device node is shut down by its application master (The Janet Device application is permanent)
%%
handle_call({dev_reg,DevSrvPid,DevStatemPid},{DevSrvPid,_},SrvState) when SrvState#devmgrstate.dev_state =:= booting andalso node(DevSrvPid) =:= SrvState#devmgrstate.dev_node ->

 % Create a monitor towards the device's 'dev_server' process
 MonRef = monitor(process,DevSrvPid),

 % Update the device node state to "CONNECTING" in the 'devmanager' table
 {atomic,ok} = mnesia:transaction(fun() -> mnesia:write(#devmanager{dev_id=SrvState#devmgrstate.dev_id,loc_id=SrvState#devmgrstate.loc_id,mgr_pid=self(),status="CONNECTING"}) end),
 
 % Log that the device has successfully booted
 io:format("[dev_mgr-~w]: Device node successfully booted~n",[SrvState#devmgrstate.dev_id]),
 
 % Confirm the device registration and update the 'dev_srv_pid','dev_statem_pid' and the 'dev_state' state variables
 {reply,ok,SrvState#devmgrstate{dev_state = connecting, dev_srv_pid = DevSrvPid, dev_statem_pid = DevStatemPid, dev_srv_mon = MonRef}}; 


%% SENDER:    The simulation controller (the user)
%% WHEN:      -
%% PURPOSE:   Execute a command on the device's node via its 'dev_server' and return the result of the operation
%% CONTENTS:  The Module, Function and ArgsList to be evaluated by the 'dev_server' via apply()
%% MATCHES:   When the 'dev_server' is registered (and the request comes from the JANET Simulator node)
%% ACTIONS:   Forward the command to the 'dev_server' via a synhcronous call and return its response [TODO]: Rewrite using a gen_cli?
%% ANSWER:    The answer of the 'dev_server', consisting in the result of the specified command
%% NEW STATE: -
%%
handle_call({dev_command,Module,Function,ArgsList},{CommPid,_},SrvState) when SrvState#devmgrstate.dev_srv_pid =/= none andalso node(CommPid) =:= node() ->

 % Forward the command to the device 'dev_server', waiting for its response up to a predefined timeout
 Res = try gen_server:call(SrvState#devmgrstate.dev_srv_pid,{dev_command,Module,Function,ArgsList},4800)
 catch
  exit:{timeout,_} ->
  
   % dev_server timeout
   {error,dev_timeout}
 end,
 
 % Return the 'dev_server' response (or the timeout expiration)
 {reply,Res,SrvState};  
 
 
 
%% DEBUGGING PURPOSES [TODO]: REMOVE
handle_call(_,{ReqPid,_},SrvState) ->
 io:format("[dev_mgr-~w]: <WARNING> Generic response issued to ReqPid = ~w~n",[SrvState#devmgrstate.dev_id,ReqPid]),
 {reply,gen_response,SrvState}.


%% ========================================================= HANDLE_CAST ========================================================= %% 

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
 

 
%% --------- STUB
handle_cast(reset,State) -> % Resets the server State
 {noreply,State}.


%% ========================================================= HANDLE_INFO ========================================================= %%  

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
  
 % Deregister the monitor to the device's 'dev_server' process, if present
 if
  is_reference(SrvState#devmgrstate.dev_srv_mon) ->
   demonitor(SrvState#devmgrstate.dev_srv_mon);
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