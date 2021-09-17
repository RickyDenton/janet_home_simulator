%% This module represents a location controller's manager in the JANET Simulator application %%

-module(ctr_manager).
-behaviour(gen_server).

-export([start_link/1,init/1,terminate/2,handle_call/3,handle_cast/2,handle_continue/2,handle_info/2]).  % gen_server Behaviour Callback Functions

-include("sim_mnesia_tables_definitions.hrl").  % Janet Simulator Mnesia Tables Records Definitions


%% This record represents the state of a ctr_manager gen_server
-record(ctrmgrstate,    
        {
		 ctr_state,       % The state of the managed controller node
		 ctr_node,        % The reference to the managed node
		 ctr_srv_pid,     % The PID of the controller's 'ctr_simserver' process
		 ctr_srv_mon,     % A reference used for monitoring the controller's 'ctr_simserver' process (and consequently the node)
		 loc_id           % The controller's location ID
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
 
 % For parallelizing the controller node's initialization as well as for preventing limit race conditions between its
 % creation and deletion, retrieve its record from the database and use is as a bridge state between the "init(Loc_id)" 
 % and the "handle_continue(init,LocationRecord)" callback function where the initialization will continue  
 {ok,LocationRecord} = db:get_record(location,Loc_id),
 {ok,LocationRecord,{continue,init}}.
 

%% ======================================================= HANDLE_CONTINUE ======================================================= %%
  
%% Initializes the controller's node (called right after the 'init' callback function)
handle_continue(init,LocationRecord) ->
 
 %% -------------- Controller Node Configuration Parameters Definition -------------- %%
 
 % Retrieve the Loc_id and convert it to string
 Loc_id = LocationRecord#location.loc_id,
 Loc_id_str = integer_to_list(Loc_id),
 
 % Retrieve the location port to be used as 'rest_port' by the controller
 Loc_port = LocationRecord#location.port,
 
 % Derive the initial contents of the controller's 'ctr_sublocation' and 'ctr_device' tables
 {ok,CtrSublocTable,CtrDeviceTable} = prepare_ctr_tables(Loc_id),
 
 % Retrieve the 'remotehost' environment variable
 {ok,RemoteHost} = application:get_env(remotehost),
 
 %% ---------------------------- Controller Node Creation ---------------------------- %% 
 
 % Set the cookie for allowing the Janet Simulator to connect with the controller's node
 % NOTE: The use of atoms is required by the erlang:set_cookie BIF 
 erlang:set_cookie(utils:str_to_atom("ctr-" ++ Loc_id_str ++ "@localhost"),utils:str_to_atom(Loc_id_str)),
 
 % Prepare the Host, Name and Args parameters of the controller's node
 NodeHost = "localhost",
 NodeName = "ctr-" ++ Loc_id_str,
 NodeArgs = "-setcookie " ++ Loc_id_str ++ " -connect_all false -pa ebin/",
 
 % Instantiate the controller's node and link it to the manager
 {ok,Node} = slave:start_link(NodeHost,NodeName,NodeArgs),
 
  % Derive the initial contents of the controller's 'ctr_sublocation' and 'ctr_device' tables
 {ok,CtrSublocTable,CtrDeviceTable} = prepare_ctr_tables(Loc_id),
 
 % Launch the Janet Controller application on the controller node
 ok = rpc:call(Node,jctr,run,[Loc_id,CtrSublocTable,CtrDeviceTable,self(),Loc_port,RemoteHost]),
 
 % Set the proper initial 'dev_manager' gen_server state and wait for
 % the registration request of the controller's 'ctr_simserver' process
 {noreply,#ctrmgrstate{ctr_state = booting, ctr_node = Node, loc_id = Loc_id, _ = none}}.


%% ========================================================= HANDLE_CALL ========================================================= %% 

%% CTR_REG
%% -------
%% SENDER:    The controller's ctr_simserver
%% WHEN:      During the ctr_simserver initialization (handle_continue(init,_))
%% PURPOSE:   Controller registration request
%% CONTENTS:  The PID of the ctr_simserver
%% MATCHES:   When the controller is booting (and the request comes from the spawned controller node)
%% ACTIONS:   Create a monitor towards the controller's 'ctr_simserver' process and
%%            update the controller's state to "ONLINE" in the 'ctrmanager' table
%% ANSWER:    'ok' (the controller registration was successful)
%% NEW STATE: Update the 'ctr_state' to 'online' and set the 'ctr_srv_pid' and 'ctr_srv_mon' variables
%%
%% NOTE:      This message can be received only once, since if the ctr_simserver process crashes the entire
%%            controller node is shut down by its application master (The Janet Controller application is permanent)
%%
handle_call({ctr_reg,CtrSrvPid},{CtrSrvPid,_},SrvState) when SrvState#ctrmgrstate.ctr_state =:= booting andalso node(CtrSrvPid) =:= SrvState#ctrmgrstate.ctr_node ->

 % Create a monitor towards the device's 'ctr_simserver' process
 MonRef = monitor(process,CtrSrvPid),
 
 % Update the controller node state to "ONLINE" in the 'ctrmanager' table 
 %% [TODO]: Maybe another intermediate state is required for checking if it can connect with the remote MongoDB database 
 {atomic,ok} = mnesia:transaction(fun() -> mnesia:write(#ctrmanager{loc_id=SrvState#ctrmgrstate.loc_id,mgr_pid=self(),status="ONLINE"}) end),
 
 % Log that the controller has successfully booted
 io:format("[ctr_mgr-~w]: Controller node successfully booted~n",[SrvState#ctrmgrstate.loc_id]),
 
 % Confirm the controller registration and update the 'ctr_srv_pid' and the 'ctr_state' state variables
 {reply,ok,SrvState#ctrmgrstate{ctr_state = online, ctr_srv_pid = CtrSrvPid, ctr_srv_mon = MonRef}}; 
 

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
 
 
 
 
%% DEBUGGING PURPOSES [TODO]: REMOVE
handle_call(_,{ReqPid,_},SrvState) ->
 io:format("[ctr_mgr-~w]: <WARNING> Generic response issued to ReqPid = ~w~n",[SrvState#ctrmgrstate.loc_id,ReqPid]),
 {reply,gen_response,SrvState}.


%% ===================================================== HANDLE_CAST (STUB) ===================================================== %% 

%% This represents a STUB of the handle_cast() callback function, whose
%% definition is formally required by the 'gen_server' OTP behaviour
handle_cast(Request,SrvState) ->
 
 % Report that this gen_server should not receive cast requests
 io:format("[ctr_manager-~w]: <WARNING> Unexpected cast (Request = ~w, SrvState = ~w)~n",[SrvState#ctrmgrstate.loc_id,Request,SrvState]),

 % Keep the SrvState
 {noreply,SrvState}.


%% ========================================================= HANDLE_INFO ========================================================= %%  

%% CONTROLLER NODE DOWN
%% --------------------
%% SENDER:    The Erlang Run-Time System (ERTS)
%% WHEN:      When the monitored 'ctr_simserver' process on the controller node terminates
%% PURPOSE:   Inform of the 'ctr_simserver' process termination
%% CONTENTS:  1) The monitor reference the notification refers to
%%            2) The PID of the process that was monitored (the 'ctr_simserver' process)
%%            3) The reason for the monitored process's termination
%% MATCHES:   (always) (when the monitor reference and the PID of the monitored process match the ones in the server's state)
%% ACTIONS:   If Reason =:= 'noproc' log the event (it should not happen), and stop the controller manager
%% ANSWER:    -
%% NEW STATE: Stop the server (reason = 'controller_node_stopped')
%%
handle_info({'DOWN',MonRef,process,CtrSrvPid,Reason},SrvState) when MonRef =:= SrvState#ctrmgrstate.ctr_srv_mon, CtrSrvPid =:= SrvState#ctrmgrstate.ctr_srv_pid ->

 % If Reason = 'noproc', which is associated to the fact that the 'ctr_simserver' never existed
 % in the first place or crashed before the monitor could be established, log the error
 if
  Reason =:= noproc ->
   io:format("[ctr_mgr-~w]: <WARNING> The device node's 'ctr_simserver' process does not exist~n",[SrvState#ctrmgrstate.loc_id]);
  true ->
   ok
 end,
 
 % Stop the controller manager (reason = 'controller_node_stopped')
 {stop,controller_node_stopped,SrvState}.

 
%% ========================================================== TERMINATE ========================================================== %% 

%% Called when:
%%
%% 1) The manager is asked to shutdown by its 'sup_loc' supervisor (Reason = 'shutdown')
%% 2) The managed controller node crashes (Reason = CrashReason)
%% 3) The 'ctr_simserver' process on the managed controller node stops (Reason = 'controller_node_stopped')
%%
terminate(_,SrvState) ->
 
 % Deregister the monitor to the controller's 'ctr_simserver' process, if present
 if
  is_reference(SrvState#ctrmgrstate.ctr_srv_mon) ->
   demonitor(SrvState#ctrmgrstate.ctr_srv_mon);
  true ->
   ok
 end,
 
 % Update the controller node state as "STOPPED" and deregister the manager's PID from the 'ctrmanager' table
 {atomic,ok} = mnesia:transaction(fun() -> mnesia:write(#ctrmanager{loc_id=SrvState#ctrmgrstate.loc_id,mgr_pid='-',status="STOPPED"}) end),
 
 % Note that if still running the controller node is
 % also terminated, being it linked to its manager
 ok.


%%====================================================================================================================================
%%                                                    PRIVATE HELPER FUNCTIONS
%%==================================================================================================================================== 

%% Derives the initial contents of a controller's 'ctr_sublocation' and 'ctr_device' tables (handle_continue(init,SrvState) helper function)
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