%% This module represents a location controller's manager in the Janet Simulator application %%

-module(ctr_manager).
-behaviour(gen_server).

-export([start_link/1,init/1,terminate/2,handle_call/3,handle_cast/2,handle_continue/2]).  % gen_server Behaviour Callback Functions

-include("sim_mnesia_tables_definitions.hrl").  % Janet Simulator Mnesia Tables Records Definitions


%% This record represents the state of a ctr_manager gen_server
-record(ctrmgrstate,    
        {
		 ctr_state,       % The state of the managed controller node
		 ctr_node,        % The reference to the managed node
		 ctr_srv_pid,     % The PID of the device's ctr_simserver
		 loc_id           % The controller's location ID
		}).
		
%%====================================================================================================================================
%%                                                  GEN_SERVER CALLBACK FUNCTIONS                                                        
%%====================================================================================================================================

%% ============================================================ INIT ============================================================ %%
init(Loc_id) ->

 % Trap exit signals so to allow cleanup operation at shutdown in the terminate(shutdown,State) callback function
 process_flag(trap_exit,true),
 
 % Register the manager in the 'ctrmanager' table
 {atomic,ok} = mnesia:transaction(fun() -> mnesia:write(#ctrmanager{loc_id=Loc_id,sup_pid=self(),status="BOOTING"}) end),
 
 % Return the server initial state, where the initialization of the controller node will continue
 % in the "handle_continue(Continue,State)" callback function for parallelization purposes 
 {ok,#ctrmgrstate{ctr_state=booting,ctr_node=none,ctr_srv_pid=none,loc_id=Loc_id},{continue,init}}.
 

%% ======================================================= HANDLE_CONTINUE ======================================================= %%
  
%% Initializes the controller's node (called right after the 'init' callback function)
handle_continue(init,SrvState) ->
 
 %% -------------- Controller Node Configuration Parameters Definition -------------- %%
 
 % Retrieve the Loc_id and convert it to string
 Loc_id = SrvState#ctrmgrstate.loc_id,
 Loc_id_str = integer_to_list(Loc_id),
 
 % Retrieve the location record
 {ok,LocationRecord} = db:get_record(location,Loc_id),
 
 % Check the record validity
 %% [TODO]: This is probably is not required, since if the record is corrupted the function crashes anyway (and it just checks for it to be in the form {location,...}
 % true = is_record(LocationRecord,location),
 
 % Retrieve the location port to be used as 'rest_port' by the controller
 Loc_port = LocationRecord#location.port,
 
 % Retrieve the records of the sublocations in the location and use them for preparing the controller's 'devalloc' table
 {atomic,SublocationRecords} = mnesia:transaction(fun() -> mnesia:match_object(#sublocation{sub_id = {Loc_id,'_'}, _ = '_'}) end),
 DevAlloc = derive_devalloc(SublocationRecords,[]),
 
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
 
 % Launch the Janet Controller application on the controller node
 ok = rpc:call(Node,jctr,run,[Loc_id,DevAlloc,self(),Loc_port,RemoteHost]),
 
 % Set the ctr_node in the server state and wait for the
 % registration request of the controller's 'ctr_simserver' process
 {noreply,SrvState#ctrmgrstate{ctr_node = Node}}.


%% ========================================================== TERMINATE ========================================================== %% 

%% Called when the manager is asked to shutdown by its 'sup_loc' supervisor
terminate(shutdown,SrvState) ->
 
 % Update the controller node state as "STOPPED" and deregister the manager's PID from the 'ctrmanager' table
 {atomic,ok} = mnesia:transaction(fun() -> mnesia:write(#ctrmanager{loc_id=SrvState#ctrmgrstate.loc_id,sup_pid='-',status="STOPPED"}) end),
 
 % Note that since they are linked the termination of the controller manager
 % also causes the controller node to terminate with reason 'shutdown'
 ok.


%% ========================================================= HANDLE_CALL ========================================================= %% 

%% ----------------------------------------------------------- BOOTING ----------------------------------------------------------- %%

%% SENDER:    The controller's ctr_simserver
%% WHEN:      During the ctr_simserver initialization (handle_continue(init,_))
%% PURPOSE:   Controller registration request
%% CONTENTS:  The PID of the ctr_simserver
%% MATCHES:   When the controller is booting (and the requests comes from the spawned controller node)
%% ACTIONS:   Update the controller's state to "ONLINE" in the 'ctrmanager' table
%% ANSWER:    'ok' (the controller registration was successful)
%% NEW STATE: Set the 'ctr_srv_pid' and update the 'ctr_state' to 'online"
%%
%% NOTE:      This message can be received only once, since if the ctr_simserver process crashes the entire
%%            controller node is shut down by its application master (The Janet Controller application is permanent)
%%
handle_call({ctr_reg,CtrSrvPid},{CtrSrvPid,_},SrvState) when SrvState#ctrmgrstate.ctr_state =:= booting andalso node(CtrSrvPid) =:= SrvState#ctrmgrstate.ctr_node ->

 % Update the controller node state to "ONLINE" in the 'ctrmanager' table 
 %% [TODO]: Maybe another intermediate state is required for checking if it can connect with the remote MongoDB database 
 {atomic,ok} = mnesia:transaction(fun() -> mnesia:write(#ctrmanager{loc_id=SrvState#ctrmgrstate.loc_id,sup_pid=self(),status="ONLINE"}) end),
 
 % Log that the controller has successfully booted
 io:format("[ctrmgr-" ++ integer_to_list(SrvState#ctrmgrstate.loc_id) ++ "]: Controller node successfully booted~n"),
 
 % Confirm the controller registration and update the 'ctr_srv_pid' and the 'ctr_state' state variables
 {reply,ok,SrvState#ctrmgrstate{ctr_state = online, ctr_srv_pid = CtrSrvPid}}; 
 
%% --------- STUB
handle_call(Num,_,{Sum,N}) when is_number(Num) ->
 New_Sum = Sum + Num,
 New_N = N+1,
 {reply,New_Sum/New_N,{New_Sum,New_N}};
 
 
%% --------- STUB
handle_call(_,_,_) ->
 timer:sleep(4000),
 {reply,ok,ok}.


%% ========================================================= HANDLE_CAST ========================================================= %% 


%% --------- STUB
handle_cast(reset,State) -> % Resets the server State
 {noreply,State}.


%%====================================================================================================================================
%%                                                    PRIVATE HELPER FUNCTIONS
%%==================================================================================================================================== 

%% Derives the controller 'devalloc' table from the records of the sublocations in the location (handle_continue(init,{booting,none,Loc_id}) helper function)
derive_devalloc([],DevAlloc) ->

 % Return the final 'devalloc' table
 DevAlloc;
derive_devalloc([Subloc|NextSubloc],DevAlloc) ->

 % Retrieve the subloc_id associated with the sublocation
 {_,Subloc_id} = Subloc#sublocation.sub_id,
 
 % Append the subloc_id and the list of devices in the location to the DevAlloc
 NextDevAlloc = DevAlloc ++ [{Subloc_id,Subloc#sublocation.devlist}],
 
 % Parse the next sublocation record
 derive_devalloc(NextSubloc,NextDevAlloc).


%%====================================================================================================================================
%%                                                         START FUNCTION                                                        
%%====================================================================================================================================

%% Called by its 'sup_loc' location supervisor whenever a new location management tree is created, which may happen:
%%  - At boot time by the locations' tree boot initializer     (locs_init:spawn_sup_loc([Loc_id|NextLoc_Id]))
%%  - At run time when a new location is added to the database (db:add_location(Loc_id,Name,User,Port))
start_link(Loc_id) ->
 gen_server:start_link(?MODULE,Loc_id,[]).