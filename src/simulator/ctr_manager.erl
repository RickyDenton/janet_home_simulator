%% This module represents a location controller's manager in the Janet Simulator application %%

-module(ctr_manager).
-behaviour(gen_server).

-export([start_link/1,init/1,terminate/2,handle_call/3,handle_cast/2,handle_continue/2]).  % gen_server Behaviour Callback Functions

-include("table_records.hrl").  % Mnesia Table Records Definitions

%%====================================================================================================================================
%%                                                  GEN_SERVER CALLBACK FUNCTIONS                                                        
%%====================================================================================================================================

%% ============================================================ INIT ============================================================ %%
init(Loc_id) ->

 % Trap exit signals so to allow cleanup operation at shutdown in the terminate(shutdown,State) callback function
 process_flag(trap_exit,true),
 
 % Register the manager in the 'ctrmanager' table
 {atomic,ok} = mnesia:transaction(fun() -> mnesia:write(#ctrmanager{loc_id=Loc_id,sup_pid=self(),status="BOOTING"}) end),
 
 % The controller node's inizialization will continue in the handle_continue(Continue,State) callback function for parallelization purposes
 {ok,{booting,none,Loc_id},{continue,init}}.
 

%% ======================================================= HANDLE_CONTINUE ======================================================= %%
  
%% Initializes the controller's node (called after the 'init' callback function)
handle_continue(init,{booting,none,Loc_id}) ->
 
 %% -------------- Controller Node Configuration Parameters Definition -------------- %%
 
 % Retrieve the location record
 {ok,LocationRecord} = db:get_record(location,Loc_id),
 
 % Check the record validity
 %% [NOTE]: This is probably is not required, since if the record is corrupted the function crashes anyway (and it just checks for it to be in the form {location,...}
 % true = is_record(LocationRecord,location),
 
 % Convert the Loc_id to string
 Loc_id_str = integer_to_list(Loc_id),
 
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
 
 % Update the controller status in the 'ctrmanager' table to 'ONLINE' 
 %% [TODO]: Maybe another intermediate state is required for checking if it can connect with the remote MongoDB database 
 {atomic,ok} = mnesia:transaction(fun() -> mnesia:write(#ctrmanager{loc_id=Loc_id,sup_pid=self(),status="ONLINE"}) end),
 
 % Return the updated manager's state
 {noreply,{online,Node,Loc_id}}.


%% ========================================================== TERMINATE ========================================================== %% 

%% Called when the manager is asked to shutdown by its 'sup_loc' supervisor
terminate(shutdown,{_,_,Loc_id}) ->
 
 % Deregister the manager from the 'ctrmanager' table
 {atomic,ok} = mnesia:transaction(fun() -> mnesia:write(#ctrmanager{loc_id=Loc_id,sup_pid='-',status="STOPPED"}) end),
 
 % Return (note that the controller's node will be automatically terminated since,
 % being it linked with the manager, will receive an exit signal with reason 'shutdown')
 ok.


%% ========================================================= HANDLE_CALL ========================================================= %% 

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

%% Called by its 'sup_loc' location supervisor when a new location tree is created
start_link(Loc_id) ->
 gen_server:start_link(?MODULE,Loc_id,[]).