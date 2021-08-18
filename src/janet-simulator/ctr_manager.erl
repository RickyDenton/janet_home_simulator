-module(ctr_manager).
-behaviour(gen_server).
-export([start_link/1,init/1,terminate/2,handle_call/3,handle_cast/2,handle_continue/2]).

-include("table_records.hrl").   % Mnesia table records definition


%% ================================================ GEN_SERVER CALLBACK FUNCTIONS ================================================ %%


init(LocationRecord) ->

 % Trap Exit signals
 process_flag(trap_exit,true),
 
 % Register the controller's manager in the ctrmanager table
 {atomic,ok} = mnesia:transaction(fun() -> mnesia:write(#ctrmanager{loc_id=LocationRecord#location.loc_id,sup_pid=self(),status="BOOTING"}) end),
 
 % The controller node initialization will continue in the handle_continue(Continue,State) callback for parallelization purposes 
 {ok,{booting,none,LocationRecord},{continue,init}}.
 
  
%% Continues with the initialization
handle_continue(init,{booting,none,LocationRecord}) ->
 
 % Retrieve the Loc_id, both as a number and as a string
 Loc_id = LocationRecord#location.loc_id,
 Loc_id_str = integer_to_list(Loc_id),
 
 % Retrieve the location port
 Loc_port = LocationRecord#location.port,
 
 % Prepare the node's "devalloc" table from the contents of the sublocation table
 {atomic,SublocationRecords} = mnesia:transaction(fun() -> mnesia:match_object(#sublocation{sub_id = {Loc_id,'_'}, _ = '_'}) end),
 DevAlloc = derive_devalloc(SublocationRecords,[]),
 
 % Retrieve the Remote Host environment variable
 {ok,RemoteHost} = application:get_env(remotehost),
 
 
 % Set the cookie for connecting to the target node (NOTE: the use of atoms is required by the erlang:set_cookie BIF) 
 erlang:set_cookie(devutils:str_to_atom("ctr-" ++ Loc_id_str ++ "@localhost"),devutils:str_to_atom(Loc_id_str)),
 
 % Prepare the controller's node Name, Host and VM arguments
 NodeHost = "localhost",
 NodeName = "ctr-" ++ Loc_id_str,
 NodeArgs = "-setcookie " ++ Loc_id_str ++ " -connect_all false -pa ebin/",
 
 % Instantiate and link to the slave controller node
 {ok,Node} = slave:start_link(NodeHost,NodeName,NodeArgs),
 
 % Launch the controller application on the slave node
 ok = rpc:call(Node,jctr,run,[Loc_id,DevAlloc,self(),Loc_port,RemoteHost]),
 
 % Set the controller's status in the ctrmanager table to "ONLINE"
 {atomic,ok} = mnesia:transaction(fun() -> mnesia:write(#ctrmanager{loc_id=Loc_id,sup_pid=self(),status="ONLINE"}) end),
 
 io:format("[ctr_manager ~w]: Controller node successfully initialized~n",[Loc_id]),
 
 {noreply,{online,Node,Loc_id}}.


%% Utility function for deriving the devalloc table for the location 
derive_devalloc([],DevAlloc) ->
 DevAlloc;
derive_devalloc([Subloc|NextSubloc],DevAlloc) ->
 {_,Subloc_id} = Subloc#sublocation.sub_id,
 NextDevAlloc = DevAlloc ++ [{Subloc_id,Subloc#sublocation.devlist}],
 derive_devalloc(NextSubloc,NextDevAlloc).



 
 %{ok,[booting]}.  % Initial State
   
 % If something failed {stop,Reason}
 
 
 
 
 
%% --------- STUB
terminate(normal,_) ->
 io:format("[ctr_manager ]: Terminated").

%% --------- STUB
handle_call(Num,_,{Sum,N}) when is_number(Num) ->
 New_Sum = Sum + Num,
 New_N = N+1,
 {reply,New_Sum/New_N,{New_Sum,New_N}}.

%% --------- STUB
handle_cast(reset,State) -> % Resets the server State
 {noreply,State}.

%% ======================================================== START FUNCTION ======================================================== %%

%% Called by the associated "loc_sup" when starting the controller's manager 
start_link(LocationRecord) ->
 gen_server:start_link(?MODULE,LocationRecord,[]).