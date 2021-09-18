-module(ctr_restserver).
-export([start_link/0,init/1,terminate/2,handle_call/3,handle_cast/2]).
-behaviour(gen_server).

% Simulator Database Bridge Functions
%% [TODO]: Remove export when ready
-export([sim_db_sync/3]). 


init(_) ->
 % io:format("[ctr_restserver]: Initialized~n"),
 {ok,[]}.  % Initial State

% STUB
% NOTE: This is currently not called on shutdown since the process is not trapping exit signals
terminate(normal,_) ->
 io:format("[ctr_restserver]: Terminated").

% STUB
handle_call(Num,_,{Sum,N}) when is_number(Num) ->
 New_Sum = Sum + Num,
 New_N = N+1,
 {reply,New_Sum/New_N,{New_Sum,New_N}}.

% STUB
handle_cast(reset,State) -> % Resets the server State
 {noreply,State}.
 
start_link() ->
 gen_server:start_link({local,?MODULE},?MODULE,[],[]).
 



%%====================================================================================================================================
%%                                                SIMULATOR DATABASE BRIDGE FUNCTIONS
%%==================================================================================================================================== 

%% Attempts to perform an operation in the JANET Simulator database and,
%% if successful, attempts to mirror it in the controller's database
sim_db_sync(DBFun,SimArgsList,CtrArgsList) ->

 % Attempt to perform the "DBFun" function with "SimArgsList" arguments in the JANET Simulator database
 % by routing the request via the 'ctr_simserver' to the 'ctr_manager' associated with the controller
 case gen_sim_command(db,DBFun,SimArgsList) of
 
  {error,Error} ->
  
   % If an error was raised in performing the operation, whether it occured in the JANET Simulator database or
   % within the communication, return it without attempting to mirror the operation on the controller's database
   {error,Error};
   
  SimDBRes ->
   
   % If the JANET Simulator database operation was successful, regardless of the results of its
   % side-effects, if any (which are carried in the "SimDBRes" variable), attempt to mirror such
   % operation in the location controller, concatenating in order the results of the two operations
   print_sim_db_sync_result(SimDBRes,apply(ctr_db,DBFun,CtrArgsList))
 end.
   
   
%% Concatenates the result of a database operation in the JANET Simulator with the result of its mirroring
%% in the controller's database into a tuple (sim_db_sync(DBFun,SimArgsList,CtrArgsList) helper function) 
print_sim_db_sync_result(SimDBRes,CtrDBRes) when is_atom(SimDBRes), is_atom(CtrDBRes) ->
 {SimDBRes,CtrDBRes};
print_sim_db_sync_result(SimDBRes,CtrDBRes) when is_atom(SimDBRes), is_tuple(CtrDBRes) ->
 list_to_tuple([SimDBRes] ++ [CtrDBRes]);
print_sim_db_sync_result(SimDBRes,CtrDBRes) when is_tuple(SimDBRes), is_atom(CtrDBRes) ->
 list_to_tuple(tuple_to_list(SimDBRes) ++ [CtrDBRes]);
print_sim_db_sync_result(SimDBRes,CtrDBRes) when is_tuple(SimDBRes), is_tuple(CtrDBRes) ->
 list_to_tuple(tuple_to_list(SimDBRes) ++ [CtrDBRes]).

 
%% Attempts to synchronously execute a command on the JANET Simulator by routing the request
%% via the 'ctr_simserver' to the 'ctr_manager' associated with this controller, returning
%% the result of the operation (sim_db_sync(DBFun,SimArgsList,CtrArgsList) helper function) 
gen_sim_command(Module,Function,ArgsList) ->
 
 % Route the command via the 'ctr_simserver' to the 'ctr_manager' associated
 % with this controller and wait for a response up to a predefined timeout
 try gen_server:call(ctr_simserver,{sim_command,Module,Function,ArgsList},5000)
 catch
    exit:{timeout,_} ->
	   
     % Command timeout
	 {error,request_timeout}
 end.