-module(ctr_manager).
-behaviour(gen_server).
-export([start_link/1,init/1,terminate/2,handle_call/3,handle_cast/2]).


-include("table_records.hrl").   % Mnesia table records definition


%% ================================================ GEN_SERVER CALLBACK FUNCTIONS ================================================ %%


init(LocationRecord) ->

 % Trap Exit signals
 process_flag(trap_exit,true),
 
 % Register the controller's manager in the ctrmanager table
 Loc_id = LocationRecord#location.loc_id,
 {atomic,ok} = mnesia:transaction(fun() -> mnesia:write(#ctrmanager{loc_id=Loc_id,sup_pid=self(),status="BOOTING"}) end),
 
 %% [TODO]: Continue with initialization
 io:format("[ctr_manager ~w]: Initialized~n",[Loc_id]),
 {ok,[]}.  % Initial State
   
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