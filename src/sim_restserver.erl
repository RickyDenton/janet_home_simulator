-module(sim_restserver).
-export([start_link/0,init/1,terminate/2,handle_call/3,handle_cast/2]).
-behaviour(gen_server).

start_link() ->
 gen_server:start_link({local,sim_restserver},?MODULE,[],[]).

init(_) ->
 io:format("[sim_restserver]: Initialized~n"),
 {ok,[]}.  % Initial State

% STUB
% NOTE: This is currently not called on shutdown since the process is not trapping exit signals
terminate(normal,{_,N}) ->
 io:format("[sim_restserver]: Terminated").

% STUB
handle_call(Num,_,{Sum,N}) when is_number(Num) ->
 New_Sum = Sum + Num,
 New_N = N+1,
 {reply,New_Sum/New_N,{New_Sum,New_N}}.

% STUB
handle_cast(reset,State) -> % Resets the server State
 {noreply,State}.