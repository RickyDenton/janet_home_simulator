-module(light).
-export([start_link/0,init/1,terminate/2,handle_call/3,handle_cast/2,handle_event/3,handle_sync_event/4]).
-behaviour(gen_fsm).

init(_) ->
 io:format("[fsm_light]: Initialized~n"),
 {ok,[]}.  % Initial State

% STUB
% NOTE: This is currently not called on shutdown since the process is not trapping exit signals
terminate(normal,_) ->
 io:format("[fsm_light]: Terminated").

% STUB
handle_call(Num,_,{Sum,N}) when is_number(Num) ->
 New_Sum = Sum + Num,
 New_N = N+1,
 {reply,New_Sum/New_N,{New_Sum,New_N}}.

% STUB
handle_cast(reset,State) -> % Resets the server State
 {noreply,State}.



% STUB 
handle_event(Event, StateName, Data) ->
 unexpected(Event, StateName),
{next_state, StateName, Data}. 

% STUB
handle_sync_event(Event, _From, StateName, Data) ->
 unexpected(Event, StateName),
 {next_state, StateName, Data}.
 
% STUB
unexpected(Msg, State) ->
 io:format("~p received unknown event ~p while in state ~p~n",
 [self(), Msg, State]).


 
start_link() ->
 gen_server:start_link({local,dev_fsm},?MODULE,[],[]).