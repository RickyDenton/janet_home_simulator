-module(dev_server).
-behaviour(gen_server).

-export([start_link/0,init/1,terminate/2,handle_call/3,handle_cast/2]).




init(_) ->

 {ok,MgrPid} = application:get_env(mgrpid),
 gen_server:cast(MgrPid,{dev_server_pid,self()}),

 io:format("[dev_server]: Initialized~n"),
 {ok,[]}.  % Initial State

% STUB
% NOTE: This is currently not called on shutdown since the process is not trapping exit signals
terminate(normal,_) ->
 io:format("[dev_server]: Terminated").

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
 
