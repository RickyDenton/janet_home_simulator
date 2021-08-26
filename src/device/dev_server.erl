-module(dev_server).
-behaviour(gen_server).

-export([start_link/0,init/1,terminate/2,handle_call/3,handle_cast/2,handle_continue/2]).


init(_) ->
 {ok,state,{continue,init}}.
 
 
%% ======================================================= HANDLE_CONTINUE ======================================================= %%
  
handle_continue(init,_) ->

 {ok,MgrPid} = application:get_env(mgrpid),
 
 case whereis(dev_statem) of
 
  undefined ->
   {stop,dev_statem_not_registered,halt};
   
  DevFSMPID ->
   MgrReg = try gen_server:call(MgrPid,{dev_reg,self(),DevFSMPID},5000)
   catch 
   exit:{timeout,_} -> 
    timeout
   end,
 
   case MgrReg of
 
    ok ->
     io:format("[dev_server]: Initialized~n"),
     {noreply,somestate};  % Initial State

    timeout ->
   
     io:format("NOPE~n"),
     {noreply,somestate}  % Initial State
     %{stop,mgr_reg_timeout}
   end
 end.


 
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
 
