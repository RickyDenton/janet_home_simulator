-module(loc_ctrl).
-export([start_link/2,init/1,terminate/2,handle_call/3,handle_cast/2]).
-behaviour(gen_server).

start_link(ControllerID,DevicesList) ->
 gen_server:start_link({local,utils:concat_atoms(loc_ctrl_,ControllerID)},?MODULE,[{ControllerID,DevicesList}],[]).

init({ControllerID,DevicesList}) ->
 io:format("[loc_ctrl_~s]: Initialized (DevicesList = ~w)~n",[atom_to_list(ControllerID),DevicesList]),
 {ok,{ControllerID,DevicesList}}.  % Initial State

% STUB
% NOTE: This is currently not called on shutdown since the process is not trapping exit signals
terminate(normal,{ControllerID,_}) ->
 io:format("[loc_ctrl_~s]: Terminated~n",[atom_to_list(ControllerID)]).

% STUB
handle_call(Num,_,{Sum,N}) when is_number(Num) ->
 New_Sum = Sum + Num,
 New_N = N+1,
 {reply,New_Sum/New_N,{New_Sum,New_N}}.

% STUB
handle_cast(reset,State) -> % Resets the server State
 {noreply,State}.