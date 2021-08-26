-module(jdoor).
-behaviour(gen_statem).

-export([start_link/0,callback_mode/0,init/1,terminate/3]).

%% --------- STUB
-export([button/1]).
-export([locked/3,open/3]).
-define(NAME, code_lock).

%% --------- STUB
callback_mode() ->
    state_functions.


init(_) ->

 {ok,MgrPid} = application:get_env(mgrpid),
 
 io:format("[statem_door]: Initialized~n"),
 
 {ok, locked, MgrPid}.  % Initial State.


%% --------- STUB	
terminate(_Reason, _State, _Data) ->
    ok.
	
%% --------- STUB	
button(Digit) ->
    gen_statem:cast(?NAME, {button,Digit}).

%% --------- STUB
locked(
  cast, {button,Digit},
  #{code := Code, remaining := Remaining} = Data) ->
    case Remaining of
        [Digit] ->
	    do_unlock(),
            {next_state, open, Data#{remaining := Code},
             [{state_timeout,10000,lock}]};
        [Digit|Rest] -> % Incomplete
            {next_state, locked, Data#{remaining := Rest}};
        _Wrong ->
            {next_state, locked, Data#{remaining := Code}}
    end.

open(state_timeout, lock,  Data) ->
    do_lock(),
    {next_state, locked, Data};
open(cast, {button,_}, Data) ->
    {next_state, open, Data}.
	
%% --------- STUB
do_lock() ->
    io:format("Lock~n", []).
do_unlock() ->
    io:format("Unlock~n", []).




start_link() ->
    gen_statem:start_link({local,dev_statem}, ?MODULE, [], []).