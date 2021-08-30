%% This module represents the state machine of a simulated fan in the JANET Device application %%

-module(jfan).
-behaviour(gen_statem).

-export([start_link/1,callback_mode/0,init/1,handle_event/4]). % gen_statem Behaviour Callback Functions

-include("devtypes_configurations_definitions.hrl").  % Janet Device Configuration Records Definitions

%%====================================================================================================================================
%%                                                  GEN_STATEM CALLBACK FUNCTIONS                                                        
%%====================================================================================================================================

%% ============================================ CALLBACK_MODE (handle_event_function) ============================================ %%
callback_mode() ->
    handle_event_function.
	

%% ============================================================ INIT ============================================================ %%
init(Cfg) ->
 
 io:format("[statem_fan]: Initialized (config = ~p)~n",[Cfg]),

 % Propagate the initial state
 gen_server:cast({local,dev_server},{dev_config_update,{Cfg,erlang:system_time(second)}}),

 {ok,Cfg#fancfg.onoff,Cfg}.  % Initial State + Data
 
%% ======================================================== HANDLE_EVENT ======================================================== %% 

handle_event({call,DevSrvPid},{dev_config_change,NewCfg},_,_) ->

 IsValidConfig = catch(utils:validate_dev_config(NewCfg,fan)),
 case IsValidConfig of
 
  ok -> 
   
   io:format("[statem_fan]: New Configuration: ~p~n",[NewCfg]),
   {next_state,NewCfg#fancfg.onoff,NewCfg#fancfg.fanspeed,[{reply,DevSrvPid,{ok,{NewCfg,erlang:system_time(second)}}}]};
   
  _ ->
  
   io:format("[statem_fan]: WRONG New Configuration: ~p~n",[NewCfg]),
   {keep_state_and_data,[{reply,DevSrvPid,IsValidConfig}]}
 end;

% Get config
handle_event({call,DevSrvPid},get_config,_,Cfg) ->
 {keep_state_and_data,[{reply,DevSrvPid,{ok,{Cfg,erlang:system_time(second)}}}]}.



%%====================================================================================================================================
%%                                                         START FUNCTION                                                        
%%==================================================================================================================================== 

%% Called by its 'sup_jdev' supervisor during the JANET Device boot
start_link(Config) ->
    gen_statem:start_link({local,dev_statem}, ?MODULE, Config, []).  % The spawned process is also registered locally under the 'dev_statem' name