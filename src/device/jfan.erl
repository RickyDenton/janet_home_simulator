%% This module represents the state machine of a simulated fan in the JANET Device application %%

-module(jfan).
-behaviour(gen_statem).

-export([start_link/1,callback_mode/0,init/1,handle_event/4]). % gen_statem Behaviour Callback Functions

-include("devtypes_configurations_definitions.hrl").  % Janet Device Configuration Records Definitions

%% ------------------------- Inactivity Update Constants ------------------------- %%

% The inactivity time after which the state machine automatically forwards
% its state to the the 'dev_server' via  the 'inactivity_update_timeout' (ms)
-define(Inactivity_update_timeout,60 * 1000).

%% ------------------------- Simulated Activity Constants ------------------------- %%

% Minimum inactivity time after which the 'dev_statem' can change state
% autonomously with the purpose of simulating real-world user interaction (ms)
-define(Min_sim_inactivity,20 * 1000).

% Mean of the normal distribution used for defining the time after
% which the 'dev_statem' attempts simulated state changes (ms)
-define(Sim_mean,30 * 1000).

% Mean of the normal distribution used for defining the time after
% which the 'dev_statem' attempts simulated state changes (ms)
-define(Sim_var, 200000 * 1000).

%% ------------------- State Machine State and Data Definitions ------------------- %%

% State: Config#fancfg
% Data:  - LastUpdate -> The last time the state of the 'dev_statem'
%                        was sent to the 'dev_server' process

%%====================================================================================================================================
%%                                                  GEN_STATEM CALLBACK FUNCTIONS                                                        
%%====================================================================================================================================

%% ======================================================== CALLBACK_MODE ======================================================== %%
callback_mode() ->

 % Defines the event-handling mode of the 'gen_statem' engine
 handle_event_function.
	

%% ============================================================ INIT ============================================================ %%
init(Config) ->

 % Logging purposes
 %% [TODO]: Remove when ready 
 io:format("[statem_fan]: Initialized (config = ~p)~n",[Config]),

 {ok,                                                                                % Inform the 'gen_statem' engine that the 'dev_statem' has successfully initialized
  Config,                                                                            % The initial state of the 'dev_statem'
  erlang:system_time(second),                                                        % Safe initialization of the "LastUpdate" variable (that will be properly initialized 
  [                                                                                  % in the "gen_config" call sent by the 'dev_server' at its initialization)
   {{timeout,inactivity_update_timer},?Inactivity_update_timeout,none},              % Inactivity Update Timer
   {{timeout,simulated_activity_timer},utils:next_sim_time(?Sim_mean,?Sim_var),none} % Simulated Activity Timer
  ]
 }.
 
%% ======================================================== HANDLE_EVENT ======================================================== %% 

%% ------------------------------------------------------ GENERIC TIMEOUTS ------------------------------------------------------ %%

%% INACTIVITY UPDATE TIMER
%%
handle_event({timeout,inactivity_update_timer},_,Config,LastUpdate) ->

 % Get the current time
 Now = erlang:system_time(second),
 if
   (Now - LastUpdate)*1000 >= ?Inactivity_update_timeout ->
	
    % If at least "?Inactivity_update_timeout" seconds have passed since
    % the 'dev_statem' sent its state to the 'dev_server', send it now  
    gen_server:cast(dev_server,{dev_config_update,{Config,Now}}),
	
	% Reinitialize the 'inactivity_update_timer'
	% and update the "LastUpdate" variable
	{keep_state,Now,[{{timeout,inactivity_update_timer},?Inactivity_update_timeout,none}]};
 
   true ->
   
    % Otherwise, restart the 'inactivity_update_timer' so to trigger
    % at "?Inactivity_update_timeout" seconds after the LastUpdate
    {keep_state_and_data,[{{timeout,inactivity_update_timer},?Inactivity_update_timeout-((Now - LastUpdate)*1000),none}]}
 end;
  
  
%% SIMULATED ACTIVITY TIMER
%%
handle_event({timeout,simulated_activity_timer},_,_,LastUpdate) ->

 % Get the current time
 Now = erlang:system_time(second),
 if
   (Now - LastUpdate)*1000 < ?Min_sim_inactivity ->
	
    % If less that "?Min_sim_inactivity" seconds have passed since the 'dev_statem' sent
	% its state to the 'dev_server' do not simulate activities and simply restart the timer
	{keep_state_and_data,[{{timeout,simulated_activity_timer},?Inactivity_update_timeout,none}]};
 
   true ->
   
    % Otherwise, simulate user activity
	
    %% [TODO]: Remove
	io:format("[Simulated Activity] (after ~w seconds from LastUpdate)~n",[(Now - LastUpdate)]),

    %% [TODO]: Implement
    {keep_state_and_data,[{{timeout,simulated_activity_timer},utils:next_sim_time(?Sim_mean,?Sim_var),none}]}
 end;

 
%% ------------------------------------------------------- CALL CALLBACKS ------------------------------------------------------- %%
 
%% DEV_CONFIG_CHANGE
%%   
handle_event({call,DevSrvPid},{dev_config_change,UpdateCfg},Config,_) ->

 % Derive and validate the new configuration to be applied from the current and the updated configuration 
 case catch(utils:check_merge_devconfigs(Config,UpdateCfg,fan)) of
 
  % If the new configuration is valid
  {ok,NewConfig} -> 
   
   % Get the current time
   Now = erlang:system_time(second),

   % Update the 'dev_statem' state and "LastUpdate" variables and return them to the 'dev_server'
   {next_state,NewConfig,Now,[{reply,DevSrvPid,{ok,{NewConfig,Now}}}]};

  % Otherwise, if the new configuration is invalid
  {error,invalid_devconfig} ->
  
   % Logging purposes
   %% [TODO]: Remove
   io:format("[statem_fan]: WRONG New Configuration: ~p~n",[UpdateCfg]),
   
   % Keep the 'dev_statem' state and LastUpdate variables and inform
   % the 'dev_server' that the passed configuration is invalid   
   {keep_state_and_data,[{reply,DevSrvPid,{error,invalid_devconfig}}]}
 end;


%% GET_CONFIG
%%   
handle_event({call,DevSrvPid},get_config,Config,_) ->

 % Get the current time
 Now = erlang:system_time(second),
 
 % Update the "LastUpdate" variable and return it along with the state to the 'dev_server'
 {keep_state,Now,[{reply,DevSrvPid,{ok,{Config,Now}}}]}.


%%====================================================================================================================================
%%                                                         START FUNCTION                                                        
%%==================================================================================================================================== 

%% Called by its 'sup_jdev' supervisor during the JANET Device boot
start_link(Config) ->
    gen_statem:start_link({local,dev_statem}, ?MODULE, Config, []).  % The spawned process is also registered locally under the 'dev_statem' name