%% This module represents the simulated state machine of a device in the JANET Device application %%

-module(dev_statem).
-behaviour(gen_statem).

-export([start_link/2,callback_mode/0,init/1,handle_event/4]). % gen_statem Behaviour Callback Functions

-include("devtypes_configurations_definitions.hrl").  % Janet Device Configuration Records Definitions

%% ------------------------- Inactivity Update Constants ------------------------- %%

% The inactivity time after which the state machine automatically forwards
% its state to the the 'dev_server' via  the 'inactivity_update_timeout' (ms)
-define(Inactivity_update_timeout,60 * 1000).

%% ------------------------- Simulated Activity Constants ------------------------- %%

% Minimum inactivity time after which the 'dev_statem' can change state
% autonomously with the purpose of simulating real-world user interaction (ms)
-define(Min_sim_inactivity,1 * 1000). %30

% Mean of the normal distribution used for defining the time after
% which the 'dev_statem' attempts simulated state changes (ms)
-define(Sim_mean,5 * 1000).    % 30

% Mean of the normal distribution used for defining the time after
% which the 'dev_statem' attempts simulated state changes (ms)
-define(Sim_var, 200 * 1000).   % 200000

%% ------------------- State Machine State and Data Definitions ------------------- %%

%% State: Config#devtypecfg

%% Data
-record(statemdata,    
        {
		 lastupdate,  % The last time the state of the 'dev_statem'
		 type         % The device's type
		}).


%%====================================================================================================================================
%%                                                  GEN_STATEM CALLBACK FUNCTIONS                                                        
%%====================================================================================================================================

%% ======================================================== CALLBACK_MODE ======================================================== %%
callback_mode() ->

 % Defines the event-handling mode of the 'gen_statem' engine
 handle_event_function.
	

%% ============================================================ INIT ============================================================ %%
init({Config,Type}) ->

 % Logging purposes
 %% [TODO]: Remove when ready 
 io:format("[statem_~w]: Initialized (config = ~p)~n",[Type,Config]),

 {ok,                                                                                % Inform the 'gen_statem' engine that the 'dev_statem' has successfully initialized
  Config,                                                                            % Initial State of the 'dev_statem'
  #statemdata{lastupdate = erlang:system_time(second), type = Type},                 % Initial Data of the 'dev_statem' (note that the "LastUpdate" variable will be more properly 
  [                                                                                  %  initialized in the "get_config" call performed by the 'dev_server' during its initialization)
   {{timeout,inactivity_update_timer},?Inactivity_update_timeout,none},              % Inactivity Update Timer
   {{timeout,simulated_activity_timer},next_sim_time(?Sim_mean,?Sim_var),none} % Simulated Activity Timer
  ]
 }.
 
%% ======================================================== HANDLE_EVENT ======================================================== %% 

%% ------------------------------------------------------ GENERIC TIMEOUTS ------------------------------------------------------ %%

%% INACTIVITY UPDATE TIMER
%%
handle_event({timeout,inactivity_update_timer},_,State,Data) ->

 % Get the current time
 Now = erlang:system_time(second),
 if
   (Now - Data#statemdata.lastupdate)*1000 >= ?Inactivity_update_timeout ->
	
    % If at least "?Inactivity_update_timeout" seconds have passed since
    % the 'dev_statem' sent its state to the 'dev_server', send it now  
    gen_server:cast(dev_server,{dev_config_update,{State,Now}}),
	
	% Reinitialize the 'inactivity_update_timer'
	% and update the "LastUpdate" data variable
	{keep_state,Data#statemdata{lastupdate = Now},[{{timeout,inactivity_update_timer},?Inactivity_update_timeout,none}]};
 
   true ->
   
    % Otherwise, restart the 'inactivity_update_timer' so to trigger
    % at "?Inactivity_update_timeout" seconds after the LastUpdate
    {keep_state_and_data,[{{timeout,inactivity_update_timer},?Inactivity_update_timeout-((Now - Data#statemdata.lastupdate)*1000),none}]}
 end;
  
  
%% SIMULATED ACTIVITY TIMER
%%
handle_event({timeout,simulated_activity_timer},_,State,Data) ->

 % Get the current time
 Now = erlang:system_time(second),
 if
   (Now - Data#statemdata.lastupdate)*1000 < ?Min_sim_inactivity ->
	
    % If less that "?Min_sim_inactivity" seconds have passed since the 'dev_statem' sent
	% its state to the 'dev_server' do not simulate activities and simply restart the timer
	{keep_state_and_data,[{{timeout,simulated_activity_timer},next_sim_time(?Sim_mean,?Sim_var),none}]};
 
   true ->
   
    % Otherwise, simulate user activity by generating a simulated candidate state
	CandidateState = simulate_activity(State,Data#statemdata.type),

    io:format("[statem_~w]: Garabba (State = ~p, CandidateState = ~p)~n",[Data#statemdata.type,State,CandidateState]),

	% Merge the generated candidate with the current state
	% and check the validiting of the result new state
	case catch(utils:check_merge_devconfigs(State,CandidateState,Data#statemdata.type)) of
     {ok,NewState} ->     

      % If the new simulated state is valid, ensure that it is different from the current
      if 
	   NewState =/= State ->
   
        % If it is different, send it along with the current time to the 'dev_server'
	    gen_server:cast(dev_server,{dev_config_update,{NewState,Now}}),

        % Update the 'dev_statem' state and "LastUpdate"
	    % variables and reinitialize the simulated activity timer
        {next_state,NewState,Data#statemdata{lastupdate = Now},[{{timeout,simulated_activity_timer},next_sim_time(?Sim_mean,?Sim_var),none}]};

       true ->
	   
	    % If instead there would be no state change, just restart the simulated activity timer
	    io:format("[statem_~w]: SAME simulated new Configuration: ~p~n",[Data#statemdata.type,CandidateState]),
	    {keep_state_and_data,[{{timeout,simulated_activity_timer},next_sim_time(?Sim_mean,?Sim_var),none}]}
	  end;
	  
     {error,invalid_devconfig} ->
      
	   % If instead the new simulated state is not valid, keep the current one and restart the simulated activity timer 
       io:format("[statem_~w]: INVALID simulated new Configuration: ~p~n",[Data#statemdata.type,CandidateState]),
	   {keep_state_and_data,[{{timeout,simulated_activity_timer},next_sim_time(?Sim_mean,?Sim_var),none}]}
    end
 end;

 
%% ------------------------------------------------------- CALL CALLBACKS ------------------------------------------------------- %%
 
%% DEV_CONFIG_CHANGE
%%   
handle_event({call,DevSrvPid},{dev_config_change,CandidateState},State,Data) ->

 % Derive and validate the new state the current and the candidate state
 case catch(utils:check_merge_devconfigs(State,CandidateState,Data#statemdata.type)) of
 
  % If the new state is valid
  {ok,NewState} -> 
   
   % Get the current time
   Now = erlang:system_time(second),

   % Update the 'dev_statem' state and "LastUpdate" variables and return them to the 'dev_server'
   {next_state,NewState,Data#statemdata{lastupdate = Now},[{reply,DevSrvPid,{ok,{NewState,Now}}}]};

  % Otherwise, if the new state is invalid
  {error,invalid_devconfig} ->
  
   % Logging purposes
   %% [TODO]: Remove
   io:format("[statem_~w]: WRONG New Configuration: ~p~n",[Data#statemdata.type,CandidateState]),
   
   % Keep the state and data as they are and inform the
   % 'dev_server' that the passed configuration is invalid   
   {keep_state_and_data,[{reply,DevSrvPid,{error,invalid_devconfig}}]}
 end;


%% GET_CONFIG
%%   
handle_event({call,DevSrvPid},get_config,State,Data) ->

 % Get the current time
 Now = erlang:system_time(second),
 
 % Update the "LastUpdate" variable and return it along with the state to the 'dev_server'
 {keep_state,Data#statemdata{lastupdate = Now},[{reply,DevSrvPid,{ok,{State,Now}}}]}.


%%====================================================================================================================================
%%                                                    PRIVATE HELPER FUNCTIONS
%%==================================================================================================================================== 


 


simulate_activity(State,fan) ->
 OnOff = simulate_onoff_trait(State,fan),
 FanSpeed = simulate_fanspeed_trait(next_onoff(State#fancfg.onoff,OnOff),State#fancfg.fanspeed),
 #fancfg{onoff = OnOff, fanspeed = FanSpeed};
 
simulate_activity(State,light) ->
 OnOff = simulate_onoff_trait(State,light),
 Brightness = simulate_brightness_trait(next_onoff(State#lightcfg.onoff,OnOff),State#lightcfg.brightness),
 ColorSetting = simulate_colorsetting_trait(next_onoff(State#lightcfg.onoff,OnOff)),
 #lightcfg{onoff = OnOff, brightness = Brightness, colorsetting = ColorSetting};
 
simulate_activity(State,door) ->
 {OpenClose,LockUnlock} = simulate_door_traits(State),
 #doorcfg{openclose = OpenClose, lockunlock = LockUnlock};

simulate_activity(_,_) ->
 %% [TODO]
 ok.
 
 
 
 
%% This refers to Type = fan ([TODO]: Normalize)
simulate_onoff_trait(_,fan) ->

 % Retrieve the current hour
 {Hour,_,_} = erlang:time(),
 
 % Generate a uniformely distributed
 % random number between 0.0 <= Rand < 1.0
 Rand = rand:uniform(),
 
 io:format("onoff trait rand = ~w~n",[Rand]),
 
 if
  % Hot hours
  Hour >= 10 andalso Hour =< 19  ->
  
   % 50% on, 20% keep, 30% off
   if 
    Rand =< 0.5 ->
	 on;
	Rand =< 0.7 ->
	 '$keep';
	Rand < 1 ->
	 off
   end;
   
  % Cold hours
  Hour < 10 orelse Hour > 19 ->

   % 50% off, 20% keep, 30% on
   if 
    Rand =< 0.5 ->
	 off;
	Rand =< 0.7 ->
	 '$keep';
	Rand < 1 ->
	 on
   end
 end;

%% This refers to Type = light ([TODO]: Normalize)
simulate_onoff_trait(_,light) ->

 % Retrieve the current hour
 {Hour,_,_} = erlang:time(),
 
 % Generate a uniformely distributed
 % random number between 0.0 <= Rand < 1.0
 Rand = rand:uniform(),
 
 io:format("onoff trait rand = ~w~n",[Rand]),
 
 if
  % Light hours
  Hour >= 8 andalso Hour =< 18  ->
  
   % 60% off, 20% keep, 20% on
   if 
    Rand =< 0.6 ->
	 off;
	Rand =< 0.8 ->
	 '$keep';
	Rand < 1 ->
	 on
   end;
   
  % Dark hours
  Hour < 8 orelse Hour > 18 ->

   % 60% on, 20% keep, 20% off
   if 
    Rand =< 0.6 ->
	 on;
	Rand =< 0.8 ->
	 '$keep';
	Rand < 1 ->
	 off
   end
 end.
 

%% This refers to Type = fan ([TODO]: Normalize)

% If the simulated action results in turning off the 
% device, it has no sense to change its fanspeed
simulate_fanspeed_trait(off,_) ->
 io:format("fanspeed trait off->keep~n"),
 '$keep';
 
simulate_fanspeed_trait(on,CurrFanSpeed) -> 
 % Retrieve the current hour
 {Hour,_,_} = erlang:time(),
 
 % Generate a uniformely distributed
 % random number between 0.0 <= Rand < 1.0
 Rand = rand:uniform(),
 
 io:format("fanspeed trait rand = ~w~n",[Rand]),
 
 if
  % Hot hours
  Hour >= 10 andalso Hour =< 19  ->
  
   % 50% increase, 20% keep, 30% decrease of 10, 20 or 30
   % units % (uniformely distributed, and consider capping)
   if 
    Rand =< 0.5 ->
	 min(100,CurrFanSpeed + (rand:uniform(3)*10));
	Rand =< 0.7 ->
	 '$keep';
	Rand < 1 ->
	 max(10,CurrFanSpeed - (rand:uniform(3)*10))
   end;
   
  % Cold hours
  Hour < 10 orelse Hour > 19 ->

   % 50% decrease, 20% keep, 30% increase of 10, 20 or 30
   % units % (uniformely distributed, and consider capping)
   if 
    Rand =< 0.5 ->
	 max(10,CurrFanSpeed - (rand:uniform(3)*10));
	Rand =< 0.7 ->
	 '$keep';
	Rand < 1 ->
	 min(100,CurrFanSpeed + (rand:uniform(3)*10))
   end
 end.


% If the simulated action results in turning off the 
% light, it has no sense to change its brightness
simulate_brightness_trait(off,_) ->
 io:format("brightness trait off->keep~n"),
 '$keep';
 
simulate_brightness_trait(on,CurrBrightness) -> 
 % Retrieve the current hour
 {Hour,_,_} = erlang:time(),
 
 % Generate a uniformely distributed
 % random number between 0.0 <= Rand < 1.0
 Rand = rand:uniform(),
 
 io:format("brightness trait rand = ~w~n",[Rand]),
 
 if
  % Light hours
  Hour >= 8 andalso Hour =< 18  ->
  
   % 60% decrease, 20% keep, 20% increase of 10, 20 or 30
   % units % (uniformely distributed, and consider capping)
   if 
    Rand =< 0.6 ->
	 max(10,CurrBrightness - (rand:uniform(3)*10));
	Rand =< 0.8 ->
	 '$keep';
	Rand < 1 ->
	 min(100,CurrBrightness + (rand:uniform(3)*10))
   end;
   
  % Dark hours
  Hour < 8 orelse Hour > 18 ->

   % 60% increase, 20% keep, 20% decrease of 10, 20 or 30
   % units % (uniformely distributed, and consider capping)
   if 
    Rand =< 0.6 ->
	 min(100,CurrBrightness + (rand:uniform(3)*10));
	Rand =< 0.8 ->
	 '$keep';
	Rand < 1 ->
	 max(10,CurrBrightness - (rand:uniform(3)*10))
   end
 end.


simulate_colorsetting_trait(off) ->
 io:format("colorsetting trait off->keep~n"),
 '$keep';
simulate_colorsetting_trait(on) ->

 % Generate a uniformely distributed
 % random number between 0.0 <= Rand < 1.0
 Rand = rand:uniform(),
 
 io:format("colorsetting trait rand = ~w~n",[Rand]),
 
 % 60% keep, 40% random new color selected through a random number from 1 to 10
 if 
  Rand =< 0.6 ->
   '$keep';
  Rand < 1 ->
   case rand:uniform(10) of
    1 -> "White";
	2 -> "Red";
	3 -> "Yellow";
	4 -> "Blue";
	5 -> "Purple";
	6 -> "Green";
	7 -> "Orange";
	8 -> "Brown";
	9 -> "Pink";
	10 -> "Teal"
   end
 end.
 
 
 
 
 
 
simulate_door_traits(State) ->

 % Retrieve the current hour
 {Hour,_,_} = erlang:time(),
 
 % Generate a uniformely distributed
 % random number between 0.0 <= Rand < 1.0
 Rand = rand:uniform(),
 
 io:format("door traits rand = ~w~n",[Rand]),
 
 if
 
  % "Movement Hours"
  (Hour >= 6 andalso Hour =< 9) orelse
  (Hour >= 12 andalso Hour =<14) orelse
  (Hour >= 18 andalso Hour =<21) ->
  
   case {State#doorcfg.openclose,State#doorcfg.lockunlock} of

    {open,unlock} ->
   
     % 40% {open,unlock} (keep), 60% {close,unlock} 
     if
 	  Rand =< 0.4 ->
	   {open,unlock};
	  Rand < 1 ->
	   {close,unlock}
	 end;
	
    {close,unlock} ->
   
      % 40% {close,unlock} (keep), 40% {open,unlock}, 20% {close,lock}
      if
	   Rand =< 0.4 ->
	    {close,unlock};
	   Rand =< 0.8 ->
	    {open,unlock};
	   Rand < 1 ->
	    {close,lock}
	  end;

    {close,lock} ->
   
     % 60% {close,lock} (keep), 40% {close,unlock}
     if
	  Rand =< 0.6 ->
	   {close,lock};
	  Rand < 1 ->
	   {close,unlock}
	 end
   end;	
	  
  % "Working Hours"
  true ->
  
   case {State#doorcfg.openclose,State#doorcfg.lockunlock} of

    {open,unlock} ->
   
     % 40% {open,unlock} (keep), 60% {close,unlock} 
     if
 	  Rand =< 0.4 ->
	   {open,unlock};
	  Rand < 1 ->
	   {close,unlock}
	 end;
	
    {close,unlock} ->
   
      % 20% {close,unlock} (keep), 20% {open,unlock}, 60% {close,lock}
      if
	   Rand =< 0.2 ->
	    {close,unlock};
	   Rand =< 0.4 ->
	    {open,unlock};
	   Rand < 1 ->
	    {close,lock}
	  end;

    {close,lock} ->
   
     % 60% {close,lock} (keep), 40% {close,unlock}
     if
	  Rand =< 0.6 ->
	   {close,lock};
	  Rand < 1 ->
	   {close,unlock}
	 end
   end
 end.

 
 
 
 
 
 
 
 
next_onoff(CurrOnOff,'$keep') ->
 CurrOnOff; 
next_onoff(_,CandidateOnOff) ->
 CandidateOnOff.




%% DESCRIPTION:  Returns the time in ms after which a new simulated activity will occur in a 'dev_statem'
%%               as a random value taken from a normal distribution of its "Mean" and "Var" arguments
%%
%% ARGUMENTS:    - Mean: The mean of the normal distribution to be used for generating the random value
%%               - Var:  of the normal distribution to be used for generating the random value
%%
%% RETURNS:      - Next_sim_time_ms -> The time after which a new simulated activity will occur in a
%%                                     'dev_statem' (lower-capped to 100ms to prevent negative values)
%%
next_sim_time(Mean,Var) ->
 max(100,trunc(rand:normal(Mean,Var))).




%%====================================================================================================================================
%%                                                         START FUNCTION                                                        
%%==================================================================================================================================== 

%% Called by its 'sup_jdev' supervisor during the JANET Device boot
start_link(Config,Type) ->
    gen_statem:start_link({local,?MODULE}, ?MODULE, {Config,Type}, []).  % The spawned process is also registered locally under the 'dev_statem' name