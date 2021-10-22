%% This module represents the simulated state machine of a device in the JANET Device application %%

-module(dev_statem).
-behaviour(gen_statem).

-export([start_link/2,callback_mode/0,init/1,handle_event/4]). % gen_statem Behaviour Callback Functions

-include("devtypes_configurations_definitions.hrl").  % Janet Device Configuration Records Definitions

%% ------------------------- Inactivity Update Constants ------------------------- %%

% The inactivity time after which the 'dev_statem' automatically forwards
% its state to the 'dev_server' via the Inactivity Update Timer (ms)
-define(Inactivity_update_timeout,90 * 1000).  % Default: 90 * 1000

%% ------------------------- Simulated Activity Constants ------------------------- %%

% Minimum inactivity time afterr which the 'dev_statem' may attempt
% to simulate a state change via the Simulated Activity Timer (ms)
-define(Min_sim_inactivity,25 * 1000).         % Default: 25 * 1000

% Mean and Variance of the normal distribution using for determining the next time at which
% the 'dev_statem' attempts to simulate a state change via the Simulated Activity Timer (ms)
-define(Sim_mean,30 * 1000).                   % Default: 30 * 1000
-define(Sim_var, 200000 * 1000).               % Default: 200000 * 1000

%% -- Ambient Temperature Evolution Constants (thermostat and conditioner only) -- %%

% Mean and Variance of the normal distribution used for determining the next time at 
% which the 'dev_statem' attempts to evolve its 'temp_current' ambient temperature (ms)
-define(Amb_base_mean,30 * 1000).              % Default: 30 * 1000
-define(Amb_base_var, 200000 * 1000).          % Default: 200000 * 1000

%% ------------------- State Machine State and Data Definitions ------------------- %%

%% State: Config#devtypecfg
%% Data:  #statemdata

-record(statemdata,    
        {
		 lastupdate,  % The last time the state of the 'dev_statem' was sent to the 'dev_server'
		 type         % The 'dev_statem' device type (fan|light|door|thermostat|conditioner)
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
 
 % Define the list of timers used by the 'dev_statem', which include:
 %
 % 1) The Inactivity Update Timer
 % 2) The Simulated Activity Timer
 % 3) The Ambient Temperature Update Timer (thermostat and conditioner ONLY)
 %
 if

  % Thermostat and Conditioner timers
  Type =:= thermostat orelse Type =:= conditioner ->
   StatemTimers = [ 
                   {{timeout,inactivity_update_timer},?Inactivity_update_timeout,none},
                   {{timeout,simulated_activity_timer},next_sim_time(),none},
                   {{timeout,ambient_temperature_update_timer},next_amb_time(Config),none}
	              ];
				  
  % All other device types timers
  true ->			
   StatemTimers = [ 
                   {{timeout,inactivity_update_timer},?Inactivity_update_timeout,none},
                   {{timeout,simulated_activity_timer},next_sim_time(),none}
	              ]
 end,
 
 % Return the initialization tuple to the 'gen_statem' engine:
 {
  ok,                                                                  % Indicates to the engine that the 'dev_statem' can start
  Config,                                                              % The initial State of the 'dev_statem'
  #statemdata{lastupdate = erlang:system_time(second), type = Type},  % The initial Data of the 'dev_statem'
                                                                       % NOTE: The 'lastupdate' variable will be more properly set in the
																	   %       'get_config' call performed by the 'dev_server' at its initialization
  StatemTimers                                                         % The list of timers to be initialized
 }.

 
%% ======================================================== HANDLE_EVENT ======================================================== %% 

%% ----------------------------------------------- GENERIC TIMERS (NAMED TIMERS) ----------------------------------------------- %%

%% INACTIVITY UPDATE TIMER
%% -----------------------
%% PURPOSE:   Check if the state of the 'dev_statem' should be sent to the 'dev_server' due to inactivity
%% ACTIONS:   1.1) If less than "?Inactivity_update_timeout" have passed since the 'dev_statem' sent its
%%                 state to the 'dev_server', reinitialize the inactivity update timer so to trigger
%%                 "?Inactivity_update_timeout" milliseconds from the last update
%%            1.2) If "?Inactivity_update_timeout" or more milliseconds have passed since the 'dev_statem'
%%                 sent its state to the 'dev_server', send it now along with the current time, and reinitialize
%%                 the Inactivity Update Timer so to trigger "?Inactivity_update_timeout" milliseconds from now
%% NEW STATE: -
%% NEW DATA:  1.1- -
%%            2.2- Update "LastUpdate" to Now
%%
handle_event({timeout,inactivity_update_timer},_,State,Data) ->

 % Get the current OS time in seconds (UNIX time)
 Now = erlang:system_time(second),
 
 if
   (Now - Data#statemdata.lastupdate)*1000 >= ?Inactivity_update_timeout ->
	
    % If "?Inactivity_update_timeout" or more milliseconds have passed since the 'dev_statem'
    % sent its state to the 'dev_server', send it now along with the current time
    gen_server:cast(dev_server,{dev_config_update,{State,Now}}),
	
	% Reinitialize the Inactivity Update Timer so to trigger "?Inactivity_update_timeout"
	% milliseconds from now and update the "LastUpdate" data variable to the current time
	{keep_state,Data#statemdata{lastupdate = Now},[{{timeout,inactivity_update_timer},?Inactivity_update_timeout,none}]};
 
   true ->
   
    % Otherwise, if less than "?Inactivity_update_timeout" have passed since the 'dev_statem' sent its state to the 'dev_server',
	% reinitialize the inactivity update timer so to trigger "?Inactivity_update_timeout" milliseconds from the last update 
    {keep_state_and_data,[{{timeout,inactivity_update_timer},?Inactivity_update_timeout-((Now - Data#statemdata.lastupdate)*1000),none}]}
 end;
 
  
%% SIMULATED ACTIVITY TIMER
%% ------------------------
%% PURPOSE:   Attempt if applicable to autonomously change the state of the 'dev_statem'
%%            with the purpose of simulating physical user interaction with the device
%% ACTIONS:   1) Ensure that at least "?Min_sim_inactivity" milliseconds have passed from the last time the
%%               'dev_statem' sent its state to the 'dev_server', and if so attempt to simulate a state change.
%%            2) Reinitialize the Simulated Activity Timer so to trigger at a random time obtained
%%               via a normal distribution defined by the "?Sim_mean" and "?Sim_var" constants
%% NEW STATE: The new simulated state, if applicable
%% NEW DATA:  If the state changed, update "LastUpdate" to "Now"
%%  
handle_event({timeout,simulated_activity_timer},_,State,Data) ->

 % Get the current OS time in seconds (UNIX time)
 Now = erlang:system_time(second),
 
 if
   (Now - Data#statemdata.lastupdate)*1000 < ?Min_sim_inactivity ->
	
    % If less that "?Min_sim_inactivity" milliseconds have passed since the 'dev_statem' sent its state to the
	% 'dev_server', do not attempt to simulate a state change and simply restart the Simulated Activity Timer
	{keep_state_and_data,[{{timeout,simulated_activity_timer},next_sim_time(),none}]};
 
   true ->
   
    % Otherwise, simulate an appropriate new state for the 'dev_statem'
	CandidateState = simulate_activity(State,Data#statemdata.type),

    % Logging purposes
    % io:format("[statem_~w]: State = ~p, CandidateState = ~p~n",[Data#statemdata.type,State,CandidateState]),

    % Merge the simulated with the current state and check the validity of the resulting new state
	case catch(utils:check_merge_devconfigs(State,CandidateState,Data#statemdata.type)) of
     {ok,NewState} ->     

      % If the new simulated state is valid, ensure it to be different from the current
      if 
	   NewState =/= State ->
   
        % If it is different, send it along with the current time to the 'dev_server'
	    gen_server:cast(dev_server,{dev_config_update,{NewState,Now}}),

        % Update the 'dev_statem' State and "LastUpdate" variables and reinitialize the Simulated Activity Timer
        {next_state,NewState,Data#statemdata{lastupdate = Now},[{{timeout,simulated_activity_timer},next_sim_time(),none}]};

       true ->
	   
	    % If instead the current and simulated states are the same, just restart the Simulated Activity Timer
		
		% Logging purposes
	    % io:format("[statem_~w]: SAME simulated new Configuration: ~p~n",[Data#statemdata.type,CandidateState]),
	    
		{keep_state_and_data,[{{timeout,simulated_activity_timer},next_sim_time(),none}]}
	  end;
	  
     {error,invalid_devconfig} ->
      
	   % If instead the new simulated state is not valid, keep the current one and restart the Simulated Activity Timer
	   %
	   % NOTE: This is more of a failsafe, and should NOT happen during the execution
       %
       
	   % Logging purposes
       % io:format("[statem_~w]: INVALID simulated new Configuration: ~p~n",[Data#statemdata.type,CandidateState]),
	   
	   {keep_state_and_data,[{{timeout,simulated_activity_timer},next_sim_time(),none}]}
    end
 end;


%% AMBIENT TEMPERATURE UPDATE TIMER
%% --------------------------------
%% PURPOSE:   Autonomously update the 'temp_current' ambient temperature of a 'thermostat' or 'conditioner' device
%% ACTIONS:   1) Determine the equilibrium temperature, which is given by the 'temp_target' if the 
%%               device is ON or by a constant dependent on the hour of day if the device is OFF
%%            2) Determine the difference between the equilibrium and the current 'temp_current' temperature
%%            3) Randomly determine a new 'temp_current', which is more probable to
%%               drift towards the equilibrium temperature the greater their difference
%%            4) If the new and current 'temp_current' values differ, update the 'dev_statem'
%%               state and send it along with the current time to the 'dev_server'
%%            5) Reinitialize the Ambient Temperature Timer so to trigger at a random time obtained via a normal
%%               distribution whose defining constants "?Amb_base_mean" and "?Amb_base_var" are reduced by a
%%               factor proportional to the new difference between the equilibrium and the current temperature
%%               (so to shorten on average the interval between ambient temperature updates the greater the
%%               distance from the equilibrium point) 
%% NEW STATE: If it differs from the current, the updated 'temp_current'
%% NEW DATA:  If 'temp_current' was changed, update "LastUpdate" to "Now"
%%  
handle_event({timeout,ambient_temperature_update_timer},_,State,Data) ->

 % Retrieve the 'temp_current' value depending on the device type
 if
  Data#statemdata.type =:= thermostat ->
   TempCurrent = State#thermocfg.temp_current;
  Data#statemdata.type =:= conditioner ->
   TempCurrent = State#condcfg.temp_current
 end,
   
 % Compute the difference between the equilibrium and the current temperature
 TempDiff = temp_diff_from_eq(State),

 % Logging purposes
 % io:format("dev_amb_traits = {~w,~w} (TempCurrent,TempDiff)~n",[TempCurrent,TempDiff]),

 % Randomly determine a new "temp_current" for the device, which may drift from its current value of at most
 % 1 degree (limit whose purpose is to allow the 'dev_statem' to report every ambient temperature update)
 NewTempCurrent = update_tempcurrent_trait(TempCurrent,TempDiff),

 if
  NewTempCurrent =/= TempCurrent ->
  
   % If the new and current 'temp_current' values differ, update
   % the state of the 'dev_statem' depending on the device type 
   if
    Data#statemdata.type =:= thermostat ->
     NewState = State#thermocfg{temp_current = NewTempCurrent};
    Data#statemdata.type =:= conditioner ->
     NewState = State#condcfg{temp_current = NewTempCurrent}
   end,
  
   % Get the current OS time in seconds (UNIX time)
   Now = erlang:system_time(second),
  
   % Send the new state along with the current time to the 'dev_server'
   gen_server:cast(dev_server,{dev_config_update,{NewState,Now}}),

   % Logging purposes
   % io:format("[statem_~w]: NEW simulated TempCurrent: ~w~n",[Data#statemdata.type,NewTempCurrent]),
  
   % Update the 'dev_statem' State and "LastUpdate" variables and reinitialize
   % the Ambient Temperature Timer as explained in the notes above
   {next_state,NewState,Data#statemdata{lastupdate = Now},[{{timeout,ambient_temperature_update_timer},next_amb_time(NewState),none}]};
   
  true ->
  
   % If instead the new and current 'temp_current' values are the same, just
   % restart the Ambient Temperature Timer as explained in the notes above
   
   % Logging purposes
   % io:format("[statem_~w]: SAME simulated new TempCurrent: ~w~n",[Data#statemdata.type,NewTempCurrent]),
   
   {keep_state_and_data,[{{timeout,ambient_temperature_update_timer},next_amb_time(State),none}]}
 end;
 
 
%% -------------------------------------------------- EXTERNAL CALLS CALLBACKS -------------------------------------------------- %%

%% DEV_CONFIG_CHANGE
%% -----------------
%% SENDER:    The device's 'dev_server'
%% WHEN:      A configuration/state change request is forwarded to the device
%% PURPOSE:   Attempt to change the 'dev_statem' configuration/state
%% CONTENTS:  The Candidate State requested to the device
%% MATCHES:   (always) (when the request comes from the JANET Device node)
%% ACTIONS:   1) Merge the candidate with the current state and check the validity of the resulting new state
%%            2.1) If the new state is valid, return it to the 'dev_server' along with the
%%                 current time, and update the 'dev_statem' State and "LastUpdate" variables
%%            2.2) If the new state is NOT valid, return the error to the
%%                 'dev_server' and preserve the 'dev_statem' State and Data 
%%   
handle_event({call,{DevSrvPid,_Tag}},{dev_config_change,CandidateState},State,Data) when node() =:= node(DevSrvPid) ->

 % Merge the simulated with the current state and check the validity of the resulting new state
 case catch(utils:check_merge_devconfigs(State,CandidateState,Data#statemdata.type)) of
 
  % If the new state is valid
  {ok,NewState} -> 
   
   % Get the current OS time in seconds (UNIX time)
   Now = erlang:system_time(second),

   % Update the 'dev_statem' state and "LastUpdate" variables and return them to the 'dev_server'
   {next_state,NewState,Data#statemdata{lastupdate = Now},[{reply,{DevSrvPid,_Tag},{ok,{NewState,Now}}}]};

  % Otherwise, if the new state is invalid
  {error,invalid_devconfig} ->
  
   % Logging purposes
   % io:format("[statem_~w]: WRONG New Configuration: ~p~n",[Data#statemdata.type,CandidateState]),
   
   % Preserve the 'dev_statem' State and Data and return the error to the 'dev_server'
   {keep_state_and_data,[{reply,{DevSrvPid,_Tag},{error,invalid_devconfig}}]}
 end;


%% DEV_CONFIG_CHANGE
%% -----------------
%% SENDER:    The device's 'dev_server'
%% WHEN:      During the 'dev_server' initialization
%% PURPOSE:   Retrieve the initial state and time of the 'dev_statem'
%% CONTENTS:  -
%% MATCHES:   (always) (when the request comes from the JANET Device node)
%% ACTIONS:   Update the "LastUpdate" data variable and return
%%            it along with the State to the 'dev_server'
%%   
handle_event({call,{DevSrvPid,_Tag}},get_config,State,Data) when node() =:= node(DevSrvPid) ->

 % Get the current OS time in seconds (UNIX time)
 Now = erlang:system_time(second),
 
 % Update the "LastUpdate" data variable and return it along with the State to the 'dev_server'
 {keep_state,Data#statemdata{lastupdate = Now},[{reply,{DevSrvPid,_Tag},{ok,{State,Now}}}]}.


%%====================================================================================================================================
%%                                                    PRIVATE HELPER FUNCTIONS
%%==================================================================================================================================== 

%% ============================================= SIMULATED ACTIVITY HELPER FUNCTIONS ============================================= %% 

%% DESCRIPTION:  Simulates a candidate state for the 'dev_statem' according to its current State and Type
%%
%% ARGUMENTS:    - State#devtypecfg: The 'dev_statem' current state
%%               - Type:             The 'dev_statem' device type (fan|light|door|thermostat|conditioner)
%%
%% RETURNS:      - CandidateState#devtypecfg -> The candidate state for the 'dev_statem'
%%

%% FAN
%% ---
simulate_activity(State,fan) ->

 % Generate a random candidate value for the 'onoff' trait, which is affected by:
 %  - Whether the current system hour is classified as "hot" (10-19) or "cold" (20-9) 
 OnOff = simulate_fan_onoff_trait(),

 % Generate a random candidate value for the 'fanspeed' trait, which is affected by:
 %  - Whether in the candidate state the fan will be on or off
 %  - Whether the current system hour is classified as "hot" (10-19) or "cold" (20-9) 
 %  - The current fanspeed, with the new one being limited to its value +- 30%
 FanSpeed = simulate_fan_fanspeed_trait(new_value(State#fancfg.onoff,OnOff), State#fancfg.fanspeed),
 
 % Return the candidate state
 #fancfg{onoff = OnOff, fanspeed = FanSpeed};
 
%% LIGHT
%% -----
simulate_activity(State,light) ->

 % Generate a random candidate value for the 'onoff' trait, which is affected by:
 %  - Whether the current system hour is classified as "bright" (8-18) or "dark" (19-7)
 OnOff = simulate_light_onoff_trait(),
 
 % Generate a random candidate value for the 'brightness' trait, which is affected by:
 %  - Whether in the candidate state the light is on or off
 %  - Whether the current system hour is classified as "bright" (8-18) or "dark" (19-7)
 %  - The current brightness, with the new one being limited to its value +- 30%
 Brightness = simulate_light_brightness_trait(new_value(State#lightcfg.onoff,OnOff), State#lightcfg.brightness),
 
 % Generate a random candidate value for the 'colorsetting' trait, which is affected by:
 %  - Whether in the candidate state the light is on or off
 ColorSetting = simulate_light_colorsetting_trait(new_value(State#lightcfg.onoff,OnOff)),
 
 % Return the candidate state
 #lightcfg{onoff = OnOff, brightness = Brightness, colorsetting = ColorSetting};
 
%% DOOR
%% ----
simulate_activity(State,door) ->

 % Generate a random candidate for the 'openclose' and 'lockunlock' traits, which are affected by:
 %  - Their current values, so to switch at most one state in the door's state machine cycle ({open,unlock} <-> {close,unlock} <-> {close,lock})
 %  - Whether the current system hour is classified as "Movement" (6-9,12-14,18-21) or "Working" (22-5,10-11,15-17)
 {OpenClose,LockUnlock} = simulate_door_traits(State),
 
 % Return the candidate state
 #doorcfg{openclose = OpenClose, lockunlock = LockUnlock};

%% THERMOSTAT
%% ----------
simulate_activity(State,thermostat) ->

 % Generate a random candidate value for the 'onoff' trait, which is affected by:
 %  - Whether the thermostat is currently on or off
 %  - Whether the thermostat is currently at the equilibrium temperature
 OnOff = simulate_ambient_dev_onoff_trait(State#thermocfg.onoff, State#thermocfg.temp_target - State#thermocfg.temp_current),
 
 % Generate a random candidate value for the 'temp_target' trait, which is affected by:
 %  - Whether in the candidate state the thermostat is on or off
 %  - Whether the thermostat is currently at the equilibrium temperature
 TempTarget = simulate_ambient_dev_target_temp_trait(new_value(State#thermocfg.onoff,OnOff),State#thermocfg.temp_target - State#thermocfg.temp_current),
 
 % Return the candidate state
 %
 % NOTE: The 'temp_current' trait must not be simulated since it evolves asynchronously via the "Ambient Temperature Update Timer" 
 #thermocfg{onoff = OnOff, temp_target = TempTarget, temp_current = '$keep'};

%% CONDITIONER
%% -----------
simulate_activity(State,conditioner) ->

 % Generate a random candidate value for the 'onoff' trait, which is affected by:
 %  - Whether the conditioner is currently on or off
 %  - Whether the conditioner is currently at the equilibrium temperature
 OnOff = simulate_ambient_dev_onoff_trait(State#condcfg.onoff, State#condcfg.temp_target - State#condcfg.temp_current),
 
 % Generate a random candidate value for the 'temp_target' trait, which is affected by:
 %  - Whether in the candidate state the conditioner is on or off
 %  - Whether the conditioner is currently at the equilibrium temperature
 TempTarget = simulate_ambient_dev_target_temp_trait(new_value(State#condcfg.onoff,OnOff),State#condcfg.temp_target - State#condcfg.temp_current),
 
 % Generate a random candidate value for the 'fanspeed' trait, which is affected by:
 %  - Whether in the candidate state the conditioner is on or off
 %  - The absolute distance between the candidate equilibrium and the current temperature
 FanSpeed = simulate_conditioner_fanspeed_trait(new_value(State#condcfg.onoff,OnOff),abs(new_value(State#condcfg.temp_target,TempTarget) - State#condcfg.temp_current)),
 
 % Return the candidate state
 %
 % NOTE: The 'temp_current' trait must not be simulated since it evolves asynchronously via the "Ambient Temperature Update Timer" 
 #condcfg{onoff = OnOff, temp_target = TempTarget, temp_current = '$keep', fanspeed = FanSpeed}.
  

%% Returns the new value of a simulated trait obtained by merging its current with its candidate value (simulate_activity(State,_) helper function)
new_value(CurrValue,'$keep') ->
 CurrValue; 
new_value(_,CandValue) ->
 CandValue.

     
%% DESCRIPTION:  Returns the time in "ms" after which schedule a new instance of the Simulated Activity Timer
%%
%% ARGUMENTS:    none (the mean and variance of the normal distribution used for generating
%%                    the time after which schedule a new instance of the Simulated Activity
%%                    Timer are defined via the "?Sim_mean" and "?Sim_var" macros)
%%
%% RETURNS:      - Next_sim_time_ms -> The time in "ms" after which schedule a new instance 
%%                                     of the Simulated Activity Timer (lower-capped to 200ms)
%%
next_sim_time() ->

 Res = max(200,trunc(rand:normal(?Sim_mean,?Sim_var))),
 
 % Logging purposes
 % io:format("Next sim_time: ~wms~n",[Res]),
 
 Res.

 
%% -------------------------------------------- SIMULATED FAN TRAITS HELPER FUNCTIONS -------------------------------------------- %%  
 
%% Simulates the 'onoff' trait of a 'fan' device (simulate_activity(State,fan) helper function)
simulate_fan_onoff_trait() ->

 % Retrieve the current system hour (0-23)
 {Hour,_,_} = erlang:time(),
 
 % Generate a uniformly distributed random number in the interval 0.0 <= Rand < 1.0
 Rand = rand:uniform(),
 
 % Logging purposes
 % io:format("fan 'onoff' trait Rand = ~w~n",[Rand]),
 
 % Generate a random candidate value for the 'onoff' trait, which is affected by:
 %  - Whether the current system hour is classified as "hot" (10-19) or "cold" (20-9) 
 if
 
  % ------------ "Hot" hours (10-19) ------------ %
  Hour >= 10 andalso Hour =< 19  ->
  
   % 40% on, 35% keep, 25% off
   if 
    Rand =< 0.4 ->
	 on;
	Rand =< 0.75 ->
	 '$keep';
	Rand < 1 ->
	 off
   end;
   
  % ------------ "Cold" hours (20-9) ------------ %
  Hour < 10 orelse Hour > 19 ->

   % 40% off, 35% keep, 25% on
   if 
    Rand =< 0.4 ->
	 off;
	Rand =< 0.75 ->
	 '$keep';
	Rand < 1 ->
	 on
   end
 end.


%% Simulates the 'fanspeed' trait of a 'fan' device (simulate_activity(State,fan) helper function)
simulate_fan_fanspeed_trait(off,_) ->

 % If in the candidate state the fan is off, it has no sense to change its 'fanspeed' trait
 '$keep';
 
simulate_fan_fanspeed_trait(on,CurrFanSpeed) ->

 % Retrieve the current system hour (0-23)
 {Hour,_,_} = erlang:time(),
 
 % Generate a uniformly distributed random number in the interval 0.0 <= Rand < 1.0
 Rand = rand:uniform(),
 
 % Logging purposes
 % io:format("fan 'fanspeed' trait Rand = ~w~n",[Rand]),
 
 % Generate a random candidate value for the 'fanspeed' trait, which is affected by:
 %  - Whether the current system hour is classified as "hot" (10-19) or "cold" (20-9) 
 %  - The current fanspeed, with the new one being limited to its value +- 30% (which is
 %    uniformily distributed between 10%, 20% and 30% and considers the [10-100] value cappings)
 if
 
  % ------------ "Hot" hours (10-19) ------------ %
  Hour >= 10 andalso Hour =< 19  ->
  
   % 40% increase, 30% keep, 30% decrease
   if 
    Rand =< 0.4 ->
	 min(100,CurrFanSpeed + (rand:uniform(3)*10));
	Rand =< 0.7 ->
	 '$keep';
	Rand < 1 ->
	 max(10,CurrFanSpeed - (rand:uniform(3)*10))
   end;
   
  % ------------ "Cold" hours (20-9) ------------ %
  Hour < 10 orelse Hour > 19 ->

   % 40% decrease, 30% keep, 30% increase
   if 
    Rand =< 0.4 ->
	 max(10,CurrFanSpeed - (rand:uniform(3)*10));
	Rand =< 0.7 ->
	 '$keep';
	Rand < 1 ->
	 min(100,CurrFanSpeed + (rand:uniform(3)*10))
   end
 end.


%% ------------------------------------------- SIMULATED LIGHT TRAITS HELPER FUNCTIONS ------------------------------------------- %% 

%% Simulates the 'onoff' trait of a 'light' device (simulate_activity(State,light) helper function)
simulate_light_onoff_trait() ->

 % Retrieve the current system hour (0-23)
 {Hour,_,_} = erlang:time(),
 
 % Generate a uniformly distributed random number in the interval 0.0 <= Rand < 1.0
 Rand = rand:uniform(),
 
 % Logging purposes
 % io:format("light 'onoff' trait Rand = ~w~n",[Rand]),
 
 % Generate a random candidate value for the 'onoff' trait, which is affected by:
 %  - Whether the current system hour is classified as "bright" (8-18) or "dark" (19-7)
 if

  % ----------- "Bright" hours (8-18) ----------- %
  Hour >= 8 andalso Hour =< 18  ->
  
   % 40% off, 40% keep, 20% on
   if 
    Rand =< 0.4 ->
	 off;
	Rand =< 0.8 ->
	 '$keep';
	Rand < 1 ->
	 on
   end;
  
  % ------------ "Dark" hours (19-7) ------------ %
  Hour < 8 orelse Hour > 18 ->

   % 40% on, 40% keep, 20% off
   if 
    Rand =< 0.4 ->
	 on;
	Rand =< 0.8 ->
	 '$keep';
	Rand < 1 ->
	 off
   end
 end.
 

%% Simulates the 'brightness' trait of a 'light' device (simulate_activity(State,light) helper function)
simulate_light_brightness_trait(off,_) ->

 % If in the candidate state the light is off, it has no sense to change its 'brightness' trait
 '$keep';
 
simulate_light_brightness_trait(on,CurrBrightness) -> 

 % Retrieve the current system hour (0-23)
 {Hour,_,_} = erlang:time(),
 
 % Generate a uniformly distributed random number in the interval 0.0 <= Rand < 1.0
 Rand = rand:uniform(),
 
 % Logging purposes
 % io:format("light 'brightness' trait Rand = ~w~n",[Rand]),
 
 % Generate a random candidate value for the 'brightness' trait, which is affected by:
 %  - Whether the current system hour is classified as "bright" (8-18) or "dark" (19-7)
 %  - The current brightness, with the new one being limited to its value +- 30% (which is
 %    uniformily distributed between 10%, 20% and 30% and considers the [10-100] value cappings)
 if
 
  % ----------- "Bright" hours (8-18) ----------- %
  Hour >= 8 andalso Hour =< 18  ->
  
   % 40% decrease, 40% keep, 20% increase
   if 
    Rand =< 0.4 ->
	 max(10,CurrBrightness - (rand:uniform(3)*10));
	Rand =< 0.8 ->
	 '$keep';
	Rand < 1 ->
	 min(100,CurrBrightness + (rand:uniform(3)*10))
   end;
   
  % ------------ "Dark" hours (19-7) ------------ %
  Hour < 8 orelse Hour > 18 ->

   % 40% increase, 40% keep, 20% decrease
   if 
    Rand =< 0.4 ->
	 min(100,CurrBrightness + (rand:uniform(3)*10));
	Rand =< 0.8 ->
	 '$keep';
	Rand < 1 ->
	 max(10,CurrBrightness - (rand:uniform(3)*10))
   end
 end.


%% Simulates the 'colorsetting' trait of a 'light' device (simulate_activity(State,light) helper function) 
simulate_light_colorsetting_trait(off) ->

 % If in the candidate state the light is off, it has no sense to change its 'colorsetting' trait
 '$keep';
 
simulate_light_colorsetting_trait(on) ->

 % Generate a uniformly distributed random number in the interval 0.0 <= Rand < 1.0
 Rand = rand:uniform(),
 
 % Logging purposes
 % io:format("light 'colorsetting' trait Rand = ~w~n",[Rand]),
 
 % Generate a random candidate value for the 'colorsetting' trait
 if

  % 80% -> Keep the current color 
  Rand =< 0.8 ->
   '$keep';
   
  % 20% -> Randomly select one from a list of 10 colors
  Rand < 1 ->
   case rand:uniform(10) of
    1 ->  "#FFFFFF";           % White
	2 ->  "#FF0000";		   % Red
	3 ->  "#FFFF00";           % Yellow
	4 ->  "#0000FF";           % Blue
	5 ->  "#800080";           % Purple
	6 ->  "#008000";           % Green
	7 ->  "#FFA500";		   % Orange
	8 ->  "#A52A2A";		   % Brown
	9 ->  "#FFC0CB";           % Pink
	10 -> "#008080"			   % Teal
   end
 end. 
 
 
%% ------------------------------------------- SIMULATED DOOR TRAITS HELPER FUNCTIONS ------------------------------------------- %%
 
%% Simulates the 'openclose' and 'lockunlock' traits of a 'door' device (simulate_activity(State,door) helper function) 
simulate_door_traits(State) ->

 % Retrieve the current system hour (0-23)
 {Hour,_,_} = erlang:time(),
 
 % Generate a uniformly distributed random number in the interval 0.0 <= Rand < 1.0
 Rand = rand:uniform(),
 
 % Logging purposes
 % io:format("door traits Rand = ~w~n",[Rand]),
 
 % Generate a random candidate for the 'openclose' and 'lockunlock' traits, which are affected by:
 %  - Their current values, so to switch at most one state in the door's state machine cycle ({open,unlock} <-> {close,unlock} <-> {close,lock})
 %  - Whether the current system hour is classified as "Movement" (6-9,12-14,18-21) or "Working" (22-5,10-11,15-17)
 if
 
  % ----- "Movement" hours (6-9,12-14,18-21) ----- %
  (Hour >= 6 andalso Hour =< 9) orelse
  (Hour >= 12 andalso Hour =<14) orelse
  (Hour >= 18 andalso Hour =<21) ->
  
   % Depending on the current door state
   case {State#doorcfg.openclose,State#doorcfg.lockunlock} of

    {open,unlock} ->
   
     % 50% keep, 50% {close,unlock} 
     if
 	  Rand =< 0.5 ->
	   {open,unlock};
	  Rand < 1 ->
	   {close,unlock}
	 end;
	
    {close,unlock} ->
   
      % 40% keep, 40% {open,unlock}, 20% {close,lock}
      if
	   Rand =< 0.4 ->
	    {close,unlock};
	   Rand =< 0.8 ->
	    {open,unlock};
	   Rand < 1 ->
	    {close,lock}
	  end;

    {close,lock} ->
   
     % 80% keep, 20% {close,unlock}
     if
	  Rand =< 0.8 ->
	   {close,lock};
	  Rand < 1 ->
	   {close,unlock}
	 end
   end;	
	    
  % ----- "Working" hours (22-5,10-11,15-17) ----- %
  true ->
  
   % Depending on the current door state
   case {State#doorcfg.openclose,State#doorcfg.lockunlock} of

    {open,unlock} ->
   
     % 20% keep, 80% {close,unlock}
     if
 	  Rand =< 0.2 ->
	   {open,unlock};
	  Rand < 1 ->
	   {close,unlock}
	 end;
	
    {close,unlock} ->
   
      % 30% keep, 30% {open,unlock}, 40% {close,lock}
      if
	   Rand =< 0.3 ->
	    {close,unlock};
	   Rand =< 0.3 ->
	    {open,unlock};
	   Rand < 1 ->
	    {close,lock}
	  end;

    {close,lock} ->
   
     % 80% keep, 20% {close,unlock}
     if
	  Rand =< 0.8 ->
	   {close,lock};
	  Rand < 1 ->
	   {close,unlock}
	 end
   end
 end. 
 
 
 
%% ------------------------ SIMULATED AMBIENT DEVICES TRAITS HELPER FUNCTIONS (Thermostat + Conditioner) ------------------------ %% 
 
%% Simulates the 'onoff' trait of a 'thermostat' or a 'conditioner' device
%% (simulate_activity(State,thermostat),simulate_activity(State,conditioner), helper function)
simulate_ambient_dev_onoff_trait(off,_) ->

 % If in the candidate state the device is off, as a simplification the 'onoff' trait is
 % simulated as if the device were a FAN, and so depending on whether the current system
 % hour is classified as "hot" (10-19) or "cold" (20-9) 
 simulate_fan_onoff_trait(); 
 
simulate_ambient_dev_onoff_trait(on,0) ->
   
 % Generate a uniformly distributed random number in the interval 0.0 <= Rand < 1.0
 Rand = rand:uniform(),
   
 % Generate a random candidate value for the 'onoff' trait considering that the device
 % is on and that the ambient temperature has reached is equilibrium point, where:
 %  - 40% -> Turn the device off
 %  - 60% -> Keep the device on
 if
  Rand =< 0.4 ->
   off;
  Rand < 1 ->
   on
 end;

simulate_ambient_dev_onoff_trait(on,_) ->

 % If the device is on and the ambient temperature has
 % not yet reached its equilibrium point, keep it on
 on.


%% Simulates the 'temp_target' trait of a 'thermostat' or a 'conditioner' device
%% (simulate_activity(State,thermostat),simulate_activity(State,conditioner), helper function) 
simulate_ambient_dev_target_temp_trait(off,_) ->

 % If in the candidate state the device is off, it has no sense to change its 'temp_target' trait
 '$keep';

simulate_ambient_dev_target_temp_trait(on,0) ->

 % Generate a uniformly distributed random number in the interval 0.0 <= Rand < 1.0
 Rand = rand:uniform(),
 
 % Generate a new random candidate value for the 'temp_target' given that the
 % device has reached the equilibrium ('temp_target' = 'temp_current') as follows:
 %
 % 70% -> Keep
 % 30% -> Set to a random "comfortable" temperature obtained via a normal distribution
 %        centered on 21 degrees (and considering the [0-50] value cappings)
 if
  Rand =< 0.7 ->
   '$keep';
  Rand < 1 ->
   min(50,max(0,trunc(rand:normal(21,21))))
 end;
 
simulate_ambient_dev_target_temp_trait(on,_) ->

 % If in the candidate state the device has not yet reached
 % its 'temp_target' equilibrium point, keep it as it is
 '$keep'.

 
%% Simulates the 'fanspeed' trait of a 'conditioner' device (simulate_activity(State,conditioner) helper function) 
simulate_conditioner_fanspeed_trait(off,_) ->

 % If in the candidate state the device is off, it has no sense to change its 'fanspeed' trait
 '$keep';
 
simulate_conditioner_fanspeed_trait(on,TempDistEq) -> 
 
 % Generate a random candidate value for the 'fanspeed' trait the higher the further the distance between the
 % equilibrium and the current temperature (conceptually contributing to the reason why ambient temperature updates
 % are more frequent the further such distance).
 % More precisely the candidate 'fanspeed' trait is selected as the distance from equilibrium temperature * 10 with an
 % added noise uniformily distributed in the range (-20,10,0,+10,+20) (and considering the [10-100] value cappings) 
 max(10,min(100,(TempDistEq*10) + (rand:uniform(5)*10) - 30)).


%% ========================================= AMBIENT TEMPERATURE UPDATE HELPER FUNCTIONS ========================================= %% 

%% DESCRIPTION:  Generates a new random value for the 'temp_current' trait of an ambient device,
%%               which may drift from its current value of at most 1 degree (limit whose purpose
%%               is to allow the 'dev_statem' to report every ambient temperature update)
%%
%% ARGUMENTS:    - TempCurrent: The current value of the device 'temp_current' trait
%%               - TempDiff:    The difference between the device 'temp_current' and the equilibrium point
%%
%% RETURNS:      New_TempDiff -> The new value of the device's 'temp_current' trait,
%%                               (which differs from the old of at most 1 degree)
%% 
update_tempcurrent_trait(TempCurrent,TempDiff) ->
 
 % Generate a uniformly distributed random number in the interval 0.0 <= Rand < 1.0
 Rand = rand:uniform(),
 
 % Logging purposes
 % io:format("'temp_current' update Rand = ~w (TempCurrent = ~w, TempDiff = ~w)~n",[Rand,TempCurrent,TempDiff]),

 % Generate a new random value for the 'temp_current' trait drifting at most 1 degree
 % from the current and that is affected by its difference from the equilibrium point
 if 
 
  % -------- DistFromEq >= 10 -------- %
  abs(TempDiff) >= 10 ->
  
   % Drift towards the equilibrium (no randomicity)
   TempCurrent + sign(TempDiff); 
  
  % ------ 6 =< DistFromEq < 10 ------ %
  abs(TempDiff) >= 6 ->
  
   % 80% drift towards equilibrium, 20% keep
   if
    Rand =< 0.8 ->
	 TempCurrent + sign(TempDiff);
	Rand < 1 ->
	 TempCurrent
   end;
  
  % ------ 4 =< DistFromEq < 6 ------ %  
  abs(TempDiff) >= 4 ->
  
   % 60% drift towards equilibrium, 40% keep
   if
    Rand =< 0.6 ->
	 TempCurrent + sign(TempDiff);
	Rand < 1 ->
	 TempCurrent
   end;  
 
  % ------ 2 =< DistFromEq < 4 ------ %
  abs(TempDiff) >= 2 ->
  
   % 40% drift towards equilibrium, 60% keep
   if
    Rand =< 0.4 ->
	 TempCurrent + sign(TempDiff);
	Rand < 1 ->
	 TempCurrent
   end; 
   
  % -------- DistFromEq =:= 1 -------- %
  abs(TempDiff) =:= 1 ->
  
   % 20% drift towards equilibrium, 80% keep
   if
    Rand =< 0.2 ->
	 TempCurrent + sign(TempDiff);
	Rand < 1 ->
	 TempCurrent
   end;    
   
  % --------- AT EQUILIBRIUM --------- % 
  TempDiff =:= 0 ->
  
   % Retrieve the native equilibrium temperature assocaited with the current system hour
   TempEqNative = get_native_equilibrium(),
   if
   
    % If the current temperature differs from the native equilibrium temperature (which means
	% that the device is 'on' and its 'temp_current' value is equal to its 'temp_target' value)
	% consider a chance for the temperature of drifting towards the native equilibrium
    TempCurrent =/= TempEqNative ->
	
     % 20% drift towards native equilibrium, 80% keep equilibrium
     if
      Rand =< 0.2 ->
	   TempCurrent + sign(TempEqNative - TempCurrent);
	  Rand < 1 ->
	   TempCurrent
     end;
	
	% If instead the device is at its native equilibrium temperature (which means that is 'off'
	% or its 'temp_target' value is equal to the native equilibrium), consider a chance for the
	% temperature of drifting in either direction	
	true ->
	
	 % 10% +1 from equilibrium, 10% -1 from equilibrium, 80% keep native equilibrium
	 if
	  Rand =< 0.1 ->
	   TempCurrent + 1;
	  Rand =< 0.2 ->
       TempCurrent - 1;
      Rand < 1 ->
       TempCurrent
     end	   
   end
 end.
 
 
%% DESCRIPTION:  Returns the time in "ms" after which schedule a new instance of the
%%               Ambient Temperature Update Timer in a 'thermostat' or 'conditioner' device
%%
%% ARGUMENTS:    - State: The current state of the 'thermostat' or 'conditioner' device
%%
%% RETURNS:      - Next_amb_time_ms -> The time in "ms" after which schedule a new instance of the
%%                                     Ambient Temperature Update Timer (lower-capped to 3000ms)
%%
%% NOTE:         The mean and variance of the normal distribution used for generating the
%%               time after which schedule a new instance of the Ambient Temperature Update
%%               Timer are defined via the "?Amb_base_mean" and "?Amb_base_var" macros)  
%%
next_amb_time(State) ->

  % Retrive the distance between the equilibrium and the current temperature in the device
  TempDistEq = abs(temp_diff_from_eq(State)),

  % Return the time in "ms" after which schedule a new instance of the Ambient Temperature Update Timer as
  % a normally distributed random value, whose base mean and variance are reduced by two factors the greater
  % the higher the distance between the equilibrium and the current temperature in the device (modelling the
  % fact that the higher the distance from equilibrium, the faster the evolution of the ambient temperature)
  Res = max(3000,trunc(rand:normal(?Amb_base_mean-(1500 * TempDistEq),?Amb_base_var/max(1,TempDistEq)))),
  
  % Logging purposes
  % io:format("Next_amb_time: ~wms~n",[Res]),  
  Res. 
 
               
%% DESCRIPTION:  Returns the difference between the current and the equilibrium
%%               temperature in an ambient temperature device, with the latter consisting:
%%                - If the device is 'on', in its 'temp_current' trait
%%                - If the device is 'off', in the native equilibrium temperature
%%
%% ARGUMENTS:    - State: The State/Config of the ambient temperature device
%%
%% RETURNS:      - TempDiff -> The difference between the equilibrium and the current
%%                             temperature in the device (an integer >,< or =:= 0)
%%
% Thermostat ON (TempEq = 'temp_target')
temp_diff_from_eq(State) when is_record(State, thermocfg), State#thermocfg.onoff =:= on ->
 State#thermocfg.temp_target - State#thermocfg.temp_current;
 
% Thermostat OFF (TempEq = native equilibrium)
temp_diff_from_eq(State) when is_record(State, thermocfg), State#thermocfg.onoff =:= off ->
 get_native_equilibrium() - State#thermocfg.temp_current;  

% Conditioner ON (TempEq = 'temp_target')
temp_diff_from_eq(State) when is_record(State, condcfg), State#condcfg.onoff =:= on ->
 State#condcfg.temp_target - State#condcfg.temp_current;
 
% Conditioner OFF (TempEq = native equilibrium)
temp_diff_from_eq(State) when is_record(State, condcfg), State#condcfg.onoff =:= off ->
 get_native_equilibrium() - State#condcfg.temp_current. 


%% DESCRIPTION:  Returns the ambient native equilibrium temperature associated with
%%               the current system hour (where a temperate climate is considered)
%%
%% ARGUMENTS:    none
%%
%% RETURNS:      - TempEqNative -> The ambient native equilibrium temperature
%%                                 associated with the current system hour
%% 
get_native_equilibrium() ->
 
 % Retrieve the current system hour (0-23)
 {Hour,_,_} = erlang:time(),
 
 % Return the ambient native equilibrium temperature
 % associated with the current system hour via a LUT
 %
 %        Hour:   0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23
 element(Hour+1,{16,15,14,12,10,11,12,13,14,15,17,19,22,24,25,26,25,24,23,22,21,20,19,17}).


%% DESCRIPTION:  A simple sign function
%%
%% ARGUMENTS:    - X: A number
%%
%% RETURNS:      - sign(X) (1 if X =:= 0) 
%%
sign(X) when is_number(X), X >= 0 ->
 1; 
sign(X) when is_number(X), X < 0 ->
 -1.
 
 
%%====================================================================================================================================
%%                                                         START FUNCTION                                                        
%%==================================================================================================================================== 

%% Called by its 'sup_jdev' supervisor during the JANET Device boot
start_link(Config,Type) ->
 gen_statem:start_link({local,?MODULE}, ?MODULE, {Config,Type}, []).  % The spawned process is also registered locally under the 'dev_statem' name
