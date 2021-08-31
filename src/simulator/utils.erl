%% This module offers a set of utility functions for the Janet Simulator application %%

-module(utils).

-export([is_valid_devtype/1,is_valid_devconfig/2,build_dev_config_wildcard/2,check_merge_devconfigs/3,  % Devices Utility Functions
         get_devtype_default_config/1,deprefix_dev_config/1,next_sim_time/2]). 
-export([resolve_nodetype_shorthand/1,prefix_node_id/2]).				                                % Nodes Utility Functions
-export([is_running/1,str_to_atom/1]).							                                        % Other Utility Functions

-include("devtypes_configurations_definitions.hrl").  % Janet Device Configuration Records Definitions

%%====================================================================================================================================
%%                                                    DEVICES UTILITY FUNCTIONS
%%====================================================================================================================================

%% DESCRIPTION:  Checks if a passed device type is valid or not
%%
%% ARGUMENTS:    - DevType: The device type to check the validity (an atom)
%%
%% RETURNS:      - true  -> The DevType is valid
%%               - false -> The DevType is invalid
%%

% Valid DevType
is_valid_devtype(DevType) when 

 DevType =:= fan orelse 
 DevType =:= light orelse
 DevType =:= door orelse
 DevType =:= thermostat orelse
 DevType =:= heater ->

 true;

% Invalid Devtype
is_valid_devtype(_) ->
 false.


%% DESCRIPTION:  Checks a device configuration to be valid
%%
%% ARGUMENTS:    - Config: The device's configuration
%%               - DevType: The device's type (an atom)  
%%
%% RETURNS:      - ok -> The device configuration is valid
%% 
%% THROWS:       - {error,invalid_devconfig} -> The device configuration is invalid
%%               - {error,unknown_devtype}   -> The device type is invalid

%% ------------------- Valid Fan Configuration ------------------- %%
is_valid_devconfig(Config,fan) when 

 % 'onoff' trait ('on'|'off')
 Config#fancfg.onoff =:= on orelse Config#fancfg.onoff =:= off,

 % 'fanspeed' trait	(0 < fanspeed <= 100)					     
 is_number(Config#fancfg.fanspeed),
 Config#fancfg.fanspeed > 0, Config#fancfg.fanspeed =< 100 ->
 
 % Valid fan configuration
 ok;

%% ------------------ Valid Light Configuration ------------------ %%
is_valid_devconfig(Config,light) when 

 % 'onoff' trait ('on'|'off')
 Config#lightcfg.onoff =:= on orelse Config#lightcfg.onoff =:= off,
 
 % 'brightness' trait (0 < brightness <= 100)
 is_number(Config#lightcfg.brightness),
 Config#lightcfg.brightness > 0, Config#lightcfg.brightness =< 100 ->
 
 % Valid light configuration (NOTE: The 'colorsetting' trait is NOT checked)
 ok;

%% ------------------- Valid Door Configuration ------------------- %%
is_valid_devconfig(Config,door) when 

 % 'openclose' + 'lockunlock' traits ({'open' && 'unlock'} | {'close' && ('lock' || 'unlock'))		  
 (Config#doorcfg.openclose =:= open andalso Config#doorcfg.lockunlock =:= unlock) orelse 
 (Config#doorcfg.openclose =:= close andalso (Config#doorcfg.lockunlock =:= unlock orelse Config#doorcfg.lockunlock =:= lock)) ->

 % Valid door configuration
 ok;									 

%% ---------------- Valid Thermostat Configuration ---------------- %%
is_valid_devconfig(Config,thermostat) when 

 % 'onoff' trait ('on'|'off')
 Config#thermocfg.onoff =:= on orelse Config#thermocfg.onoff =:= off,
 									        
 % 'temp_target' trait (0 <= temp_target <= 50)						
 is_number(Config#thermocfg.temp_target),
 Config#thermocfg.temp_target >= 0, Config#thermocfg.temp_target =< 50,
 
 % 'temp_current' trait (NOTE: the interval range is NOT checked)
 is_number(Config#thermocfg.temp_current) ->
 
 % Valid thermostat configuration
 ok;

%% ------------------ Valid Heater Configuration ------------------ %% 
is_valid_devconfig(Config,heater) when 

 % 'onoff' trait ('on'|'off')
 Config#heatercfg.onoff =:= on orelse Config#heatercfg.onoff =:= off,

 % 'fanspeed' trait	(0 < fanspeed <= 100)
 is_number(Config#heatercfg.fanspeed),
 Config#heatercfg.fanspeed > 0, Config#heatercfg.fanspeed =< 100,
 
 % 'temp_target' trait (0 <= temp_target <= 50)
 is_number(Config#heatercfg.temp_target),
 Config#heatercfg.temp_target >= 0, Config#heatercfg.temp_target =< 50,
                    
 % 'temp_current' trait (NOTE: the interval range is NOT checked)                   
 is_number(Config#heatercfg.temp_current) ->

 % Valid heater configuration
 ok;
					
%% ----------- Valid Device Type, Invalid Configuration ----------- %%
is_valid_devconfig(_,ValidDev) when
 
 ValidDev =:= fan orelse ValidDev =:= light orelse ValidDev =:= door orelse 
 ValidDev =:= thermostat orelse ValidDev =:= heater -> 
 
 throw({error,invalid_devconfig});

%% --------------------- Invalid Device Type --------------------- %%
is_valid_devconfig(_,_) ->
 throw({error,unknown_devtype}).


%% DESCRIPTION:  Build and returns a device configuration record of the appropriate
%%               #devtypecfg type initialized with its configuration arguments
%%
%% ARGUMENTS:    - {Config}: A tuple of type-specific variables representing a
%%                           device's configuration, with the following  being
%%                           allowed (see the "devtypes_configurations_definitions.hrl"
%%                           header file for more information):
%%                            - fan:        {OnOff,FanSpeed}
%%                            - light:      {OnOff,Brightness,ColorSetting}
%%                            - door:       {OpenClose,LockUnlock}
%%                            - thermostat: {OnOff,TempTarget,TempCurrent}
%%                            - heater:     {OnOff,FanSpeed,TempTarget,TempCurrent}
%%                           The '$keep' wildcard can also be used in any field for the
%%                           purposes of preserving its value when applying this
%%                           configuration with another of the same #cfgdevtype
%%                           (namely, when the configuration returned by this function
%%                           is used for updating a device's configuration)
%%                           
%%               - Type:     The device type associated with the configuration to build
%%
%% RETURNS:      - Config#devtypecfg -> The device configuration record
%%                                      associated with the {Config} arguments
%% 
%% THROWS:       - {error,invalid_devconfig} -> The passed {Config} arguments represent an
%%                                              invalid configuration for the device Type
%%               - {error,unknown_devtype}   -> The device type is invalid

%% ------------------- Valid Fan Configuration ------------------- %%
build_dev_config_wildcard({OnOff,FanSpeed},fan) when 

 % 'onoff' trait ('on'|'off')
 OnOff =:= '$keep' orelse (OnOff =:= on orelse OnOff =:= off),

 % 'fanspeed' trait	(0 < fanspeed <= 100) 
 FanSpeed =:= '$keep' orelse (is_number(FanSpeed) andalso FanSpeed >0 andalso FanSpeed =< 100) ->

 % Build and return the valid fan configuration
 #fancfg{onoff = OnOff, fanspeed = FanSpeed};

%% --------------- Build Valid Light Configuration --------------- %%
build_dev_config_wildcard({OnOff,Brightness,ColorSetting},light) when 

 % 'onoff' trait ('on'|'off')
 OnOff =:= '$keep' orelse (OnOff =:= on orelse OnOff =:= off),
 
 % 'brightness' trait (0 < brightness <= 100)
 Brightness =:= '$keep' orelse (is_number(Brightness) andalso Brightness >0 andalso Brightness =< 100) ->

 % Build and return the valid light configuration
 %
 % NOTE: The 'colorsetting' trait is NOT checked
 #lightcfg{onoff = OnOff, brightness = Brightness, colorsetting = ColorSetting};

%% ---------------- Build Valid Door Configuration ---------------- %%
build_dev_config_wildcard({OpenClose,LockUnlock},door) when 

 % 'openclose' + 'lockunlock' traits ({'open' && 'unlock'} | {'close' && ('lock' || 'unlock')) 								  
 (OpenClose =:= open orelse OpenClose =:= close orelse OpenClose =:= '$keep')     andalso
 (LockUnlock =:= lock orelse LockUnlock =:= unlock orelse LockUnlock =:= '$keep') andalso
 not (OpenClose =:= open andalso LockUnlock =:= lock) ->

 % Build and return the valid door configuration
 #doorcfg{openclose = OpenClose, lockunlock = LockUnlock};

%% ------------- Build Valid Thermostat Configuration ------------- %%
build_dev_config_wildcard({OnOff,TempTarget,TempCurrent},thermostat) when 

 % 'onoff' trait ('on'|'off')
 OnOff =:= '$keep' orelse (OnOff =:= on orelse OnOff =:= off),
 
 % 'temp_target' trait (0 <= temp_target <= 50)	
 TempTarget =:= '$keep' orelse (is_number(TempTarget) andalso TempTarget >=0 andalso TempTarget =< 50),

 % 'temp_current' trait (NOTE: the interval range is NOT checked)
 TempCurrent =:= '$keep' orelse is_number(TempCurrent) ->

 % Build and return the valid thermostat configuration
 #thermocfg{onoff = OnOff, temp_target = TempTarget, temp_current = TempCurrent};

%% --------------- Build Valid Heater Configuration --------------- %%
build_dev_config_wildcard({OnOff,FanSpeed,TempTarget,TempCurrent},heater) when 

 % 'onoff' trait ('on'|'off')
 OnOff =:= '$keep' orelse (OnOff =:= on orelse OnOff =:= off),
 
 % 'fanspeed' trait	(0 < fanspeed <= 100) 
 FanSpeed =:= '$keep' orelse (is_number(FanSpeed) andalso FanSpeed >0 andalso FanSpeed =< 100),
 
 % 'temp_target' trait (0 <= temp_target <= 50)	
 TempTarget =:= '$keep' orelse (is_number(TempTarget) andalso TempTarget >=0 andalso TempTarget =< 50),

 % 'temp_current' trait (NOTE: the interval range is NOT checked)
 TempCurrent =:= '$keep' orelse is_number(TempCurrent) ->

 % Build and return the valid heater configuration
 #heatercfg{onoff = OnOff, fanspeed = FanSpeed, temp_target = TempTarget, temp_current = TempCurrent};

%% ----------- Valid Device Type, Invalid Configuration ----------- %%
build_dev_config_wildcard(_,ValidDev) when
 
 ValidDev =:= fan orelse ValidDev =:= light orelse ValidDev =:= door orelse 
 ValidDev =:= thermostat orelse ValidDev =:= heater -> 
 
 throw({error,invalid_devconfig});

%% --------------------- Invalid Device Type --------------------- %%
build_dev_config_wildcard(_,_) ->
 throw({error,unknown_devtype}).
 
 
%% DESCRIPTION:  Merges a device current configuration with an updated configuration considering
%%               '$keep' wildcards in the latter, returning the resulting new device configuration
%%
%% ARGUMENTS:    - {CurrCfg}:    A tuple of type-specific variables representing the current device's configuration
%%               - {UpdatedCfg}: A tuple of type-specific variables representing the updated device's
%%                               configuration to be merged with the current considering '$keep' wildcards
%%               - Type:         The device's type (fan|light|door|thermostat|heater)
%%
%%               Please refer to the "devtypes_configurations_definitions.hrl"
%%               header file for the definitions of allowed device configurations
%%
%% RETURNS:      - {ok,NewCfg} -> The resulting valid configuration to be applied to the device
%% 
%% THROWS:       - {error,invalid_devconfig} -> The passed {Config} arguments represent an
%%                                              invalid configuration for the device Type
%%               - {error,unknown_devtype}   -> The device type is invalid
%%
check_merge_devconfigs(CurrCfg,UpdateCfg,Type) when element(1,CurrCfg) =:= element(1,UpdateCfg) ->
 
 % Build the new device configuration by retaining only the fields in "CurrCfg" whose corresponding field in "UpdateCfg" is set to '$keep'
 NewCfg = try list_to_tuple(lists:zipwith(fun(X,Y) -> if Y =:= '$keep' -> X; true -> Y end end, tuple_to_list(CurrCfg), tuple_to_list(UpdateCfg)))
 catch
  error:_ ->
  
  % An invalid configuration was passed
   throw({error,invalid_devconfig});
  
  exit:_ ->
  
   % An invalid configuration was passed
   throw({error,invalid_devconfig})
 end,
 
 % Validate and return the new configuration
 %
 % NOTE: If the new configuration is not valid or a invalid device Type
 %       was passed the is_valid_devconfig() function will raise a throw
 %
 {is_valid_devconfig(NewCfg,Type),NewCfg};
 
%% ----------- Valid Device Type, Invalid Configuration ----------- %% 
check_merge_devconfigs(_,_,ValidDev) when
 
 ValidDev =:= fan orelse ValidDev =:= light orelse ValidDev =:= door orelse 
 ValidDev =:= thermostat orelse ValidDev =:= heater -> 

 throw({error,invalid_devconfig});

%% --------------------- Invalid Device Type --------------------- %% 
check_merge_devconfigs(_,_,_) ->
 throw({error,unknown_devtype}).
 

%% DESCRIPTION:  Returns a device's default configuration according to its type
%%
%% ARGUMENTS:    - DevType: The device's type (an atom)
%%
%% RETURNS:      - Config#devtypecfg -> The device default configuration as a record of the appropriate type
%% 
%% THROWS:       - {error,unknown_devtype} -> The device's type is invalid
%%
% Fan
get_devtype_default_config(fan) ->
 #fancfg{onoff = off, fanspeed = 50};

% Light
get_devtype_default_config(light) ->
 #lightcfg{onoff = off, brightness = 50, colorsetting = "white"};

% Door
get_devtype_default_config(door) ->
 #doorcfg{openclose = close, lockunlock = unlock};

% Thermostat
get_devtype_default_config(thermostat) ->
 #thermocfg{onoff = off, temp_target = 21, temp_current = 21};

% Heater
get_devtype_default_config(heater) ->
 #heatercfg{onoff = off, fanspeed = 50, temp_target = 21, temp_current = 21};

% Unknown device
get_devtype_default_config(_) ->
 throw(unknown_devtype).


%% DESCRIPTION:  Returns a device's configuration without the first 
%%               "{devtypecfg," tuple element (prettier printing purposes)
%%
%% ARGUMENTS:    - Config: The device's configuration
%%
%% RETURNS:      - The device's configuration without the first "{devtypecfg," tuple element
%% 
%% THROWS:       - {error,unknown_devtype} -> The device's type is invalid
%%
deprefix_dev_config(Config) ->

 % Return the device's configuration without the first tuple element according to its type
 case element(1,Config) of
 
  % Fan
  fancfg ->
   {Config#fancfg.onoff,Config#fancfg.fanspeed};
  
  % Light
  lightcfg ->
   {Config#lightcfg.onoff,Config#lightcfg.brightness,Config#lightcfg.colorsetting};
   
  % Door
  doorcfg ->
   {Config#doorcfg.openclose,Config#doorcfg.lockunlock};
   
  % Thermostat
  thermocfg ->
   {Config#thermocfg.onoff,Config#thermocfg.temp_target,Config#thermocfg.temp_current};  

  % Heater
  heatercfg ->
   {Config#heatercfg.onoff,Config#heatercfg.fanspeed,Config#heatercfg.temp_target,Config#heatercfg.temp_current};
   
  % Unknown Devtype
  _ ->
   throw(unknown_devtype)
 end.
 
 
%%====================================================================================================================================
%%                                                     NODES UTILITY FUNCTIONS
%%====================================================================================================================================

%% DESCRIPTION:  Returns the node type associated to its argument, possibly considering shorthand forms
%%
%% ARGUMENTS:    - NodeTypeShorthand: A node type (controller or device), possibly in a shorthand form
%%
%% RETURNS:      - NodeType -> The node type atom associated to NodeTypeShorthand ('controller' or 'device')
%%
%% THROWS:       - {error,unknown_nodetype} -> If no node type could be associated with NodeTypeShorthand 
%%
resolve_nodetype_shorthand(NodeTypeShorthand) ->
 if
 
  % Controller node shorthands
  NodeTypeShorthand =:= controller orelse NodeTypeShorthand =:= ctr orelse NodeTypeShorthand =:= contr ->
   controller;
   
  % Device node shorthands
  NodeTypeShorthand =:= device orelse NodeTypeShorthand =:= dev ->
   device;
  
  % Unknown nodetype  
  true ->
   throw({error,unknown_nodetype})
 end.


%% DESCRIPTION:  Returns the ID of a controller or device node prefixed with its type (which other than
%%               for logging purposes corresponds to its manager's childID under of its 'sup_loc' supervisor)
%%
%% ARGUMENTS:    - NodeTypeShorthand: An atom indicating the type of node to be stopped, also taking
%%                                    into account shorthand forms, with the following being allowed:
%%                                     - controller,ctr,contr -> controller node
%%                                     - device, dev          -> device node
%%               - Node_id:           The ID of the node to be prefixed ('loc_id' for controller nodes
%%                                    and 'dev_id' for device nodes)
%%
%% RETURNS:      - PrefixedId -> The prefixed Node_id depending on the NodeType (e.g. "ctr-1","dev-5", etc.)
%%
prefix_node_id(controller,Loc_id) ->
 "ctr-" ++ integer_to_list(Loc_id);

prefix_node_id(device,Dev_id) ->
 "dev-" ++ integer_to_list(Dev_id);

prefix_node_id(NodeTypeShorthand,Node_id) ->

 % Determine the node type, also taking shorthand forms into account
 NodeType = resolve_nodetype_shorthand(NodeTypeShorthand),
 
 % Call the function clause associated with the NodeType
 prefix_node_id(NodeType,Node_id).


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
 Res = max(100,trunc(rand:normal(Mean,Var))),
 io:format("[next_sim_time]: Returning ~w~n",[Res]),
 Res.

 
%%====================================================================================================================================
%%                                                     OTHER UTILITY FUNCTIONS
%%====================================================================================================================================

%% DESCRIPTION:  Checks if an application is running, also considering shorthand forms
%%
%% ARGUMENTS:    - AppName: The name of the application to check if it is running, or its shorthand formmust not be already taken and be >=30000
%%
%% RETURNS:      - true           -> The application is running
%%               - false          -> The application is NOT running
%%               - {error,badarg} -> Invalid arguments
%%
is_running(AppShorthand) when is_atom(AppShorthand) ->

 % Resolve possible application names shorthands
 AppName = resolve_appname_shorthand(AppShorthand),

 % Query the kernel if the application is running
 case [App || {App, _, _} <- application:which_applications(), App =:= AppName] of
 
  % If the application is running, return true
  [AppName] ->
   true;
   
  % Otherwise, return false 
  [] ->
   false
 end;

is_running(_) ->
 {error,badarg}.

%% Resolves application names shorthands (is_running(AppShorthand) helper function)
resolve_appname_shorthand(AppShorthand) ->
 if
 
  % Janet Simulator application shorthand names
  AppShorthand =:= janet_simulator orelse AppShorthand =:= jsim orelse AppShorthand =:= sim orelse AppShorthand =:= janet_sim ->
   janet_simulator;
  AppShorthand =:= janet_controller orelse AppShorthand =:= jctr orelse AppShorthand =:= ctr orelse AppShorthand =:= janet_ctr ->
   janet_controller;
  AppShorthand =:= janet_device orelse AppShorthand =:= jdev orelse AppShorthand =:= dev orelse AppShorthand =:= janet_dev ->
   janet_device;
  
  % Unknown Shorthand  
  true ->
   AppShorthand
 end.


%% DESCRIPTION:  Converts a string to atom using the Erlang BIFs
%%
%% ARGUMENTS:    - Str: The string to be converted to an atom
%%
%% RETURNS:      - 'Str' -> The string converted to atom 
%% 
%% NOTE:         While generating dynamic atoms is strongly discouraged in general, this is required
%%               by some BIFs used in the application (e.g. erlang:set_cookie('node','cookie'))
%%               [TODO]: Also used by 'dev_server's when attempting to register with their controller nodes
%%
str_to_atom(Str) ->
 list_to_atom(lists:flatten(io_lib:format("~s",[Str]))). 