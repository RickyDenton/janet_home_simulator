%% This module offers a set of utility functions for the Janet Simulator application %%

-module(utils).

-export([is_valid_devtype/1,get_devtype_default_config/1,validate_dev_config/2,deprefix_dev_config/1]). % Devices Utility Functions
-export([resolve_nodetype_shorthand/1,prefix_node_id/2]).												% Nodes Utility Functions
-export([is_running/1,str_to_atom/1]).																    % Other Utility Functions

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
validate_dev_config(Config,fan) when 

 % 'onoff' trait ('on'|'off')
 is_atom(Config#fancfg.onoff),
 Config#fancfg.onoff =:= on orelse Config#fancfg.onoff =:= off,

 % 'fanspeed' trait	(0 < fanspeed <= 100)					     
 is_number(Config#fancfg.fanspeed),
 Config#fancfg.fanspeed > 0, Config#fancfg.fanspeed =< 100 ->
 
 % Valid configuration
 ok;

%% ------------------ Valid Light Configuration ------------------ %%
validate_dev_config(Config,light) when 

 % 'onoff' trait ('on'|'off')
 is_atom(Config#lightcfg.onoff),
 Config#lightcfg.onoff =:= on orelse Config#lightcfg.onoff =:= off,
 
 % 'brightness' trait (0 < brightness <= 100)
 is_number(Config#lightcfg.brightness),
 Config#lightcfg.brightness > 0, Config#lightcfg.brightness =< 100 ->
 
 % Valid configuration (NOTE: The 'colorsetting' trait is NOT checked)
 ok;

%% ------------------- Valid Door Configuration ------------------- %%
validate_dev_config(Config,door) when 

 % 'openclose' + 'lockunlock' traits ({'open' && 'unlock'} | {'close' && ('lock' || 'unlock'))
 is_atom(Config#doorcfg.openclose), is_atom(Config#doorcfg.lockunlock), 								  
 (Config#doorcfg.openclose =:= open andalso Config#doorcfg.lockunlock =:= unlock) orelse 
 (Config#doorcfg.openclose =:= close andalso (Config#doorcfg.lockunlock =:= unlock orelse Config#doorcfg.lockunlock =:= lock)) ->

 ok;									 

%% ---------------- Valid Thermostat Configuration ---------------- %%
validate_dev_config(Config,thermostat) when 

 % 'onoff' trait ('on'|'off')
 is_atom(Config#thermocfg.onoff),
 Config#thermocfg.onoff =:= on orelse Config#thermocfg.onoff =:= off,
 									        
 % 'temp_target' trait (0 <= temp_target <= 100)						
 is_number(Config#thermocfg.temp_target),
 Config#thermocfg.temp_target >= 0, Config#thermocfg.temp_target =< 50,
 
 % 'temp_current' trait (NOTE: the interval range is NOT checked)
 is_number(Config#thermocfg.temp_current) ->
 
 % Valid configuration
 ok;

%% ------------------ Valid Heater Configuration ------------------ %% 
validate_dev_config(Config,heater) when 

 % 'onoff' trait ('on'|'off')
 is_atom(Config#heatercfg.onoff),
 Config#heatercfg.onoff =:= on orelse Config#heatercfg.onoff =:= off,

 % 'fanspeed' trait	(0 < fanspeed <= 100)
 is_number(Config#heatercfg.fanspeed),
 Config#heatercfg.fanspeed > 0, Config#heatercfg.fanspeed =< 100,
 
 % 'temp_target' trait (0 <= temp_target <= 100)
 is_number(Config#heatercfg.temp_target),
 Config#heatercfg.temp_target >= 0, Config#heatercfg.temp_target =< 50,
                    
 % 'temp_current' trait (NOTE: the interval range is NOT checked)                   
 is_number(Config#heatercfg.temp_current) ->

 % Valid configuration
 ok;
					
%% -------------------- Invalid Configuration -------------------- %%
validate_dev_config(_,DevType) ->
 
 % At this point the passed configuration is for sure
 % invalid, check if at least the passed "DevType" is
 case is_valid_devtype(DevType) of
 
  % If a valid DevType is passed, its configuration is invalid
  true ->
   throw({error,invalid_devconfig});
   
  % If an invalid DevType was passed, no configuration can be valid
  false ->
   throw({error,unknown_devtype})  
 end.


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