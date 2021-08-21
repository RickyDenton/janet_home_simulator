%% This module offers a set of utility function for the Janet Simulator application %%

-module(utils).

-export([is_running/1,is_valid_devtype/1,get_devtype_default_config/1,resolve_nodetype_shorthand/1,str_to_atom/1]).

%%====================================================================================================================================
%%                                                    PUBLIC UTILITY FUNCTIONS
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


%% DESCRIPTION:  Returns the node type associated to its argument, possibly considering shorthand forms
%%
%% ARGUMENTS:    - NodeTypeShorthand: A node name, possibly in a shorthand form
%%
%% RETURNS:      - NodeType -> The node type atom associated to NodeTypeShorthand
%%               - unknown  -> If no node type could be associated with NodeTypeShorthand
%%
resolve_nodetype_shorthand(NodeTypeShorthand) ->
 if
 
  % Controller node
  NodeTypeShorthand =:= controller orelse NodeTypeShorthand =:= ctr orelse NodeTypeShorthand =:= contr ->
   controller;
   
  % Device node
  NodeTypeShorthand =:= device orelse NodeTypeShorthand =:= dev ->
   device;
  
  % Unknown nodetype  
  true ->
   unknown
 end.
 

%% DESCRIPTION:  Checks if a device type is valid
%%
%% ARGUMENTS:    - DevType: The device type to check the validity
%%
%% RETURNS:      - true  -> Valid DevType
%%               - false -> Invalid DevType
%%               - {error,badarg} -> Invalid argument
%%
is_valid_devtype(DevType) when is_atom(DevType) ->
 if
 
  % If the device type is valid, return true
  DevType =:= light orelse
  DevType =:= fan   orelse
  DevType =:= door  orelse
  DevType =:= thermostat orelse
  DevType =:= heater ->
   true;
   
  % Otherwise, return false
  true ->
   false
 end;
 
is_valid_devtype(_) ->
 {error,badarg}. 


%% DESCRIPTION:  Returns a device's default configuration according to its type
%%
%% ARGUMENTS:    - DevType: The device's type
%%
%% RETURNS:      - Config         -> The device default configuration associated to its DevType
%%               - {error,badarg} -> Invalid argument
%% 
%% THROWS:       - unknown_devtype -> If the device's type is invalid (which is not allowed at this point)
%%
get_devtype_default_config(DevType) when is_atom(DevType) ->
 if
 
  % Return the device's default configuration according to its type
  DevType =:= light ->
   {off,0,white};
  DevType =:= fan ->
   {off,0};
  DevType =:= door ->
   {close,lock};
  DevType =:= thermostat ->
   {off,21,21};
  DevType =:= heater ->
   {off,21,21,0};
   
  % If unknown device type, raise a throw exception 
  true ->
   throw(unknown_devtype)
 end;
  
get_devtype_default_config(_) ->
 {error,badarg}.   


%% DESCRIPTION:  Converts a string to atom using the Erlang BIFs
%%
%% ARGUMENTS:    - Str: The string to be converted to an atom
%%
%% RETURNS:      - 'Str' -> The string converted to atom 
%% 
%% NOTE:         While generating dynamic atoms is strongly discouraged in general, this is required
%%               by some BIFs used in the application (e.g. erlang:set_cookie('node','cookie'))
%%
str_to_atom(Str) ->
 list_to_atom(lists:flatten(io_lib:format("~s",[Str]))). 