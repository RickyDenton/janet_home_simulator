-module(utils).
-export([is_valid_devtype/1,get_devtype_default_config/1,str_to_atom/1]).
-export([is_running/1]).

% Checks if an application is running
is_running(AppName) ->
 case [App || {App, _, _} <- application:which_applications(), App =:= AppName] of
  [AppName] ->
   true;
  [] ->
   false
 end.


is_valid_devtype(DevType) ->
 if
  DevType =:= light orelse
  DevType =:= fan   orelse
  DevType =:= door  orelse
  DevType =:= thermostat orelse
  DevType =:= heater ->
   true;
  true ->
   false
 end.
 
get_devtype_default_config(DevType) ->
 if
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
  true ->
   throw(unknown_devtype)
 end.
  
str_to_atom(Str) ->
 list_to_atom(lists:flatten(io_lib:format("~s",[Str]))). 


%% -- Old, remove
%% 
%%concat_atoms(Atom1,Atom2) ->
%% list_to_atom(atom_to_list(Atom1) ++ atom_to_list(Atom2)).