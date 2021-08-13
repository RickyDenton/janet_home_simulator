-module(devutils).
-export([is_valid_devtype/1,get_devtype_default_config/1]).

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
   {offline,off,0,white};
  DevType =:= fan ->
   {offline,off,0};
  DevType =:= door ->
   {offline,close,lock};
  DevType =:= thermostat ->
   {offline,21,21};
  DevType =:= heater ->
   {offline,21,21,0};
  true ->
   throw(unknown_devtype)
 end.
  
 
%% -- Old, remove
%%str_to_atom(Str) ->
%% list_to_atom(lists:flatten(io_lib:format("~s",[Str]))).
%% 
%%concat_atoms(Atom1,Atom2) ->
%% list_to_atom(atom_to_list(Atom1) ++ atom_to_list(Atom2)).