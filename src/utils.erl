-module(utils).
-export([str_to_atom/1,concat_atoms/2]).

str_to_atom(Str) ->
 list_to_atom(lists:flatten(io_lib:format("~s",[Str]))).
 
concat_atoms(Atom1,Atom2) ->
 list_to_atom(atom_to_list(Atom1) ++ atom_to_list(Atom2)).