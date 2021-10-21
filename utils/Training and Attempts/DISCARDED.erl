%%======================================%%
%%     Old reset function (buggy)       %%
%%======================================%%
% Resets the Mnesia database (WARNING!)
reset() ->
 case jsim:is_running(janet_simulator) of
  true ->
   io:format("Please stop the JANET Simulator first~n");
  false ->
   
   % Read the number of records in the Location and the Device table so to warn the user
   jsim:start_mnesia(),
   
   N_Locations= get_table_keys_num(location),
   N_Devices = get_table_keys_num(device),
 
   jsim:stop_mnesia(),
   
   Reset = if
            N_Locations + N_Devices > 0 ->
	         io:format("WARNING: ~p location(s) and ~p device(s) are currently stored in the Mnesia database, deleting them may generate inconsistencies with the remote database.~n",[N_Locations,N_Devices]),
	         {ok,[Ans]} = io:fread("Are you sure you want to proceed? (y/N): ","~s"),
	         if
			  Ans =:= "y"; Ans =:= "Y" ->
			   reset;
	          true ->
	           not_reset
	         end;
           true ->
            reset
          end,
		  
   case Reset of
    reset ->                             % NOTE: The schema in fact cannot be dropped using the mnesia:delete_schema([node()]) and/or mnesia:create_schema([node()]), thus only tables are reset
     install_db();
	_ ->
	 aborted
   end
 end.





%%====================================================%%
%%        Old print_tree_* function (buggy)           %%
%%====================================================%%
print_tree_user([]) ->
 ok;
print_tree_user([User|NextUser]) ->
 
 % Retrieve the user's locations list
 Loclist = mnesia:dirty_match_object(#location{user = User, _ = '_'}),

 io:format("{user: ~s}~n",[io_lib:format("~p",[User])]),
 print_tree_location(Loclist,"|--"),
 io:format("~n"),	
 print_tree_user(NextUser).


print_tree_location([],_) ->
 ok;
print_tree_location([Loc|Nextloc],Indent) ->
 
 % Retrieve the list of sublocations in the location (note that at least the "default" sublocation is always returned)
 F = fun() -> mnesia:match_object(#sublocation{sub_id = {Loc#location.loc_id,'_'}, _ = '_'}) end,
 
 {atomic,Subloclist} = mnesia:transaction(F),
 
 % Print information on the location
 io:format("~s~s~n",[Indent,io_lib:format("~p",[Loc])]),
 
 % Print information on all location's sublocations, augmenting the indentation accordingly and also considering if its the last location
 case {Indent, Nextloc} of
  {"",_} ->
   print_tree_sublocation(Subloclist,"|--");
  {_,[]} ->
   print_tree_sublocation(Subloclist,"   " ++ Indent);
  {_,_Otherloc} ->
   print_tree_sublocation(Subloclist,"|  " ++ Indent)
 end,
 print_tree_location(Nextloc,Indent).
 
 %case Nextloc of
 % [] ->
 %  print_tree_sublocation(Subloclist,Indent ++ "   ");
%	 
%  _Otherloc ->
%   print_tree_sublocation(Subloclist,Indent ++ "|  ")
% end,
% 



print_tree_sublocation([],_) ->
 ok;
print_tree_sublocation([Subloc|NextSubloc],Indent) ->

 
 
 % Retrieve the list of devices in the sublocation
 F = fun() -> mnesia:match_object(#device{sub_id=Subloc#sublocation.sub_id, _ = '_'}) end,

 Result = mnesia:transaction(F),
 case Result of
  {atomic, []} ->
   
   % If the sublocation is empty, just print its information
   io:format("~s~s (empty)~n",[Indent,io_lib:format("~p",[Subloc])]);
		  
  {atomic,DevList} ->
   
   % Otherwise also print its devices, augmenting the indentation accordingly and also considering if its the last sublocation
   io:format("~s~s~n",[Indent,io_lib:format("~p",[Subloc])]),
   
   case {Indent, NextSubloc} of
    {"",_} ->
	 print_tree_device(DevList,"|--");
	{_,[]} ->
	 print_tree_device(DevList,"   " ++ Indent);
    {_,_OtherSubloc} ->
     print_tree_device(DevList,"|  " ++ Indent)
   end;
  
 
  % case NextSubloc of
  %  [] ->
%	 print_tree_device(DevList,Indent ++ "   ");
%	 
  %  _OtherSubloc ->
 %    print_tree_device(DevList,Indent ++ "|  ")
 %  end;
   
  _ ->
   Result
   
 end,
 print_tree_sublocation(NextSubloc,Indent).
 
print_tree_device([],_) ->
 ok;
print_tree_device([Dev|NextDev],Indent) ->
 io:format("~s~s~n",[Indent,io_lib:format("~p",[Dev])]),
 print_tree_device(NextDev,Indent).
 
 
 
 
%%====================================================%%
%%             Old print_* function                   %%
%% (works but not elegant, superseded by print_tree*) %%
%%====================================================%% 

%% DESCRIPTION:  Print the information on a sublocation and all devices in it
%%
%% ARGUMENTS:    - {Loc_id,Subloc_id}: The sub_id of the sublocation to print information
%%
%% RETURNS:      - If found, information on the sublocation and its devices
%%               - {error,sublocation_not_exists} if the sublocation is not found
%%               - {error,badarg} if wrong argument format
%%
%% THROWS:       none 
print_sublocation({Loc_id,Subloc_id}) when is_number(Loc_id), Loc_id>0, is_number(Subloc_id), Subloc_id>=0 ->
 F = fun() ->
 
      % Check the sublocation to exist
	  case mnesia:read({sublocation,{Loc_id,Subloc_id}}) of
	   [Sublocation] ->
	    
		% Retrieve the list of devices in the sublocation
		DevList = mnesia:match_object(#device{sub_id={Loc_id,Subloc_id}, _ = '_'}),
		case DevList of
		
		 % If the sublocation is empty, just print its information
		 [] ->
		  io:format("~n~s (empty)~n~n",[io_lib:format("~p",[Sublocation])]);
		  
		 % Otherwise, also print all its devices 
		 _ -> 
		  io:format("~n~s~n",[io_lib:format("~p",[Sublocation])]),
		  print_dev_in_sub(DevList)
		end;
		
	   [] ->
	    mnesia:abort(sublocation_not_exists)
      end
     end,
 
 Result = mnesia:transaction(F),
 case Result of
  {atomic, ok} ->
   ok;
  _ ->
   Result
 end;
 
print_sublocation(_) ->
 {error,badarg}.

print_dev_in_sub([]) ->
 io:format("~n");
print_dev_in_sub([Dev|T]) ->
 io:format("|--~s~n",[io_lib:format("~p",[Dev])]),
 print_dev_in_sub(T).


%% DESCRIPTION:  Prints indented information on a location along with all its sublocations and devices
%%
%% ARGUMENTS:    - Loc_id: The loc_id of the location to print information on
%%
%% RETURNS:      - If found, indented information on the location along with all its sublocations and devices
%%               - {error,location_not_exists} if the location is not found
%%               - {error,badarg} if wrong argument format
%%
%% THROWS:       none 
print_location(Loc_id) when is_number(Loc_id), Loc_id>0 ->
 F = fun() ->
 
      % Check the location to exist
	  case mnesia:read({location,Loc_id}) of
	   [Location] ->
	    
		% Retrieve the list of sublocations in the location (note that at least the "default" sublocation is always returned)
		Subloclist = mnesia:match_object(#sublocation{sub_id = {Loc_id,'_'}, _ = '_'}),
		io:format("~n~s~n",[io_lib:format("~p",[Location])]),
		print_sub_in_loc(Subloclist,"|--");
		
	   [] ->
	    mnesia:abort(location_not_exists)
      end
     end,
 
 mnesia:transaction(F);
 
print_location(_) ->
 {error,badarg}.

%% Prints all sublocations in a location (print_location helper function) 
print_sub_in_loc([],_) ->
 io:format("~n"); % Final newline
 
print_sub_in_loc([Subloc|T],Indentation) ->

 % Retrieve the list of devices in the sublocation
 DevList = mnesia:match_object(#device{sub_id=Subloc#sublocation.sub_id, _ = '_'}),
   
 case DevList of
		
  [] ->
   % If the sublocation is empty, just print its information
   io:format("~s~s (empty)~n",[Indentation,io_lib:format("~p",[Subloc])]);
		  
  _ ->
  
   % Otherwise, also print all its devices, setting the indentation accordingly if its the last sublocation 
   io:format("~s~s~n",[Indentation,io_lib:format("~p",[Subloc])]),
   case T of
   	[] ->
	 print_dev_in_sub_in_loc(DevList,"   |--");
	 
    [_OtherSubloc] ->
	 print_dev_in_sub_in_loc(DevList,"|  |--")
   end
 end,
 print_sub_in_loc(T,Indentation).

%% Prints all devices in a sublocation (print_location->print_sub_in_loc helper function) 
print_dev_in_sub_in_loc([],_) ->
 ok;
print_dev_in_sub_in_loc([Dev|T],Indentation) ->
 io:format("~s~s~n",[Indentation,io_lib:format("~p",[Dev])]),
 print_dev_in_sub_in_loc(T,Indentation).