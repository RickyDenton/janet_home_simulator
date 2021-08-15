-module(db).
-export([add_location/4,add_sublocation/2,add_device/3,update_dev_subloc/2,update_dev_config/2,update_loc_name/2,update_subloc_name/2,subtry/0,read_location/1,install/0,clear/0,reset/0,reset/1,dump/0,dump/1,print_table/1,record_by_id/2,recordnum/0]).
-export([print_tree_sublocation/3,print_tree_location/2,print_tree_user/1]).
-export([print_tree/1,print_tree/2]).
%%====================================================================================================================================%%
%%                                               MNESIA TABLES RECORDS DEFINITIONS                                                    %%
%%====================================================================================================================================%%

%% --- disc_copies tables --- %%

-record(location,     	% A location
        {
		 loc_id,       	% The location/controller's ID (must be unique)
		 name,        	% The location's name (optional)
		 user,        	% The location's user
		 port        	% The location controller's port for REST operations (must be unique)
		}).

-record(sublocation, 	% A sublocation
        {
		 sub_id,     	% The sublocation's full ID {loc_id,subloc_id} ({X,0} = default sublocation of location X)
		 name       	% The sublocation's name
		}).
		
-record(device,      	% A device
        {
		 dev_id,      	% The device's ID
		 sub_id,        % The full ID {loc_id,subloc_id} of the sublocation the device is deployed in
		 type,       	% The device's type (light, fan, door, thermostat, heater)
		 config      	% The device's configuration (type-specific)
		}).

%% --- ram_copies tables --- %%

-record(locmanager,     % The PID of a location top-level supervisor
        {
		 loc_id,        % The location's ID
         sup_pid        % The PID of the location's top-level supervisor
        }).		 
		
-record(ctrmanager,     % A location controller's manager
        {
		 loc_id,        % The location's ID
		 sup_pid,       % The PID of the location controller's manager
		 status         % The controller's status (on,off)
		}).

-record(devmanager,     % A device's manager
        {
		 dev_id,        % The device's ID
		 sup_pid,       % The PID of the device's manager
		 status         % The device's status (on,off)
		}).       



%%====================================================================================================================================
%%                                                        CRUD OPERATIONS
%%==================================================================================================================================== 


add_location(Loc_id,Name,User,Port) when is_number(Loc_id), Loc_id>0, is_number(Port), Port>0 ->
 F = fun() ->
 
      % Check if the location already exists
	  case mnesia:read({location,Loc_id}) of
	   [] ->
	    
		% Check if the port is already taken
		case mnesia:match_object(#location{port = Port, _ = '_'}) of
		 [] ->
		 
		  % Insert the new location and its default sublocation
	      mnesia:write(#location{loc_id=Loc_id,name=Name,user=User,port=Port}),
		  mnesia:write(#sublocation{sub_id={Loc_id,0}, name="(default)"});
		  
		 [_LocationRecord] ->
		  mnesia:abort(port_already_taken)
		end;
		
	   [_LocationRecord] ->
	    mnesia:abort(location_already_exists)
      end
     end,
 
 Result = mnesia:transaction(F),
 case Result of
  {atomic, ok} ->
   %% [TODO]: Start sup_loc
   Result;   
  _ ->
   Result
 end;
 
add_location(_,_,_,_) ->
 {error,badarg}.


add_sublocation({Loc_id,Subloc_id},Name) when is_number(Loc_id), Loc_id>0, is_number(Subloc_id), Subloc_id>0 ->
 F = fun() ->
 
      % Check the location to exist
	  case mnesia:read({location,Loc_id}) of
	   [_LocationRecord] ->
	    
		% Check if a sublocation with the same Subloc_id already exists
		case mnesia:read({sublocation,{Loc_id,Subloc_id}}) of
		 [] ->
		 
		  % Insert the new sublocation
	      mnesia:write(#sublocation{sub_id={Loc_id,Subloc_id}, name=Name});
		  
		 [_SublocationRecord] ->
		  mnesia:abort(sublocation_already_exists)
		end;
		
	   [] ->
	    mnesia:abort(location_not_exists)
      end
     end,
 mnesia:transaction(F);

add_sublocation(_,_) ->
 {error,badarg}.


add_device(Dev_id,{Loc_id,Subloc_id},Type) when is_number(Dev_id), Dev_id>0, is_number(Loc_id), Loc_id>0, is_number(Subloc_id), Subloc_id>=0 ->
 F = fun() ->
 
      % Check the target sublocation to exist
	  case mnesia:read({sublocation,{Loc_id,Subloc_id}}) of
	   [_SublocationRecord] ->
	    
		% Check if a device with the same Dev_id already exists
		case mnesia:read({device,Dev_id}) of
		 [] ->
		 
		  % Insert the new device
	      mnesia:write(#device{dev_id=Dev_id, sub_id={Loc_id,Subloc_id}, type=Type, config=devutils:get_devtype_default_config(Type)});
		  
		 [_DeviceRecord] ->
		  mnesia:abort(device_already_exists)
		end;
		
	   [] ->
	    mnesia:abort(sublocation_not_exists)
      end
     end,
	 
 % Check the device type to be valid (outside the transaction)
 case devutils:is_valid_devtype(Type) of
 
  % If it is not, directly return an error 
  false ->
   {error,devtype_not_exists};
  
  % If it is, attempt the transaction
  true -> 
   Result = mnesia:transaction(F), 
   case Result of
    
    % If the transaction succeded
    {atomic, ok} ->
     %% [TODO]: Start sup_dev
	 %% [TODO]: inform the controller of the new device (if not passing directly from it)
     Result;
	 
    _ ->
     Result
   end
 end;

add_device(_,_,_) ->
 {error,badarg}.


update_dev_subloc(Dev_id,{Loc_id,Subloc_id}) when is_number(Dev_id), Dev_id>0, is_number(Loc_id), Loc_id>0, is_number(Subloc_id), Subloc_id>=0 ->
 F = fun() ->
 
      % Check the device to exist
	  case mnesia:wread({device,Dev_id}) of      % wread = write lock
	   [Device] ->
		if 
		
		 % If the new and old sublocation coincide, return
		 Device#device.sub_id =:= {Loc_id,Subloc_id} ->  
		  ok;

         % Otherwise, check the target sublocation to exist
		 true ->
		  case mnesia:read({sublocation,{Loc_id,Subloc_id}}) of
		   [_SublocationRecord] ->
		   
		    % Change the device to the target sublocation 
		    UpdatedDevice = Device#device{sub_id={Loc_id,Subloc_id}},
		    mnesia:write(UpdatedDevice);
		  
		   [] ->
		    mnesia:abort(sublocation_not_exists)
		  end
	    end;
		
	   [] ->
	    mnesia:abort(device_not_exists)
      end
     end,
 mnesia:transaction(F);	 
	 
update_dev_subloc(_,_) ->
 {error,badarg}.


update_dev_config(Dev_id,Config) when is_number(Dev_id), Dev_id>0 ->
 F = fun() ->
 
      % Check the device to exist
	  case mnesia:wread({device,Dev_id}) of      % wread = write lock
	   [Device] ->
		if 
		
		 % If the new and old configuration coincide, return
		 Device#device.config =:= Config ->  
		  ok;

         % Otherwise, update the configuration (NOTE: no check on the validity of "Config" is performed)
		 true ->
		  UpdatedDevice = Device#device{config=Config},
		  mnesia:write(UpdatedDevice)
	    end;
		
	   [] ->
	    mnesia:abort(device_not_exists)
      end
     end,
 mnesia:transaction(F);	 
	 
update_dev_config(_,_) ->
 {error,badarg}.


update_loc_name(Loc_id,Name) when is_number(Loc_id), Loc_id>0 ->
 F = fun() ->
 
      % Check the location to exist
	  case mnesia:wread({location,Loc_id}) of      % wread = write lock
	   [Location] ->
		if 
		
		 % If the new and old names coincide, return
		 Location#location.name =:= Name ->  
		  ok;

         % Otherwise, update the location's name
		 true ->
		  UpdatedLocation = Location#location{name=Name},
		  mnesia:write(UpdatedLocation)
	    end;
		
	   [] ->
	    mnesia:abort(location_not_exists)
      end
     end,
 mnesia:transaction(F);	 

update_loc_name(_,_) ->
 {error,badarg}.


update_subloc_name({Loc_id,Subloc_id},Name) when is_number(Loc_id), Loc_id>0, is_number(Subloc_id), Subloc_id>0 -> % NOTE: The name of the default sublocation cannot be changed (>0)
 F = fun() ->
 
      % Check the sublocation to exist
	  case mnesia:wread({sublocation,{Loc_id,Subloc_id}}) of      % wread = write lock
	   [Sublocation] ->
		if 
		
		 % If the new and old names coincide, return
		 Sublocation#sublocation.name =:= Name ->  
		  ok;

         % Otherwise, update the sublocation's name
		 true ->
		  UpdatedSublocation = Sublocation#sublocation{name=Name},
		  mnesia:write(UpdatedSublocation)
	    end;
		
	   [] ->
	    mnesia:abort(sublocation_not_exists)
      end
     end,
 mnesia:transaction(F);	 

update_subloc_name(_,_) ->
 {error,badarg}.
 








%% DESCRIPTION:  Prints the contents of all or a specific table in the database
%%
%% ARGUMENTS:    - (Tabletype): The table whose contents are to be printed, also considering shorthand forms
%%               - (all):   Print all tables contents   
%%
%% RETURNS:      - If it exists, the list of records in the specified table
%%               - {error,unknown_table} if unknown Tabletype
%%               - {error,badarg} if wrong argument format
%%
%% THROWS:       none  
print_table(all) ->
 
 % Call the function on all tables
 print_table(location),
 print_table(sublocation),
 print_table(device);
 
print_table(Tabletype) when is_atom(Tabletype) ->

 % Resolve possible table shorthand forms
 Table = resolve_tabletype_shorthand(Tabletype),
 
 case Table of
 
  % If unknown table, return an error
  unknown ->
   {error,unknown_table};
   
  % Otherwise print the table header and contents
  _->   
   print_table_header(Table),
   print_table_records(get_table_records(Table))
 end;
 
print_table(_) ->
 {error,badarg}.

%% Prints the header of a table (print_table helper function) 
print_table_header(location) ->
 io:format("~nLOCATION TABLE {loc_id,name,user,port}~n==============~n");
print_table_header(sublocation) ->
 io:format("~nSUBLOCATION TABLE {sub_id,name}~n=================~n");
print_table_header(device) -> 
 io:format("~nDEVICE TABLE {dev_id,sub_id,type,config}~n============~n").

%% Prints the records in a table (print_table helper function)  
print_table_records([]) ->
 io:format("~n");
print_table_records([H|T]) ->
 io:format("~s~n",[io_lib:format("~p",[H])]),
 print_table_records(T).
 

%% DESCRIPTION:  Searchs for a record in a table by id
%%
%% ARGUMENTS:    - Tabletype: The table where to search the record in, also considering shorthand forms
%%               - Id: The id of the record to search for
%%
%% RETURNS:      - If the record is found, its contents
%%               - not_found, if the record is not found
%%               - {error,unknown_table} if unknown Tabletype
%%               - {error,badarg} if wrong argument format
%%
%% THROWS:       none  
record_by_id(Tabletype,Id) when is_atom(Tabletype) ->

 % Resolve possible table shorthand forms
 Table = resolve_tabletype_shorthand(Tabletype),
 
 case Table of
 
  % If unknown table, return an error
  unknown ->
   {error,unknown_table};
  
  % Otherwise, search for the record by Id
  _ ->
   case mnesia:transaction(fun() -> mnesia:read({Table,Id}) end) of
    
	% If the record was found, return it
    {atomic,[Record]} ->
	 Record;
	 
	% Otherwise, return that it was not found 
	{atomic,[]} ->
	 not_exists
   end
 end;

record_by_id(_,_) ->
 {error,badarg}.





















%% DESCRIPTION:  Prints database contents indented as a tree
%%
%% ARGUMENTS:    - (all):                prints all locations, sublocations and devices of all users in the database
%%               - (user,Username):      prints all locations, sublocations and devices of a specific user in the database
%%               - (location,Loc_id):    prints all sublocations and devices in a specific location
%%               - (sublocation,Sub_id): prints all devices in a specific sublocation
%%
%% RETURNS:      A view of the database indented as a tree, depending on the arguments
%%
%% THROWS:       none  
print_tree(all) ->
 
 % Retrieve all unique users in the database
 UserList = get_all_users(),
 
 if
 
  % If there is at least one user, print all the users' locations, sublocations and devices as a tree
  length(UserList) > 0 ->
   io:format("~n"),
   print_tree_user(UserList);
  
  % Otherwise, inform that there is no user in the database
  true ->
   io:format("The database is empty")
 end;

print_tree(_) ->
 {error, badarg}.


print_tree(user,Username) ->
 
 % Check the user to exist
 UserLocs = mnesia:dirty_match_object(#location{user = Username, _ = '_'}),

 case UserLocs of
 
  % If the user was not found, return
  [] ->
   not_found;
   
  % Otherwise print information on all the user's locations, sublocations and devices as a tree, taking the indentation into account
  _Loclist -> 
   io:format("~n{user: ~s}~n",[io_lib:format("~p",[Username])]),
   print_tree_location(UserLocs,"|--"),
   io:format("~n")
 end;
  
  
print_tree(Tabletype,Id) when is_atom(Tabletype) ->
 
 % Resolve possible table shorthand forms
 Table = resolve_tabletype_shorthand(Tabletype),

 case Table of
 
  % If unknown table, return an error
  unknown ->
   {error,unknown_table};
  
  % The device table is not supported in this case
  device ->
   {error,unsupported};
  
  location ->
  
   % Check the location to exist
   Location = mnesia:dirty_read({Table,Id}),
   case Location of
   
   	% If it not exists, return an error
    [] ->
	 {error,location_not_exists};
	 
    % If it exists, print its sublocations and devices as a tree
    _Location ->
	 io:format("~n"),
	 print_tree_location(Location,""),
	 io:format("~n")
   end;
 
  sublocation ->
  
   % Check the sublocation to exist
   Sublocation = mnesia:dirty_read({Table,Id}),
   case Sublocation of
   
   	% If it not exists, return an error
	[] ->
	 {error,sublocation_not_exists};
   
    % If it exists, print its devices as a tree
    _Sublocation ->
	 io:format("~n"),
	 print_tree_sublocation(Sublocation,"",""),
     io:format("~n")
   end
 end;
 
print_tree(_,_) ->
 {error, badarg}.

%% Prints all locations, sublocations and devices belonging to a list of users as a tree (print_tree(user,Username),print_tree(All) helper function) 
print_tree_user([]) ->
 ok;
print_tree_user([User|NextUser]) ->
 
 % Retrieve the user's locations list
 Loclist = mnesia:dirty_match_object(#location{user = User, _ = '_'}),

 % Print information on the user
 io:format("{user: ~s}~n",[io_lib:format("~p",[User])]),
 
 % Print information on all the user's locations, sublocations and devices as a tree, taking the indentation into account
 print_tree_location(Loclist,"|--"),
 io:format("~n"),	
 print_tree_user(NextUser).


%% Prints all sublocations and devices in a list of locations as a tree (print_tree(location,Loc_id), print_tree_user([User|NextUser]) helper function)
print_tree_location([],_) ->
 ok;
print_tree_location([Loc|Nextloc],Indent) ->
 
 % Retrieve the list of sublocations in the location (note that at least the "(default)" sublocation is always present)
 Subloclist = mnesia:dirty_match_object(#sublocation{sub_id = {Loc#location.loc_id,'_'}, _ = '_'}),
 
 % Print information on the location
 io:format("~s~s~n",[Indent,io_lib:format("~p",[Loc])]),
 
 % Print information on all location's sublocations and devices as a tree, taking the indentation into account
 case {Indent, Nextloc} of
  {"",_} ->
   print_tree_sublocation(Subloclist,"","|--");
  {_,[]} ->
   print_tree_sublocation(Subloclist,"   ","|--");
  {_,_Otherloc} ->
   print_tree_sublocation(Subloclist,"|  ","|--")
 end,
 print_tree_location(Nextloc,Indent).


%% Prints all devices in a list of locations as a tree (print_tree(sublocation,Sub_id), print_tree_location([Loc|NextLoc],Indent) helper function)
print_tree_sublocation([],_,_) ->
 ok;
print_tree_sublocation([Subloc|NextSubloc],Indent1,Indent2) ->

 % Retrieve the list of devices in the sublocation
 Devlist = mnesia:dirty_match_object(#device{sub_id=Subloc#sublocation.sub_id, _ = '_'}),
 
 case Devlist of
  [] ->
   
   % If the sublocation is empty, just print its information
   io:format("~s~s (empty)~n",[Indent1++Indent2,io_lib:format("~p",[Subloc])]);
		  
  _Devices ->
   
   % Otherwise also print the devices within the location as a tree, taking the indentation into account
   io:format("~s~s~n",[Indent1++Indent2,io_lib:format("~p",[Subloc])]),
   case {Indent2, NextSubloc} of
    {"|--",[]} ->
	 print_tree_device(Devlist,Indent1 ++ "   " ++ Indent2);
    {"|--",_OtherSubloc} ->
     print_tree_device(Devlist,Indent1 ++ "|  " ++ Indent2);
	{"",[]} ->
	 print_tree_device(Devlist,"|--")
   end
 end,
 print_tree_sublocation(NextSubloc,Indent1,Indent2).


%% Prints all devices in a location as a tree (print_tree_sublocation([Subloc|NextSubloc],Indent1,Indent2) helper function) 
print_tree_device([],_) ->
 ok;
print_tree_device([Dev|NextDev],Indent) ->
 io:format("~s~s~n",[Indent,io_lib:format("~p",[Dev])]),
 print_tree_device(NextDev,Indent).










%% DESCRIPTION:  Prints the number of records in each of the database's disc_copies tables
%%
%% ARGUMENTS:    none
%%
%% RETURNS:      The number of records in each of the database's disc_copies tables
%%
%% THROWS:       none  
recordnum() ->

 % Retrieve the number of records in each of the database's disc_copies tables and print it
 LocationKeysNum = get_table_keys_num(location),
 SublocationKeysNum = get_table_keys_num(sublocation),
 DeviceKeysNum = get_table_keys_num(device),
 io:format("~p location(s), ~p sublocation(s), ~p device(s)~n",[LocationKeysNum,SublocationKeysNum,DeviceKeysNum]).



 
 
%% Utility 

subtry() ->
  F = fun() ->
       mnesia:match_object(#sublocation{sub_id = {1,'_'}, _ = '_'})
	  end,
  mnesia:transaction(F).



read_location(Loc_id) ->
 F = fun() ->
      mnesia:read({location,Loc_id})
	 end,
 case mnesia:transaction(F) of
  {atomic,[]} ->
   undefined;
  {atomic,[#location{loc_id=Loc_id,name=Name,user=User,port=Port}]} ->
   {Loc_id,Name,User,Port}
  end.


%%====================================================================================================================================
%%                                                    PUBLIC UTILITY FUNCTIONS
%%==================================================================================================================================== 


%% DESCRIPTION:  Clears all database tables, preserving its schema (no installation required)
%%
%% ARGUMENTS:    none
%%
%% RETURNS:      - is_running -> The JANET simulator is running, thus the operation cannot be performed
%%               - aborted    -> The user aborted the operation
%%               - ok         -> The database was successfully cleared
%%
%% THROWS:       none
clear() ->
 % Check if the operation can be performed
 CheckOp = check_db_operation("Clearing"),

 case CheckOp of
 
  % If the operation can be performed, start Mnesia and clear all disc_copies tables
  ok ->
   jsim:start_mnesia(),
   [{atomic,ok},{atomic,ok},{atomic,ok}] = [mnesia:clear_table(location),mnesia:clear_table(sublocation),mnesia:clear_table(device)],
   jsim:stop_mnesia(),
   io:format("Mnesia tables successfully cleared~n");
   
  % Otherwise return CheckOp
  _ ->
   CheckOp
 end.
 

 
%% DESCRIPTION:  Dumps the database contents to a backup file
%%
%% ARGUMENTS:    - (none) -> a default file path is used
%%               - File   -> a custom file path is used
%%
%% RETURNS:      - ok             -> Database successfully dumped to the specified file
%%               - is_running     -> The JANET simulator is running, thus the operation cannot be performed
%%               - aborted         -> The user aborted the operation
%%               - {error,Reason} -> I/O error
%%
%% THROWS:       none 
dump() ->
 dump("priv/mnesia_backup.db").  % Default backup path
 
dump(File) ->

 % Dump the database contents to a file and return the result of the operation
 DumpDB = mnesia:dump_to_textfile(File),
 case DumpDB of
  ok ->
   io:format("Mnesia database successfully dumped to file \"~s\"~n",[File]);
  _ ->
   DumpDB
 end.



%% DESCRIPTION:  Resets the database to the contents of a backup file
%%
%% ARGUMENTS:    - (none) -> a default file path is used
%%               - (File) -> a custom file path is used
%%
%% RETURNS:      - ok             -> Database successfully reset to the contents of the specified file
%%               - is_running     -> The JANET simulator is running, thus the operation cannot be performed
%%               - aborted         -> The user aborted the operation
%%               - {error,Reason} -> I/O error
%%
%% THROWS:       none  
reset() ->
 reset("priv/mnesia_backup.db").
 
reset(File) ->
 % Check if the operation can be performed
 CheckOp = check_db_operation("Resetting"),

 case CheckOp of
 
  % If the operation can be performed, reset the database to the contents of the provided file 
  ok ->
   ResetDB = mnesia:load_textfile(File),
   case ResetDB of
    {atomic, ok} ->
	 io:format("Mnesia database successfully reset to the contents of the \"~s\" file~n",[File]);
	_ ->
	 ResetDB
   end;
  _ ->
   CheckOp 
 end.
 

%% DESCRIPTION:  Installs the Mnesia local database
%%
%% ARGUMENTS:    none
%%
%% RETURNS:      - ok             -> Database successfully installed
%%               - is_running     -> The JANET simulator is running, thus the operation cannot be performed
%%               - aborted         -> The user aborted the operation
%%
%% THROWS:       none  
install() ->

 % Check if the operation can be performed
 CheckOp = check_db_operation("Installing"),

 case CheckOp of
 
  % If the operation can be performed, install the mnesia database
  ok ->
  
   % Stop Mnesia and reset its schema
   application:stop(mnesia),
   mnesia:delete_schema([node()]),
   mnesia:create_schema([node()]),
   
   % Start Mnesia again and create the tables
   application:start(mnesia),
	 
   % --- disc_copies tables 
   mnesia:create_table(location,
                      [{attributes, record_info(fields, location)},
                      {index, [#location.user,#location.port]},
                      {disc_copies, [node()]}]),				 
   mnesia:create_table(sublocation,
                      [{attributes, record_info(fields, sublocation)},
					  {type, ordered_set},
                      {disc_copies, [node()]}]),		 
   mnesia:create_table(device,
                      [{attributes, record_info(fields, device)},
                      {index, [#device.sub_id]},
                      {disc_copies, [node()]}]),

   % --- ram_copies tables %					 
   mnesia:create_table(locmanager,
                      [{attributes, record_info(fields, locmanager)},
                      {ram_copies, [node()]}]),
   mnesia:create_table(ctrmanager,
                      [{attributes, record_info(fields, ctrmanager)},
                      {ram_copies, [node()]}]),
   mnesia:create_table(devmanager,
                      [{attributes, record_info(fields, devmanager)},
                      {ram_copies, [node()]}]),
		
   % Stop Mnesia and return		
   application:stop(mnesia),
   io:format("Mnesia database successfully installed~n");
  
  _ ->
   CheckOp
 end.
 
 
%%====================================================================================================================================
%%                                                 PRIVATE UTILITY FUNCTIONS
%%==================================================================================================================================== 
 
%% DESCRIPTION:  Checks the JANET simulator to be stopped and asks the user confirmation on proceeding on a database operation
%%
%% ARGUMENTS:    - Operation: a String that will be concatenated in the user confirmation message
%%
%% RETURNS:      - is_running -> The JANET simulator is running, thus the operation cannot be performed
%%               - aborted    -> The user aborted the operation
%%               - ok         -> The user confirmed the database operation
%%
%% THROWS:       none
check_db_operation(Operation) ->

 % Check the JANET simulator not to be running
 case jsim:is_running(janet_simulator) of
 
  true ->
   io:format("Please stop the JANET Simulator first ~n"),
   is_running;
   
  false ->
   
   % Ask user confirmation for the operation
   io:format("~s",[Operation]),
   {ok,[Ans]} = io:fread(" the Mnesia database may cause inconsistencies with the remote database. Are you sure you want to proceed? (y/N): ","~s"),
   if
    Ans =:= "y"; Ans =:= "Y" ->
	
     % If the operation can proceed, ensure the Mnesia "dir" environment variable to be set
     application:set_env(mnesia,dir,"priv/mnesia.db"),
	 ok;
	 
	true ->
	 aborted
   end
 end. 


%% DESCRIPTION:  Returns all keys in a table as a list
%%
%% ARGUMENTS:    - Table: The table which to retrieve the keys
%%
%% RETURNS:      The list of keys in the table
%%
%% THROWS:       none
get_table_keys(Table) ->
 F = fun() -> {mnesia:all_keys(Table)} end,
 {atomic,{Keys}} = mnesia:transaction(F),
 Keys.


%% DESCRIPTION:  Returns all keys in a table as a list
%%
%% ARGUMENTS:    - Table: The table which to retrieve the keys
%%
%% RETURNS:      The list of keys in the table
%%
%% THROWS:       none
get_table_keys_num(Table) ->
 Keys = get_table_keys(Table),
 length(Keys).


%% DESCRIPTION:  Returns all records in a table as a list
%%
%% ARGUMENTS:    - Table: The table which to retrieve the records
%%
%% RETURNS:      The list of records in the table
%%
%% THROWS:       none 
get_table_records(Table) ->
 mnesia:dirty_select(Table,[{'_',[],['$_']}]). % The second argument is a "catch-all" clause
  
  
%% DESCRIPTION:  Returns all unique users in the database
%%
%% ARGUMENTS:    none
%%
%% RETURNS:      All unique users in the database
%%
%% THROWS:       none 
get_all_users() ->
 MatchHead = #location{user='$1', _='_'},
 Guard = [],
 Result = '$1',
 UnfilteredUserList = mnesia:dirty_select(location,[{MatchHead, Guard, [Result]}]),
 lists:usort(UnfilteredUserList).
 

%% DESCRIPTION:  Returns the Tabletype associated to its argument, also considering shorthand forms
%%
%% ARGUMENTS:    - Tabletybe: A table type, possibly in a shorthand form
%%
%% RETURNS:      The Tabletype associated to its argument, also considering shorthand forms
%%
%% THROWS:       none 
resolve_tabletype_shorthand(Tabletype) ->
 if
  Tabletype =:= loc orelse Tabletype =:= location ->
   location;
  Tabletype =:= sub orelse Tabletype =:= subloc orelse Tabletype =:= sublocation ->
   sublocation;
  Tabletype =:= dev orelse Tabletype =:= device ->
   device;
  true ->
   unknown
 end.