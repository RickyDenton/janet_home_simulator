-module(db).
-include("table_records.hrl"). % Mnesia table records definition

%% ------ EXPORTED CRUD OPERATIONS ------ %%
-export([add_location/4,add_sublocation/2,add_device/4]).                                                 % Create
-export([print_table/1,print_tree/1,print_tree/2,find_record/2,get_table_keys/1,get_records_num/1]).      % Read
-export([update_dev_sub/2,update_dev_config/2,update_loc_name/2,update_subloc_name/2,update_dev_name/2]). % Update
-export([delete_location/1,delete_sublocation/1,delete_device/1]).                                        % Delete

%% ----- EXPORTED UTILITY FUNCTIONS ----- %%
-export([backup/0,backup/1,restore/0,restore/1,clear/0,install/0]).


%%====================================================================================================================================
%%                                                        CRUD OPERATIONS
%%==================================================================================================================================== 

%% ========================================================== CREATE ===============================================================%%

%% DESCRIPTION:  Adds an empty location to the database (containing only the "(default)" sublocation {Loc_id,0}),
%%               and starts up its controller
%%
%% ARGUMENTS:    - Loc_id: The ID of the location to add, which must not already exist and must be >0
%%               - Name:   The name of the location (optional)
%%               - User:   The username of the location's owner (optional)
%%               - Port:   The port by which the location's controller listens for REST requests, which must
%%                         not be already taken and must be >0
%%
%% RETURNS:      - {atomic, ok} if the location was successfully added
%%               - {error,location_already_exists} if such Loc_id already exists in the "location" table 
%%               - {error,port_already_taken} if such port is already used by another controller
%%               - {error,badarg} if wrong argument format
%%
%% THROWS:       none 
add_location(Loc_id,Name,User,Port) when is_number(Loc_id), Loc_id>0, is_number(Port), Port>0 ->
 F = fun() ->
 
      % Check if the location already exists
	  case mnesia:read({location,Loc_id}) of
	   [] ->
	    
		% Check if the port is already taken
		case mnesia:match_object(#location{port = Port, _ = '_'}) of
		 [] ->
		 
		  % If empty, use a default name
		  if
		   Name =:= "" ->
		    LocName = "loc-" ++ integer_to_list(Loc_id);
		   true ->
		    LocName = Name
		  end,
		 
		  % Insert the new location and its default sublocation
	      mnesia:write(#location{loc_id=Loc_id,name=LocName,user=User,port=Port}),
		  mnesia:write(#sublocation{sub_id={Loc_id,0}, name="(default)", devlist=[]});
		  
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


%% DESCRIPTION:  Adds an empty sublocation to the database
%%
%% ARGUMENTS:    - {Loc_id,Subloc_id}: The sub_id of the sublocation, which must not already exist and with
%%                                     both elements >0
%%               - Name: The name of the sublocation (optional)
%%
%% RETURNS:      - {atomic, ok} if the sublocation was successfully added
%%               - {error,location_not_exists} If the location refered by the Loc_id does not exist
%%               - {error,sublocation_already_exists} If the sub_id already exists in the sublocation table
%%               - {error,badarg} if wrong argument format
%%
%% THROWS:       none 
add_sublocation({Loc_id,Subloc_id},Name) when is_number(Loc_id), Loc_id>0, is_number(Subloc_id), Subloc_id>0 ->
 F = fun() ->
 
      % Check the location to exist
	  case mnesia:read({location,Loc_id}) of
	   [_LocationRecord] ->
	    
		% Check if a sublocation with the same Subloc_id already exists
		case mnesia:read({sublocation,{Loc_id,Subloc_id}}) of
		 [] ->
		 
		  % If empty, use a default name
		  if
		   Name =:= "" ->
		    SublocName = "subloc-{" ++ integer_to_list(Loc_id) ++ "," ++ integer_to_list(Subloc_id) ++ "}";
		   true ->
		    SublocName = Name
		  end,
		  
		  % Insert the new sublocation
	      mnesia:write(#sublocation{sub_id={Loc_id,Subloc_id}, name=SublocName, devlist=[]});
		  
		 [_SublocationRecord] ->
		  mnesia:abort(sublocation_already_exists)
		end;
		
	   [] ->
	    mnesia:abort(location_not_exists)
      end
     end,
	 
 Result = mnesia:transaction(F),
 case Result of
  {atomic, ok} ->
   %% [TODO]: Inform controller of the new sublocation
   Result;   
  _ ->
   Result
 end;

add_sublocation(_,_) ->
 {error,badarg}.


%% DESCRIPTION:  Adds a new device with a default configuration to the database, also starting its associated VM
%%
%% ARGUMENTS:    - Dev_id: The ID of the device, which must not already exist and must be >0
%%               - Name: The device's name (optional)
%%               - {Loc_id,Subloc_id}: The sub_id of the sublocation where to put the device, which must exist
%%                                     and be >=0
%%               - Type: The device's type, which must belong to the set of allowed device types
%%
%% RETURNS:      - {atomic, ok} if the device was successfully added
%%               - {error,devtype_not_exists} if the device type is unknown
%%               - {error,device_already_exists} if a device with such Dev_id already exists  
%%               - {error,sublocation_not_exists} If the sublocation where to put the device doesn't exist
%%               - {error,badarg} if wrong argument format
%%
%% THROWS:       none 
add_device(Dev_id,Name,{Loc_id,Subloc_id},Type) when is_number(Dev_id), Dev_id>0, is_number(Loc_id), Loc_id>0, is_number(Subloc_id), Subloc_id>=0 ->
 F = fun() ->
 
      % Check the target sublocation to exist
	  case mnesia:read({sublocation,{Loc_id,Subloc_id}}) of
	   [Sublocation] ->
	    
		% Check if a device with the same Dev_id already exists
		case mnesia:read({device,Dev_id}) of
		 [] ->
		 
		  % If empty, use a default name
		  if
		   Name =:= "" ->
		    DevName = "dev-" ++ integer_to_list(Dev_id);
		   true ->
		    DevName = Name
		  end,
		  
		  % Insert the new device in the Device table
	      mnesia:write(#device{dev_id=Dev_id, name=DevName, sub_id={Loc_id,Subloc_id}, type=Type, config=devutils:get_devtype_default_config(Type)}),
		  
		  % Update the "DevList" in the sublocation table  
		  UpdatedDevList = lists:append(Sublocation#sublocation.devlist,[Dev_id]),
		  UpdatedSublocation = Sublocation#sublocation{devlist=UpdatedDevList},
		  mnesia:write(UpdatedSublocation);
		  
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

add_device(_,_,_,_) ->
 {error,badarg}.


%% =========================================================== READ ================================================================%% 

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
 
 % Print all tables' headers and contents
 io:format("~n"),
 print_table_header(location),
 print_table_records(get_table_records(location)),
 print_table_header(sublocation),
 print_table_records(get_table_records(sublocation)),
 print_table_header(device),
 print_table_records(get_table_records(device)),
 print_table_header(suploc),
 print_table_records(get_table_records(suploc)),
 print_table_header(ctrmanager),
 print_table_records(get_table_records(ctrmanager)),
 print_table_header(devmanager),
 print_table_records(get_table_records(devmanager));

print_table(Tabletype) when is_atom(Tabletype) ->

 % Resolve possible table shorthand forms
 Table = resolve_tabletype_shorthand(Tabletype),
 
 case Table of
 
  % If unknown table, return an error
  unknown ->
   {error,unknown_table};
   
  % Otherwise print the table header and contents
  _->
   io:format("~n"),
   print_table_header(Table),
   print_table_records(get_table_records(Table))
 end;
 
print_table(_) ->
 {error,badarg}.

%% Prints the header of a table (print_table() helper function) 
print_table_header(location) ->
 io:format("LOCATION TABLE {loc_id,name,user,port}~n==============~n");
print_table_header(sublocation) ->
 io:format("SUBLOCATION TABLE {sub_id,name}~n=================~n");
print_table_header(device) -> 
 io:format("DEVICE TABLE {dev_id,name,sub_id,type,config}~n============~n");
print_table_header(suploc) -> 
 io:format("SUPLOC TABLE {loc_id,sup_pid}~n============~n");
print_table_header(ctrmanager) -> 
 io:format("CTRMANAGER TABLE {loc_id,sup_pid,status}~n================~n");
print_table_header(devmanager) -> 
 io:format("DEVMANAGER TABLE {dev_id,sup_pid,status}~n================~n").

%% Prints the records in a table (print_table() helper function)  
print_table_records([]) ->
 io:format("~n");
print_table_records([H|T]) ->
 io:format("~s~n",[io_lib:format("~p",[H])]),
 print_table_records(T).
 

%% DESCRIPTION:  Prints database contents indented as a tree
%%
%% ARGUMENTS:    - (all):                prints all locations, sublocations and devices of all users in the database
%%               - (user,Username):      prints all locations, sublocations and devices of a specific user in the database
%%               - (location,Loc_id):    prints all sublocations and devices in a specific location
%%               - (sublocation,Sub_id): prints all devices in a specific sublocation
%%
%% RETURNS:      - A view of the database indented as a tree, depending on the arguments
%%               - user_not_found: if with the print(user,Username) syntax the user was not found in the database
%%               - {error,unsupported}: if with the print(Tabletype,Id) syntax the "device" table was passed
%%               - {error,unknown_table}: if with the print(Tabletype,Id) syntax the table was not found in the database
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
   io:format("The database is empty~n")
 end;

print_tree(_) ->
 {error, badarg}.


print_tree(user,Username) ->
 
 % Check the user to exist
 UserLocs = mnesia:dirty_match_object(#location{user = Username, _ = '_'}),

 case UserLocs of
 
  % If the user was not found, return
  [] ->
   user_not_found;
   
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
 
 % Retrieve the location controller's status from the ctrmanager table
 ReadMgrRecord = mnesia:dirty_read({ctrmanager,Loc#location.loc_id}),
 case ReadMgrRecord of
 
  % If the record exists in the ctrmanager table
  [CtrMgrRecord] ->
   CtrMgrStatus = CtrMgrRecord#ctrmanager.status;
   
  % Otherwise, if the record doesn't exist
  [] ->
   CtrMgrStatus = "NOT_STARTED"
 end,
 
 % Print information on the location
 io:format("~s~s - ~s~n",[Indent,io_lib:format("~p",[Loc]),CtrMgrStatus]),
 
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

 % Retrieve the device's status from the devmanager table
 ReadMgrRecord = mnesia:dirty_read({devmanager,Dev#device.dev_id}),
 case ReadMgrRecord of
 
  % If the record exists in the devmanager table
  [DevMgrRecord] ->
   DevMgrStatus = DevMgrRecord#devmanager.status;
   
  % Otherwise, if the record doesn't exist
  [] ->
   DevMgrStatus = "NOT_STARTED"
 end,
 
 % Print information on the device
 io:format("~s~s - ~s~n",[Indent,io_lib:format("~p",[Dev]),DevMgrStatus]),
 print_tree_device(NextDev,Indent).


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
find_record(Tabletype,Id) when is_atom(Tabletype) ->

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
	 not_found
   end
 end;

find_record(_,_) ->
 {error,badarg}.


%% DESCRIPTION:  Returns all keys in a table as a list
%%
%% ARGUMENTS:    - Tabletype: The table which to retrieve the keys, or its shorthand form
%%
%% RETURNS:      The list of keys in the table identified by Tabletype
%%
%% THROWS:       none
get_table_keys(Tabletype) ->

 % Resolve possible table shorthand forms
 Table = resolve_tabletype_shorthand(Tabletype),

 case Table of
 
  % If unknown table, return an error
  unknown ->
   {error,unknown_table};
  
  % Otherwise retrieve and return the table keys
  _ ->
   {atomic,{Keys}} = mnesia:transaction(fun() -> {mnesia:all_keys(Table)} end),
   Keys
 end.


%% DESCRIPTION:  Returns the number of records in a specific or all database tables
%%
%% ARGUMENTS:    - (all): Returns the number of records of all database tables
%%               - (Tabletype): The table which to retrieve the number of records, or its shorthand form
%%
%% RETURNS:      The number of records a specific or all database tables
%%
%% THROWS:       none  
get_records_num(all) ->
 {get_records_num(location),get_records_num(sublocation),get_records_num(device),get_records_num(suploc),get_records_num(ctrmanager),get_records_num(devmanager)};
get_records_num(Tabletype) ->
 
 % Retrieve the table keys, also considering shorthand forms
 Keys = get_table_keys(Tabletype),
 
 case Keys of
   
  % If the table was not found, return an error
  {error,unknown_table} ->
   {error,unknown_table};
   
  % Otherwise return the number of keys in the table
  _ ->
  length(Keys)
 end.
 

%% ========================================================== UPDATE ===============================================================%% 

%% DESCRIPTION:  Changes a device from a sublocation to another
%%
%% ARGUMENTS:    - Dev_id: The ID of the device to change sublocation, which must exist and be >0
%%               - {Loc_id,Subloc_id}: The sub_id of the sublocation where to put the device in, whose elements must be >0
%%
%% RETURNS:      - {atomic, ok} if the device's sublocation was successfully changed
%%               - {error,device_not_exists} if the device was not found in the device table
%%               - {error,sublocation_not_exists} if the target sublocation was not found in the sublocation table
%%               - {error,badarg} if wrong argument format
%%
%% THROWS:       none 
%%
%% NOTE:         This function doesn't check that the current and new sublocation belong to the same location
update_dev_sub(Dev_id,{Loc_id,Subloc_id}) when is_number(Dev_id), Dev_id>0, is_number(Loc_id), Loc_id>0, is_number(Subloc_id), Subloc_id>=0 ->
 F = fun() ->
 
      % Check the device to exist
	  case mnesia:wread({device,Dev_id}) of      % wread = write lock
	   [Device] ->
	   
	    % Retrieve the record of the device's current sublocation
	    CurrSublocID = Device#device.sub_id,
	
		if 
		
		 % If the current and new sublocations coincide, return
		 CurrSublocID =:= {Loc_id,Subloc_id} ->  
		  ok;

         % Otherwise, check the target sublocation to exist
		 true ->
		  case mnesia:wread({sublocation,{Loc_id,Subloc_id}}) of
		   [NewSubloc] ->
		   
		    % Remove the device from its current sublocation
			[CurrSubloc] = mnesia:wread({sublocation,CurrSublocID}),		
			UpdatedCurrSublocDevlist = lists:delete(Dev_id,CurrSubloc#sublocation.devlist),
			UpdatedCurrSubloc = CurrSubloc#sublocation{devlist=UpdatedCurrSublocDevlist},
			mnesia:write(UpdatedCurrSubloc),
			
			% Insert the device in its new sublocation
			UpdatedNewSublocDevList = lists:append(NewSubloc#sublocation.devlist,[Dev_id]),
			UpdatedNewSubloc = NewSubloc#sublocation{devlist = UpdatedNewSublocDevList},
			mnesia:write(UpdatedNewSubloc),
			
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
	 
 Result = mnesia:transaction(F),
 case Result of
  {atomic, ok} ->
   %% [TODO]: Inform controller of the change of sublocation for the device
   Result;   
  _ ->
   Result
 end;
	 
update_dev_sub(_,_) ->
 {error,badarg}.


%% DESCRIPTION:  Updates a device's configuration
%%
%% ARGUMENTS:    - Dev_id: The ID of the device to update the configuration, which must exist and be >0
%%               - Config: The updated device configuration
%%
%% RETURNS:      - {atomic, ok} if the device's configuration was successfully updated
%%               - {error,device_not_exists} if the device was not found in the device table
%%               - {error,badarg} if wrong argument format
%%
%% THROWS:       none 
%%
%% NOTE:         This function doesn't check the new configuration to be appropriate for the device type
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


%% DESCRIPTION:  Updates a location's name
%%
%% ARGUMENTS:    - Loc_id: The ID of the location which update the name, which must exist and be >0
%%               - Name: The updated location name
%%
%% RETURNS:      - {atomic, ok} if the location's name was successfully updated
%%               - {error,location_not_exists} if the location was not found in the location table
%%               - {error,badarg} if wrong argument format
%%
%% THROWS:       none
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


%% DESCRIPTION:  Updates a sublocation's name
%%
%% ARGUMENTS:    - {Loc_id,Subloc_id}: The sub_id of the sublocation which update the name, which must exist and with
%%                                     both elements >0
%%               - Name: The updated sublocation name
%%
%% RETURNS:      - {atomic, ok} if the sublocation's name was successfully updated
%%               - {error,sublocation_not_exists} if the sublocation was not found in the sublocation table
%%               - {error,badarg} if wrong argument format
%%
%% THROWS:       none
%%
%% NOTE:         The name of the default sublocation cannot be changed (>0)
update_subloc_name({Loc_id,Subloc_id},Name) when is_number(Loc_id), Loc_id>0, is_number(Subloc_id), Subloc_id>0 -> 
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
 

%% DESCRIPTION:  Updates a device's name
%%
%% ARGUMENTS:    - Dev_id: The dev_id of the device which update the name, which must exist and be >0
%%               - Name: The updated device name
%%
%% RETURNS:      - {atomic, ok} if the device's name was successfully updated
%%               - {error,device_not_exists} if the device was not found in the device table
%%               - {error,badarg} if wrong argument format
%%
%% THROWS:       none
update_dev_name(Dev_id,Name) when is_number(Dev_id), Dev_id>0 ->
 F = fun() ->
 
      % Check the device to exist
	  case mnesia:wread({device,Dev_id}) of      % wread = write lock
	   [Device] ->
		if 
		
		 % If the new and old names coincide, return
		 Device#device.name =:= Name ->  
		  ok;

         % Otherwise, update the device's name
		 true ->
		  UpdatedDevice = Device#device{name=Name},
		  mnesia:write(UpdatedDevice)
	    end;
		
	   [] ->
	    mnesia:abort(device_not_exists)
      end
     end,
 mnesia:transaction(F);	 

update_dev_name(_,_) ->
 {error,badarg}.
 
%% ========================================================== DELETE ===============================================================%% 

%% DESCRIPTION:  Deletes a location, along with all its sublocations and devices, from the database, also stopping the associated VMs
%%
%% ARGUMENTS:    - Loc_id: The ID of the location to delete
%%
%% RETURNS:      - {atomic, ok} if the location was successfully removed from the database
%%               - {error,location_not_exists} if the location was not found in the location table
%%               - {error,badarg} if wrong argument format
%%
%% THROWS:       none
delete_location(Loc_id) when is_number(Loc_id), Loc_id>0 ->
 F = fun() ->
 
      % Check the location to exist
	  case mnesia:wread({location,Loc_id}) of      % wread = write lock
	   [_Location] ->
		
		% Delete all devices in the location from the device table
		LocationDevList = mnesia:match_object(#device{sub_id = {Loc_id,'_'}, _ = '_'}),
		delete_device_records(LocationDevList),
		
		% Delete all sublocations in the location from the sublocation table
		LocationSublocList = mnesia:match_object(#sublocation{sub_id = {Loc_id,'_'}, _ = '_'}),
		delete_subloc_records(LocationSublocList),
		
		% Delete the location from the location table
		mnesia:delete({location,Loc_id});
		
	   [] ->
	    mnesia:abort(location_not_exists)
      end
     end,
 Result = mnesia:transaction(F),
 case Result of
  {atomic, ok} ->
   %% [TODO]: Shut down and remove the entire location from the supervision tree
   Result;   
  _ ->
   Result	 
 end; 

delete_location(_) ->
 {error,badarg}.


%% Deletes a list of device records from the device table (delete_location(Loc_id) helper function)
delete_device_records([]) ->
 ok;
delete_device_records([Dev|NextDev]) ->
 mnesia:delete({device,Dev#device.dev_id}),
 delete_device_records(NextDev).
 
 
%% Deletes a list of sublocation records from the sublocation table (delete_location(Loc_id) helper function)
delete_subloc_records([]) ->
 ok;
delete_subloc_records([Subloc|NextSubloc]) ->
 mnesia:delete({sublocation,Subloc#sublocation.sub_id}),
 delete_subloc_records(NextSubloc). 


%% DESCRIPTION:  Delete a sublocation from the database, moving all its devices to its location's default sublocation {Loc_id,0}
%%
%% ARGUMENTS:    - {Loc_id,Subloc_id}: The sub_id of the sublocation to delete, with both elements >0
%%
%% RETURNS:      - {atomic, ok} if the sublocation was successfully removed from the database
%%               - {error,sublocation_not_exists} if the sublocation was not found in the sublocation table
%%               - {error,badarg} if wrong argument format
%%
%% THROWS:       none
%%
%% NOTE:         The default sublocation of a location cannot be removed (Subloc_id > 0)
delete_sublocation({Loc_id,Subloc_id}) when is_number(Loc_id), Loc_id>0, is_number(Subloc_id), Subloc_id>0 -> 
 F = fun() ->
 
      % Check the sublocation to exist
	  case mnesia:wread({sublocation,{Loc_id,Subloc_id}}) of      % wread = write lock
	   [Sublocation] ->

		 % Move all the sublocation's devices to the (default) sublocation {Loc_id,0}
		 SublocDevList = Sublocation#sublocation.devlist,
		 [DefaultSubloc] = mnesia:wread({sublocation,{Loc_id,0}}),
		 UpdatedDefaultSublocDevlist = lists:append(DefaultSubloc#sublocation.devlist,SublocDevList),
		 UpdatedDefaultSubloc = DefaultSubloc#sublocation{devlist=UpdatedDefaultSublocDevlist},
		 mnesia:write(UpdatedDefaultSubloc),
		 
		 % Update the devices' sub_ids in the device table
		 move_devlist_to_default_subloc(SublocDevList,Loc_id),
		 
		 % Remove the sublocation from the sublocation table
		 mnesia:delete({sublocation,{Loc_id,Subloc_id}});
		
	   [] ->
	    mnesia:abort(sublocation_not_exists)
      end
     end,
 Result = mnesia:transaction(F),
 case Result of
  {atomic, ok} ->
   %% [TODO]: Inform the controller that the sublocation no longer exists and so to change the devices to the default sublocation
   Result;   
  _ ->
   Result	 
 end;
 
delete_sublocation(_) ->
 {error,badarg}.


%% Moves all devices in a list to the default sublocation {Loc_id,0} (delete_sublocation(Loc_id,Subloc_id) helper function)
move_devlist_to_default_subloc([],_) ->
 ok;
move_devlist_to_default_subloc([Dev_id|NextDev_id],Loc_id) ->
 [Device] = mnesia:wread({device,Dev_id}),
 UpdatedDevice = Device#device{sub_id={Loc_id,0}},
 mnesia:write(UpdatedDevice),
 move_devlist_to_default_subloc(NextDev_id,Loc_id).


%% DESCRIPTION:  Deletes a device from the database, also shutting down its VM in its location
%%
%% ARGUMENTS:    - Dev_id: The dev_id of the device to delete, which must exist and be >0
%%
%% RETURNS:      - {atomic, ok} if the device's name was successfully deleted from the database
%%               - {error,device_not_exists} if the device was not found in the device table
%%               - {error,badarg} if wrong argument format
%%
%% THROWS:       none
delete_device(Dev_id) when is_number(Dev_id), Dev_id>0 ->
 F = fun() ->
 
      % Check the device to exist
	  case mnesia:wread({device,Dev_id}) of      % wread = write lock
	   [Device] ->

		 % Remove the device from the devlist of its sublocation
		 [DeviceSubloc] = mnesia:wread({sublocation,Device#device.sub_id}),
		 UpdatedDeviceSublocDevlist = lists:delete(Dev_id,DeviceSubloc#sublocation.devlist),
		 UpdatedDeviceSubloc = DeviceSubloc#sublocation{devlist=UpdatedDeviceSublocDevlist},
		 mnesia:write(UpdatedDeviceSubloc),
		 
		 % Remove the device from the device table
		 mnesia:delete({device,Dev_id});
		
	   [] ->
	    mnesia:abort(device_not_exists)
      end
     end,
 Result = mnesia:transaction(F),
 case Result of
  {atomic, ok} ->
   %% [TODO]: Stop the device and remove it from the supervision tree
   %% [TODO]: Inform the controller that the device no longer exists
   Result;   
  _ ->
   Result
 end; 

delete_device(_) ->
 {error,badarg}.

%%====================================================================================================================================
%%                                                    PUBLIC UTILITY FUNCTIONS
%%==================================================================================================================================== 
 
%% DESCRIPTION:  Backups the entire database contents to a file
%%
%% ARGUMENTS:    - (none): The database is backed up to the default file ("db/mnesia_backup.db")
%%               - (File): The file where to backup the database
%%
%% RETURNS:      - ok: Database successfully backed up to the specified file
%%               - {error,Reason}: Error in writing the backup file
%%
%% THROWS:       none 
backup() ->
 backup("db/mnesia_backup.db").  % Default backup file
 
backup(File) ->

 % Backup the database to the specified file
 BackupResult = mnesia:backup(File),
 case BackupResult of
  ok ->
   io:format("Mnesia database backed up to file \"~s\"~n",[File]);
  _ ->
   BackupResult
 end.


%% DESCRIPTION:  Restores the database to the contents of a backup file
%%
%% ARGUMENTS:    - (none): The default file is used for restoring the database ("db/mnesia_backup.db")
%%               - (File): The file where to restore the database from
%%
%% RETURNS:      - ok: Database successfully restored to the contents of the specified backup file
%%               - {error,Reason}: Error in restoring the database from the backup file
%%               - janet_running: The JANET simulator is running, thus the operation cannot be performed
%%               - aborted: The user aborted the operation
%%
%% THROWS:       none
%%
%% NOTE:         The current database contents will be DISCARDED by calling this function
restore() ->
 restore("db/mnesia_backup.db").
 
restore(File) ->
 % Check if the operation can be performed
 CheckOp = check_db_operation("Restoring"),

 case CheckOp of
 
  % If the operation can be performed
  ok ->
  
   % Start Mnesia for restoring the database to the contents of the specified backup file
   ok = application:start(mnesia),
   RestoreResult = mnesia:restore(File,[{default_op,recreate_tables}]),
   ok = application:stop(mnesia),
   
   % Inform the user of the result of the operation
   case RestoreResult of
    {atomic, _} ->
	 io:format("Mnesia database successfully restored to the contents of the \"~s\" file~n",[File]);
	_ ->
	 RestoreResult
   end;
   
  _ ->
   CheckOp   
 end.
 

%% DESCRIPTION:  Clears all database tables, preserving its schema
%%
%% ARGUMENTS:    none
%%
%% RETURNS:      - ok: Database tables successfully cleared
%%               - janet_running: The JANET simulator is running, thus the operation cannot be performed
%%               - aborted: The user aborted the operation
%%
%% THROWS:       none
%%
%% NOTE:         Debug purposes only (use restore() to reset the database) 
clear() ->
 % Check if the operation can be performed
 CheckOp = check_db_operation("Clearing"),

 case CheckOp of
 
  % If the operation can be performed, start Mnesia and clear all disc_copies tables
  ok ->
   jsim:start_mnesia(),
   [{atomic,ok},{atomic,ok},{atomic,ok}] = [mnesia:clear_table(location),mnesia:clear_table(sublocation),mnesia:clear_table(device)],
   application:stop(mnesia),
   io:format("Mnesia tables successfully cleared~n");
   
  % Otherwise return CheckOp
  _ ->
   CheckOp
 end.
 
 
%% DESCRIPTION:  Installs the Mnesia database (schema + tables) from scratch
%%
%% ARGUMENTS:    none
%%
%% RETURNS:      - ok: Database successfully installed
%%               - janet_running: The JANET simulator is running, thus the operation cannot be performed
%%               - aborted: The user aborted the operation
%%
%% THROWS:       none
%%
%% NOTE:         Debug purposes only (use restore() to reset the database) 
install() ->

 % Check if the operation can be performed
 CheckOp = check_db_operation("Installing"),

 case CheckOp of
 
  % If the operation can be performed, install the mnesia database
  ok ->
  
   % Stop Mnesia and reset its schema
   application:stop(mnesia),
   ok = mnesia:delete_schema([node()]),
   ok = mnesia:create_schema([node()]),
   
   % Start Mnesia again and create the tables
   ok = application:start(mnesia),
	 
   % --- disc_copies tables --- %   
   {atomic,ok} = mnesia:create_table(location,
                                    [{attributes, record_info(fields, location)},
                                    {index, [#location.user,#location.port]},
                                    {disc_copies, [node()]}]),				 
   {atomic,ok} = mnesia:create_table(sublocation,
                                    [{attributes, record_info(fields, sublocation)},
					                {type, ordered_set},
                                    {disc_copies, [node()]}]),		 
   {atomic,ok} = mnesia:create_table(device,
                                    [{attributes, record_info(fields, device)},
                                    {index, [#device.sub_id]},
                                    {disc_copies, [node()]}]),

   % --- disc_copies tables --- %   
   {atomic,ok} = mnesia:create_table(suploc,
                                    [{attributes, record_info(fields, suploc)},
                                    {ram_copies, [node()]}]),
   {atomic,ok} = mnesia:create_table(ctrmanager,
                                    [{attributes, record_info(fields, ctrmanager)},
                                    {ram_copies, [node()]}]),
   {atomic,ok} = mnesia:create_table(devmanager,
                                    [{attributes, record_info(fields, devmanager)},
                                    {ram_copies, [node()]}]),
		
   % Stop Mnesia and return		
   ok = application:stop(mnesia),
   io:format("Mnesia database successfully installed~n");
 
  _ ->
   CheckOp
 end.
 
 
%%====================================================================================================================================
%%                                                 PRIVATE UTILITY FUNCTIONS
%%==================================================================================================================================== 
 
 
%% DESCRIPTION:  Checks the JANET simulator to be stopped and asks user confirmation before attempting a database utility operation
%%
%% ARGUMENTS:    - Operation: a String used in asking user confirmation
%%
%% RETURNS:      - ok: The user confirmed the database utility operation
%%               - janet_running: The JANET simulator is running, thus the operation cannot be performed
%%               - aborted: The user aborted the operation
%%
%% THROWS:       none  
check_db_operation(Operation) ->

 % Check the JANET simulator not to be running
 case jsim:is_running(janet_simulator) of
 
  true ->
   io:format("Please stop the JANET Simulator first ~n"),
   janet_running;
   
  false ->
   
   % Ask user confirmation for the operation
   io:format("~s",[Operation]),
   {ok,[Ans]} = io:fread(" the Mnesia database may cause inconsistencies with the remote database. Are you sure you want to proceed? (y/N): ","~s"),
   if
    Ans =:= "y"; Ans =:= "Y" ->
	
     % If the operation can proceed, ensure the Mnesia "dir" environment variable to be set
     %application:set_env(mnesia,dir,"db/mnesia.db"),
	 ok;
	 
	true ->
	 aborted
   end
 end. 


%% DESCRIPTION:  Returns all records in a table as a list
%%
%% ARGUMENTS:    - Tabletype: The table which to retrieve the records, or its shorthand form
%%
%% RETURNS:      The list of records in the table identified by Tabletype
%%
%% THROWS:       none 
get_table_records(Tabletype) ->

 % Resolve possible table shorthand forms
 Table = resolve_tabletype_shorthand(Tabletype),

 case Table of
 
  % If unknown table, return an error
  unknown ->
   {error,unknown_table};
  
  % Otherwise return all table records
  _ ->
   mnesia:dirty_select(Table,[{'_',[],['$_']}])  % The second argument is a "catch-all" clause
 end.
 
  
%% DESCRIPTION:  Returns all unique users in the database
%%
%% ARGUMENTS:    none
%%
%% RETURNS:      All unique users in the database
%%
%% THROWS:       none 
get_all_users() ->

 % Initialize select arguments
 MatchHead = #location{user='$1', _='_'}, % Consider all locations
 Guard = [],                              % No guard
 Result = '$1',                           % Return only the location's users
 
 % Retrieve the users of all locations in the database
 UnfilteredUserList = mnesia:dirty_select(location,[{MatchHead, Guard, [Result]}]),
 
 % Remove duplicates and return the list of users
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
  % --- disc_copies tables --- %
  Tabletype =:= loc orelse Tabletype =:= location ->
   location;
  Tabletype =:= sub orelse Tabletype =:= subloc orelse Tabletype =:= sublocation ->
   sublocation;
  Tabletype =:= dev orelse Tabletype =:= device ->
   device;
  
  % --- ram_copies tables --- %
  Tabletype =:= suploc orelse Tabletype =:= sup ->
   suploc;
  Tabletype =:= ctrmanager orelse Tabletype =:= ctrmgr orelse Tabletype =:= ctr orelse Tabletype =:= controllermgr orelse Tabletype =:= controllermanager ->
   ctrmanager;
  Tabletype =:= devmanager orelse Tabletype =:= devmgr orelse Tabletype =:= devicemgr orelse Tabletype =:= devicemanager ->
   devmanager;
   
  true ->
   unknown
 end.