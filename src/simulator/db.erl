%% This module offers functions for interfacing with the Mnesia database on the Janet Simulator node %%

-module(db).

-include("sim_mnesia_tables_definitions.hrl").  % Janet Simulator Mnesia Tables Records Definitions

%% ------------------------------------- PUBLIC CRUD OPERATIONS ------------------------------------- %%
-export([add_location/4,add_sublocation/2,add_device/4]).                                                  % Create
-export([print_table/0,print_table/1,print_tree/0,print_tree/1,print_tree/2,get_record/2,                  % Read
         get_table_records/1,get_table_keys/0,get_table_keys/1,get_records_num/0,get_records_num/1,
		 get_loc_devs/1,get_subloc_devs/1,get_manager_info/2,get_suploc_pid/1]).  
-export([update_dev_sub/2,update_dev_config/3,update_loc_name/2,update_subloc_name/2,update_dev_name/2]).  % Update
-export([delete_location/1,delete_sublocation/1,delete_device/1]).                                         % Delete

%% ------------------------------------ PUBLIC UTILITY FUNCTIONS ------------------------------------ %%
-export([start_mnesia/0,stop_mnesia/0,backup/0,backup/1,restore/0,restore/1,clear/0,install/0]). 


%%====================================================================================================================================
%%                                                     PUBLIC CRUD OPERATIONS                                                        
%%==================================================================================================================================== 

%% NOTE: All the public CRUD functions crash if Mnesia is NOT started on the node (which is automatically performed by the 'init' process at startup)

%% ========================================================== CREATE ===============================================================%%

%% DESCRIPTION:  Adds an empty location to the database with its (default) sublocation {Loc_id,0}, and starts up its controller
%%
%% ARGUMENTS:    - Loc_id: The ID of the location to add, which must not already exist and be >0
%%               - Name:   The name of the location (optional)
%%               - User:   The username of the location's owner (optional)
%%               - Port:   The port by which the location's controller listens for REST requests, which must not be already taken and be >=30000
%%
%% RETURNS:      - {ok,ok}                         -> The location was successfully added and its controller node was started
%%               - ok                              -> The location was successfully added (but the controller node
%%                                                    was not started since the JANET Simulator is not running)
%%               - {error,location_already_exists} -> The loc_id already exists in the "location" table 
%%               - {error,port_already_taken}      -> The port is already used by another controller
%%               - {error,badarg}                  -> Invalid arguments
%%
add_location(Loc_id,Name,User,Port) when is_number(Loc_id), Loc_id>0, is_number(Port), Port >= 30000 ->
 F = fun() ->
 
      % Check if the location already exists
	  case mnesia:read({location,Loc_id}) of
	   [] ->
	    
		% Check if the port is already taken
		case mnesia:match_object(#location{port = Port, _ = '_'}) of
		 [] ->
		 
		  % If no location name was provided, use a default one
		  if
		   Name =:= "" ->
		    LocName = "loc-" ++ integer_to_list(Loc_id);
		   true ->
		    LocName = Name
		  end,
		 
		  % If no user name was provided, use a default one
		  if
		   User =:= "" ->
		    LocUser = "(anonymous)";
		   true ->
		    LocUser = User
		  end, 
		 
		  % Insert the new location and its default sublocation
	      mnesia:write(#location{loc_id=Loc_id,name=LocName,user=LocUser,port=Port}),
		  mnesia:write(#sublocation{sub_id={Loc_id,0}, name="(default)", devlist=[]});
		  
		 [_LocationRecord] ->
		  mnesia:abort(port_already_taken)
		end;
		
	   [_LocationRecord] ->
	    mnesia:abort(location_already_exists)
      end
     end,
		
 case {mnesia:transaction(F),utils:is_running(janet_simulator)} of
  {{atomic,ok},true} ->
  
   % If the transaction was successfully and the JANET simulator is
   % running, spawn the 'sup_loc' supervisor associated with its "Loc_id"
   {SpawnRes,_} = supervisor:start_child(sup_locs,[Loc_id]),
 
   % Return the result of the transaction and
   % of the spawning of the 'sup_loc' supervisor
   {ok,SpawnRes};
   
  {{atomic,ok},false} ->
	
   % If the transaction was successful but the JANET
   % simulator is not running, return just "ok"
   ok;
	 
  {{aborted,Reason},_} ->
	
   % If an error occured in the transaction, return it
   {error,Reason}
 end;
 
add_location(_,_,_,_) ->
 {error,badarg}.


%% DESCRIPTION:  Adds an empty sublocation to the database
%%
%% ARGUMENTS:    - {Loc_id,Subloc_id}: The sub_id of the sublocation, which must not already exist and be >0
%%               - Name:               The name of the sublocation (optional)
%%
%% RETURNS:      - ok                                 -> The sublocation was successfully added
%%               - {error,location_not_exists}        -> The location 'Loc_id' does not exist
%%               - {error,sublocation_already_exists} -> A sublocation with such 'sub_id' already exists
%%               - {error,badarg}                     -> Invalid arguments
%%
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
 do_transaction(F);  %% [TODO]: If the transaction is successful, inform controller of the new sublocation

add_sublocation(_,_) ->
 {error,badarg}.


%% DESCRIPTION:  Adds a new device with a default configuration to the database, also starting its associated device node
%%
%% ARGUMENTS:    - Dev_id:             The ID of the device, which must not already exist and be >0
%%               - Name:               The device's name (optional)
%%               - {Loc_id,Subloc_id}: The device's sub_id, which must exist and with Subloc_id >=0
%%               - Type:               The device's type, which must belong to the set of valid device types
%%
%% RETURNS:      - {ok,ok}                        -> The device was successfully added and its device node was started
%%               - ok                             -> The device was successfully added (but the device node
%%                                                   was not started since the JANET Simulator is not running)
%%               - {error,invalid_devtype}        -> The device type is invalid
%%               - {error,device_already_exists}  -> A device with such 'dev_id' already exists 
%%               - {error,sublocation_not_exists} -> The 'sub_id' sublocation doesn't exist
%%               - {error,badarg}                 -> Invalid arguments
%%
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
	      mnesia:write(#device{dev_id=Dev_id, name=DevName, sub_id={Loc_id,Subloc_id}, type=Type, config=utils:get_devtype_default_config(Type), lastupdate = erlang:system_time(second)}),
		  
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
 case utils:is_valid_devtype(Type) of
 
  % If it is not, directly return an error 
  false ->
   {error,invalid_devtype};
  
  % If it is, attempt the transaction
  true -> 
   case {mnesia:transaction(F),utils:is_running(janet_simulator)} of
    {{atomic,ok},true} ->
  
     % If the transaction was successful and the JANET simulator is running,
	 % attempt to spawn the device manager under its 'sup_loc' location supervisor,
	 % returning the result of the transaction and of the spawn operation
     {ok,catch(spawn_devmanager(Dev_id,Loc_id))};
   
    {{atomic,ok},false} ->
	
	 % If the transaction was successful but the JANET
	 % simulator is not running, return just "ok"
     ok;
	 
	{{aborted,Reason},_} ->
	
	 % If an error occured in the transaction, return it
     {error,Reason}
   end
 end;
 
add_device(_,_,_,_) ->
 {error,badarg}.


%% Attempts to spawn a device manager under its location 'sup_loc' supervisor
%% (add_device(Dev_id,Name,{Loc_id,Subloc_id},Type) helper function)
spawn_devmanager(Dev_id,Loc_id) ->

 % Attempt to retrieve the PID of the location's 'sup_loc' supervisor
 Sup_pid = get_suploc_pid(Loc_id),
 
 % Device manager child specification
 DevMgrSpec = {
               "dev-" ++ integer_to_list(Dev_id),         % ChildID
               {dev_manager,start_link,[Dev_id,Loc_id]},  % Child Start Function
	           permanent,                                 % Child Restart Policy
	           8000,                                      % Child Sub-tree Max Shutdown Time
	           worker,                  	              % Child Type
	           [dev_manager]                              % Child Modules (For Release Handling Purposes)
              },

 % Attempt to spawn the device manager as a child of the 'sup_loc' supervisor
 case supervisor:start_child(Sup_pid,DevMgrSpec) of
  
  % If the spawning was successful, return just its result 
  {ok,_DevMgrPid} ->
   ok;
   
  % Otherwise, return the error raised in the spawning
  SpawnError ->
   SpawnError
 end.

 %% [TODO]: inform the controller of the new device (if not passing directly from it)


%% =========================================================== READ ================================================================%% 

%% DESCRIPTION:  Prints the contents of all or a specific table in the database
%%
%% ARGUMENTS:    - (Tabletype): The table to print, also considering shorthand forms
%%               - (),(all):    Print all tables in the database   
%%
%% RETURNS:      - ok                    -> The table(s) contents were printed
%%               - {error,unknown_table} -> Unknown table
%%               - {error,badarg}        -> Invalid arguments
%%
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
  {error,unknown_table} ->
   {error,unknown_table};
   
  % Otherwise print the table header and contents
  _->
   io:format("~n"),
   print_table_header(Table),
   print_table_records(get_table_records(Table))
 end;
 
print_table(_) ->
 {error,badarg}.

print_table() ->
 print_table(all).
 
%% Prints a table's header (print_table() helper function) 
print_table_header(location) ->
 io:format("LOCATION TABLE {loc_id,name,user,port}~n==============~n");
print_table_header(sublocation) ->
 io:format("SUBLOCATION TABLE {sub_id,name}~n=================~n");
print_table_header(device) -> 
 io:format("DEVICE TABLE {dev_id,name,sub_id,type,config,lastupdate}~n============~n");
print_table_header(suploc) -> 
 io:format("SUPLOC TABLE {loc_id,sup_pid}~n============~n");
print_table_header(ctrmanager) -> 
 io:format("CTRMANAGER TABLE {loc_id,mgr_pid,status}~n================~n");
print_table_header(devmanager) -> 
 io:format("DEVMANAGER TABLE {dev_id,loc_id,mgr_pid,status}~n================~n").

%% Prints all records in a table, or "(empty)" if there are none (print_table(Table) helper function)
print_table_records(TableRecords) when TableRecords == [] ->
 io:format("(empty)~n~n");
print_table_records(TableRecords) ->
 print_table_records_list(TableRecords).

%% Prints all records in a list (print_table(Table)->print_table_records(TableRecords) helper function)  
print_table_records_list([]) ->
 io:format("~n");
print_table_records_list([Record|NextRecord]) ->
 io:format("~s~n",[io_lib:format("~p",[Record])]),
 print_table_records_list(NextRecord).
 

%% DESCRIPTION:  Prints the database contents associated to one or more users, a location or a sublocation indented as a tree
%%
%% ARGUMENTS:    - (),(all):             Prints the tree of locations, sublocations and devices of all users
%%               - (user,Username):      Prints the tree of locations, sublocations and devices of a specific user
%%               - (location,Loc_id):    Prints the tree of sublocations and devices in a specific location
%%               - (sublocation,Sub_id): Prints the tree of devices in a specific sublocation
%%
%% RETURNS:      - ok                     -> Required contents were printed
%%               - user_not_exists        -> The specified user doesn't exist    (print_tree(user,Username))
%%               - location_not_exists    -> Location "Loc_id" does not exist    (print_tree(location,Loc_id))
%%               - sublocation_not_exists -> Sublocation "Sub_id" does not exist (print_tree(sublocation,Sub_id)
%%               - {error,unknown_table}  -> Unknown table                       (print_tree(Tabletype,id))
%%               - {error,unsupported}    -> Operation not supported             (print_tree(device,Id))
%%               - {error,badarg}         -> Invalid arguments
%%
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
 {error,badarg}.

print_tree() ->
 print_tree(all).

print_tree(user,Username) ->
 
 % Check the user to exist
 UserLocs = mnesia:dirty_match_object(#location{user = Username, _ = '_'}),

 case UserLocs of
 
  % If the user was not found, return
  [] ->
   user_not_exists;
   
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
  {error,unknown_table} ->
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
	 location_not_exists;
	 
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
	 sublocation_not_exists;
   
    % If it exists, print its devices as a tree
    _Sublocation ->
	 io:format("~n"),
	 print_tree_sublocation(Sublocation,"",""),
     io:format("~n")
   end
 end;
 
print_tree(_,_) ->
 {error,badarg}.

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


%% Prints all devices in a list as a tree (print_tree_sublocation([Subloc|NextSubloc],Indent1,Indent2) helper function) 
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
 io:format("~s{~w,~s,~w,~w,~p,~s} - ~s~n",[Indent, Dev#device.dev_id,io_lib:format("~p",[Dev#device.name]), Dev#device.sub_id, Dev#device.type, utils:deprefix_dev_config(Dev#device.config),
										   string:slice(calendar:system_time_to_rfc3339(Dev#device.lastupdate,[{time_designator,$\s}]),0,19), DevMgrStatus]),
 % Parse the next device in the list
 print_tree_device(NextDev,Indent).


%% DESCRIPTION:  Returns a table record by key, if it exists
%%
%% ARGUMENTS:    - Tabletype: The table where to search the record, also considering shorthand forms
%%               - Key:       The record key (>=0)
%%
%% RETURNS:      - {ok,Record}             -> The record with key "Key" in table "Tabletype"
%%               - {error,not_found}       -> The record with key "Key" was not found in table "Tabletype"
%%               - {error,unknown_table} -> Unknown table
%%               - {error,badarg}        -> Invalid arguments
%%
get_record(Tabletype,Key) when is_atom(Tabletype), is_number(Key), Key>=0 ->

 % Resolve possible table shorthand forms
 Table = resolve_tabletype_shorthand(Tabletype),
 
 case Table of
 
  % If unknown table, return an error
  {error,unknown_table} ->
   {error,unknown_table};
  
  % Otherwise, search for the record by Key
  _ ->
   case mnesia:transaction(fun() -> mnesia:read({Table,Key}) end) of
    
	% If the record was found, return it
    {atomic,[Record]} ->
	 {ok,Record};
	 
	% Otherwise, return that it was not found 
	{atomic,[]} ->
	 {error,not_found}
   end
 end;

get_record(_,_) ->
 {error,badarg}.


%% DESCRIPTION:  Returns all records in a table as a list
%%
%% ARGUMENTS:    - Tabletype: The table to retrieve the records, or its shorthand form
%%
%% RETURNS:      - [TableRecords]        -> The list of records in table Tabletype (possibly empty)
%%               - {error,unknown_table} -> Unknown table
%%               - {error,badarg}        -> Invalid arguments
%%
get_table_records(Tabletype) when is_atom(Tabletype) ->

 % Resolve possible table shorthand forms
 Table = resolve_tabletype_shorthand(Tabletype),

 case Table of
 
  % If unknown table, return an error
  {error,unknown_table} ->
   {error,unknown_table};
  
  % Otherwise return all the table's records
  _ ->
   mnesia:dirty_select(Table,[{'_',[],['$_']}])  % The second argument is a "catch-all" clause
 end;
 
get_table_records(_) ->
 {error,badarg}.
 
 
%% DESCRIPTION:  Returns all keys in a specific or all database tables
%%
%% ARGUMENTS:    - (),(all):  Retrieve the keys of all database tables
%%               - Tabletype: The table which to retrieve the keys, also considering shorthand forms
%%
%% RETURNS:      - [TableKeys]           -> The list of keys in table Tabletype (possibly empty)    (get_table_keys(Tabletype))
%%               - {[TablesKeys]}        -> The list of keys in all database tables (possibly empty (get_table_keys(|all))
%%               - {error,unknown_table} -> Unknown table                                           (get_table_keys(Tabletype))
%%               - {error,badarg}        -> Invalid arguments
%%
get_table_keys(all) ->
 {atomic,{LocationKeys,SublocationKeys,DeviceKeys,SuplocKeys,CtrManagerKeys,DevManagerKeys}} =
  mnesia:transaction(fun() -> {mnesia:all_keys(location),mnesia:all_keys(sublocation),
                               mnesia:all_keys(device),mnesia:all_keys(suploc),
							   mnesia:all_keys(ctrmanager),mnesia:all_keys(devmanager)} end),
 {LocationKeys,SublocationKeys,DeviceKeys,SuplocKeys,CtrManagerKeys,DevManagerKeys};
 
get_table_keys(Tabletype) when is_atom(Tabletype) ->

 % Resolve possible table shorthand forms
 Table = resolve_tabletype_shorthand(Tabletype),

 case Table of
 
  % If unknown table, return an error
  {error,unknown_table} ->
   {error,unknown_table};
  
  % Otherwise retrieve and return the table keys
  _ ->
    do_transaction(fun() -> mnesia:all_keys(Table) end)
 end;

get_table_keys(_) ->
 {error,badarg}.

get_table_keys() ->
 get_table_keys(all).
 
 
%% DESCRIPTION:  Returns the number of records in a specific or all database tables
%%
%% ARGUMENTS:    - (),(all):    Returns the number of records in all database tables
%%               - (Tabletype): The table which to retrieve the number of records, also considering shorthand forms
%%
%% RETURNS:      - Num_Records_Table      -> The number of records in the specified table (Tabletype syntax)
%%               - {[Num_Records_Tables]} -> The number of records in all tables in order (() or (all) syntax)
%%               - {error,unknown_table}  -> Unknown table (Tabletype syntax)
%%               - {error,badarg}         -> Invalid arguments
%%
get_records_num(all) ->
 {LocationKeys,SublocationKeys,DeviceKeys,SuplocKeys,CtrManagerKeys,DevManagerKeys} = get_table_keys(all),
 {length(LocationKeys),length(SublocationKeys),length(DeviceKeys),length(SuplocKeys),length(CtrManagerKeys),length(DevManagerKeys)};
  
get_records_num(Tabletype) when is_atom(Tabletype) ->
 
 % Retrieve the table keys, also considering shorthand forms
 Keys = get_table_keys(Tabletype),
 
 case Keys of
   
  % If the table was not found, return an error
  {error,unknown_table} ->
   {error,unknown_table};
   
  % Otherwise return the number of keys in the table
  _ ->
  length(Keys)
 end;
 
get_records_num(_) ->
 {error,badarg}.
 
get_records_num() ->
 get_records_num(all).
 
 
%% DESCRIPTION:  Returns the list of 'dev_id's of devices in a location
%%
%% ARGUMENTS:    - Loc_id: The ID of the location to add, which must not already exist and be >0
%%
%% RETURNS:      - [LocDevIdList]              -> The list of 'dev_id's of devices in the location
%%               - {error,location_not_exists} -> The location 'Loc_id' does not exist
%%               - {error,badarg}              -> Invalid arguments
%% 
get_loc_devs(Loc_id) when is_number(Loc_id), Loc_id>0 ->
 F = fun() ->
 
      % Check the location to exist
	  case mnesia:read({location,Loc_id}) of 
	   [_LocationRecord] ->
	    
		% If it does, retrieve the list of the dev_ids of devices in the location
        MatchHead = #device{dev_id='$1', sub_id = {Loc_id,'_'}, _='_'},  % Consider the dev_ids with loc_id == Loc_id
        Guard = [],                                                      % No guard
        Result = '$1',                                                   % Return only the dev_ids
 
        mnesia:select(device,[{MatchHead, Guard, [Result]}]);
		
	   [] ->
	   
		% Otherwise, if the location doesn't exist, return an error
		mnesia:abort(location_not_exists)
	  end
	 end,
 do_transaction(F);

get_loc_devs(_) ->
 {error,badarg}.


%% DESCRIPTION:  Returns the list of 'dev_id's of devices in a sublocation
%%
%% ARGUMENTS:    - {Loc_id,Subloc_id}: The sub_id of the sublocation which to retrieve the
%%                                     list of devices, which must exist and be {>0,>=0}
%%
%% RETURNS:      - [SubLocDevIdList]              -> The list of 'dev_id's of devices in the sublocation
%%               - {error,sublocation_not_exists} -> The sublocation {Loc_id,Subloc_id} does not exist
%%               - {error,badarg}                 -> Invalid arguments
%% 
get_subloc_devs({Loc_id,Subloc_id}) when is_number(Loc_id), Loc_id>0, is_number(Subloc_id), Subloc_id>=0 ->
 F = fun() ->
 
 	  % Check the sublocation to exist
	  case mnesia:read({sublocation,{Loc_id,Subloc_id}}) of
	  
	   [] ->
	 
	    % If it doesn't exist, return an error
		mnesia:abort(sublocation_not_exists);
		  
	   [SublocationRecord] ->
		  
		% If it exists, return the list of 'dev_id's of devices in the sublocation
		SublocationRecord#sublocation.devlist
	  end
	 end,
 do_transaction(F);
 
get_subloc_devs(_) ->
 {error,badarg}.
 
 
%% DESCRIPTION:  Returns the location ID, the PID and the status of a node's manager (ctr_manager or dev_manager)
%%
%% ARGUMENTS:    - NodeTypeShorthand: An atom indicating the type of node to be restarted, also taking
%%                                    into account shorthand forms, with the following being allowed:
%%                                     - controller,ctr,contr -> controller node
%%                                     - device, dev          -> device node
%%               - Node_id: The node's ID ('loc_id' for controller and 'dev_id' for device nodes)
%%
%% RETURNS:      - {Loc_id,Mgr_Pid,MgrStatus}            -> The node's manager location ID, PID and status as a tuple
%%
%% THROWS: 		 - {error,location_not_exists}           -> The location associated with the specified controller node does not exist 
%%               - {error,device_not_exists}             -> The specified device does not exist
%%               - {error,{internal,ctrmanager_missing}} -> The location associated with the specified controller node exists, but its
%%                                                          associated record in the 'ctrmanager' table was not found (consistency error)
%%               - {error,{internal,devmanager_missing}} -> The device associated with the specified device node exists, but its
%%                                                          associated record in the 'devmanager' table was not found (consistency error)
get_manager_info(controller,Loc_id) when is_number(Loc_id), Loc_id>0 ->

 % Attempt to retrieve the record associated with the controller's manager in the 'ctrmanager' table
 case db:get_record(ctrmanager,Loc_id) of
	  
  {error,not_found} ->
  
   % If the record was not found, it means either that:
   %
   % 1) A non-existing 'loc_id' was passed
   % 2) There is a consistency error between the 'location' and the 'ctrmanager' tables
   %
   % Determine which of the two by searching for the location record in the 'location' table
   case db:get_record(location,Loc_id) of
   
    % If the location was not found, a non-existing 'loc_id' was passed
	%
	% NOTE: This cannot happen in the "change_loc_status()" function, for
	%       the existence of the associated location was already checked for
    {error,not_found} ->
	 throw({error,location_not_exists});
	
	% If the location was found, there is a consistency error between the 'location' and the 'ctrmanager' tables
    {ok,_LocationRecord} ->
	 throw({error,{internal,ctrmanager_missing}})
   end;
	
  {ok,CtrMgrRecord} ->
	  
   % If the controller manager's record was successfully retrieved, return its location ID, PID and status
   {Loc_id,CtrMgrRecord#ctrmanager.mgr_pid,CtrMgrRecord#ctrmanager.status}
 end;

get_manager_info(device,Dev_id) when is_number(Dev_id), Dev_id>0 ->

 % Attempt to retrieve the record associated with the device's manager in the 'devmanager' table
 case db:get_record(devmanager,Dev_id) of
	  
  {error,not_found} ->
  
   % If the record was not found, it means either that:
   %
   % 1) A non-existing 'dev_id' was passed
   % 2) There is a consistency error between the 'device' and the 'devmanager' tables
   %
   % Determine which of the two by searching for the device record in the 'device' table
   case db:get_record(device,Dev_id) of
   
    % If the device was not found, it means that a non-existing 'dev_id' was passed
    {error,not_found} ->
	 throw({error,device_not_exists});
	
	% If the device was found, there is a consistency error between the 'device' and the 'devmanager' table
    {ok,_DeviceRecord} ->
	 throw({error,{internal,devmanager_missing}})
   end;
	
  {ok,DevMgrRecord} ->
	  
   % If the device manager's record was successfully retrieved, return its location ID, PID and status
   {DevMgrRecord#devmanager.loc_id,DevMgrRecord#devmanager.mgr_pid,DevMgrRecord#devmanager.status}
 end;
  
get_manager_info(NodeTypeShortHand,Node_id)  when is_number(Node_id), Node_id>0 ->

 % Determine the node type, also taking shorthand forms into account
 NodeType = utils:resolve_nodetype_shorthand(NodeTypeShortHand),
 
 % Call the function clause associated with the NodeType
 get_manager_info(NodeType,Node_id).


%% DESCRIPTION:  Returns the PID associated with a 'sup_loc' location supervisor
%%
%% ARGUMENTS:    - Loc_id: The location ID associated with the 'sup_loc' supervisor
%%
%% RETURNS:      - Sup_Pid -> The PID of the 'sup_loc' supervisor associated with location "Loc_id"
%%
%% THROWS: 		 - {error,location_not_exists}       -> The location associated with the specified 'sup_loc' supervisor does not exist 
%%               - {error,{internal,suploc_missing}} -> The location associated with the specified 'sup_loc' supervisor exists, but its
%%                                                      associated record in the 'suploc' table was not found (consistency error)
%%
get_suploc_pid(Loc_id) ->

 % Attempt to retrieve the record associated with the 'sup_loc' supervisor in the 'suploc' table
 case db:get_record(suploc,Loc_id) of
  
  {error,not_found} ->
  
   % If the record was not found, it means either that:
   %
   % 1) A non-existing 'loc_id' was passed
   % 2) There is a consistency error between the 'location' and the 'suploc' tables
   %
   % Determine which of the two by searching for the location record in the 'location' table
   case db:get_record(location,Loc_id) of
   
    % If the location was not found, a non-existing 'loc_id' was passed
	%
    % NOTE: This cannot occur when attempting to change nodes' statuses in the "change_node_status()" or
	%       "change_subloc_status()" functions, for the existence of the associated location was already checked for
	{error,not_found} ->
	 throw({error,location_not_exists});
	
	% If the location was found, there is a consistency error between the 'location' and the 'suploc' tables
    {ok,_LocationRecord} ->
	 throw({error,{internal,suploc_missing}})
   end;

  {ok,SuplocRecord} -> 
  
   % If the 'sup_loc' supervisor record was successfully retrieved, return its PID
   SuplocRecord#suploc.sup_pid
 end.
 
 
%% ========================================================== UPDATE ===============================================================%% 

%% DESCRIPTION:  Updates a device's sublocation
%%
%% ARGUMENTS:    - Dev_id:             The ID of the device to change sublocation, which must exist and be >0
%%               - {Loc_id,Subloc_id}: The 'sub_id' of the sublocation where to put the device, which must exist and be {>0,>=0}
%%
%% RETURNS:      - ok                             -> Device sublocation successfully updated
%%               - {error,device_not_exists}      -> The device 'Dev_id' does not exist
%%               - {error,sublocation_not_exists} -> The sublocation 'sub_id' does not exist
%%               - {error,badarg}                 -> Invalid arguments
%%
%% NOTE:         This function currently doesn't check whether the new and the old device sublocations belong to the same location
%%
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
 do_transaction(F);	  %% [TODO]: Inform controller of the change of sublocation for the device
	 
update_dev_sub(_,_) ->
 {error,badarg}.


%% DESCRIPTION:  Updates a device's configuration
%%
%% ARGUMENTS:    - Dev_id:    The ID of the device to update the configuration, which must exist and be >0
%%               - Config:    The updated device configuration
%%               - Timestamp: The timestamp of the updated device configuration (>0)
%%
%% RETURNS:      - ok                        -> Device configuration successfully updated
%%               - {error,device_not_exists} -> The device 'Dev_id' does not exist
%%               - {error,invalid_devconfig} -> The updated device configuration is not valid
%%               - {error,badarg}            -> Invalid arguments
%%
update_dev_config(Dev_id,Config,Timestamp) when is_number(Dev_id), Dev_id>0, is_number(Timestamp), Timestamp>0 ->
 F = fun() ->
 
      % Check the device to exist
	  case mnesia:wread({device,Dev_id}) of      % wread = write lock
	   [Device] ->
	   
	    % If it does, ensure the updated configuration to be valid
		case catch(utils:is_valid_devconfig(Config,Device#device.type)) of
		 ok ->
		     
	      % If it is, push it in the 'device' table along with its timestamp
		  UpdatedDevice = Device#device{config = Config, lastupdate = Timestamp},
		  mnesia:write(UpdatedDevice);
			
		 {error,invalid_devconfig} ->
		   
		  % Otherwise, if it is not valid (which SHOULD NOT happen), report the error and abort the transaction
		  io:format("[update_dev_config]: <WARNING> An invalid device configuration was passed (Config = ~p, DevType = ~p)~n",[Config,Device#device.type]),
		  mnesia:abort(invalid_devconfig)
		end;		
		
	   [] ->
	   
	    % If the device does not exist, abort the transaction
	    mnesia:abort(device_not_exists)
      end
     end,
 do_transaction(F);
	 
update_dev_config(_,_,_) ->
 {error,badarg}.


%% DESCRIPTION:  Updates a location's name
%%
%% ARGUMENTS:    - Loc_id: The ID of the location to update the name, which must exist and be >0
%%               - Name:   The updated location name
%%
%% RETURNS:      - ok                          -> Location name successfully updated
%%               - {error,location_not_exists} -> The location 'Loc_id' does not exist
%%               - {error,badarg}              -> Invalid arguments
%%
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
 do_transaction(F);

update_loc_name(_,_) ->
 {error,badarg}.


%% DESCRIPTION:  Updates a sublocation's name
%%
%% ARGUMENTS:    - {Loc_id,Subloc_id}: The sub_id of the sublocation to update
%%                                     the name, which must exist and be {>0,>0}
%%               - Name:               The updated sublocation name
%%
%% RETURNS:      - ok                             -> Sublocation name successfully updated
%%               - {error,sublocation_not_exists} -> The sublocation 'Sub_id' does not exist
%%               - {error,badarg}                 -> Invalid arguments
%%
%% NOTE:         Currently the name of the "(default)" sublocation of a location cannot be changed (Subloc_id >0)
%%
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
 do_transaction(F);

update_subloc_name(_,_) ->
 {error,badarg}.
 

%% DESCRIPTION:  Updates a device's name
%%
%% ARGUMENTS:    - Dev_id: The dev_id of the device to update the name, which must exist and be >0
%%               - Name:   The updated device name
%%
%% RETURNS:      - ok                        -> Device name successfully updated
%%               - {error,device_not_exists} -> The device 'Dev_id' does not exist
%%               - {error,badarg}            -> Invalid arguments
%%
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
 do_transaction(F);

update_dev_name(_,_) ->
 {error,badarg}.
 
%% ========================================================== DELETE ===============================================================%% 

%% DESCRIPTION:  Deletes a location, along with all its sublocations and devices, from the
%%               database, also stopping the associated controller' and devices' nodes
%%
%% ARGUMENTS:    - Loc_id: The ID of the location to delete from the database
%%
%% RETURNS:      - {ok,ok}                     -> The location and all its sublocations and devices were
%%                                                successfully deleted, and the associated nodes were stopped
%%               - ok                          -> The location and all its sublocations and devices were successfully
%%                                                deleted (not their nodes since the JANET Simulator is not running)
%%               - {error,location_not_exists} -> The location 'Loc_id' does not exist
%%               - {error,badarg}              -> Invalid arguments
%%
%% NOTE:         Use with caution, for deleted locations, sublocations and devices CANNOT be recovered
%%
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

 case {mnesia:transaction(F),utils:is_running(janet_simulator)} of
  {{atomic,ok},true} ->
  
   % If the transaction was successfully and the JANET simulator is running, attempt to delete
   % the location 'sup_loc' supervisor and all its managers' records from the 'ctrmanager'
   % and 'devmanager' tables, returning the result of the transaction and of such deletion
   {ok,catch(delete_suploc(Loc_id))};
   
  {{atomic,ok},false} ->
	
   % If the transaction was successful but the JANET
   % simulator is not running, return just "ok"
   ok;
	 
  {{aborted,Reason},_} ->
	
   % If an error occured in the transaction, return it
   {error,Reason}
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


%% Attempts to delete a 'sup_loc' supervisor and all its managers' records from the
%% 'ctrmanager' and 'devmanager' tables (delete_location(Loc_id) helper function)
delete_suploc(Loc_id) ->
   
 % Attempt to retrieve the PID of the location's 'sup_loc' supervisor
 SupLocPid = get_suploc_pid(Loc_id),
 
 % Attempt to terminate the 'sup_loc' supervisor via its own 'sup_locs' supervisor
 ok = supervisor:terminate_child(sup_locs,SupLocPid),
       
 % Remove the 'sup_loc' supervisor entry from the 'suploc' table
 {atomic,ok} = mnesia:transaction(fun() -> mnesia:delete({suploc,Loc_id}) end),

 % Remove the location controller manager entry from the 'ctrmanager' table
 {atomic,ok} = mnesia:transaction(fun() -> mnesia:delete({ctrmanager,Loc_id}) end),
   
 % Delete all location device managers entries from the 'devmanager' table
 F = fun() ->
     
	   % Retrieve the 'dev_id's of all device managers in the location
       MatchHead = #devmanager{dev_id='$1', loc_id=Loc_id, _='_'}, % Select all 'dev_id
       Guard = [],
       Result = '$1',
	   LocDevIds = mnesia:select(devmanager,[{MatchHead,Guard,[Result]}]),
		
	   % Delete the location device manager entries from the 'devmanager' table
       lists:foreach(fun(Dev_id) -> mnesia:delete({devmanager,Dev_id}) end, LocDevIds)
     end,
	   
 {atomic,ok} = mnesia:transaction(F),
 
 % Return the success of the operation
 ok.


%% DESCRIPTION:  Deletes a sublocation from the database, moving all its devices to its location's default sublocation {Loc_id,0}
%%
%% ARGUMENTS:    - {Loc_id,Subloc_id}: The sub_id of the sublocation to delete, with both elements >0
%%
%% RETURNS:      - ok                             -> The sublocation was successfully deleted and its devices
%%                                                   were moved to the location's default sublocation {Loc_id,0}
%%               - {error,sublocation_not_exists} -> The sublocation 'Sub_id' does not exist
%%               - {error,badarg}                 -> Invalid arguments
%%
%% NOTE:         Default sublocations cannot be removed (Subloc_id > 0)
%%
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
 do_transaction(F);  %% [TODO]: Inform the controller that the sublocation no longer exists and so to change the devices to the default sublocation

delete_sublocation(_) ->
 {error,badarg}.

%% Moves all devices in a list to the default sublocation {Loc_id,0} (delete_sublocation(Loc_id,Subloc_id) helper function)
move_devlist_to_default_subloc([],_) ->
 ok;
move_devlist_to_default_subloc([Dev_id|NextDev_id],Loc_id) ->

 % Retrieve the device record
 [Device] = mnesia:wread({device,Dev_id}),
 
 % Change the device to the default sublocation
 UpdatedDevice = Device#device{sub_id={Loc_id,0}},
 
 % Update the device record
 mnesia:write(UpdatedDevice),
 
 % Proceed with the next device
 move_devlist_to_default_subloc(NextDev_id,Loc_id).


%% DESCRIPTION:  Deletes a device from the database, also terminating its node if it is running
%%
%% ARGUMENTS:    - Dev_id: The dev_id of the device to delete, which must exist and be >0
%%
%% RETURNS:      - {ok,ok}                   -> The device was deleted from the database and its device node was terminated
%%               - ok                        -> The device was deleted from the database (its node was
%%                                              not terminated since the JANET Simulator is not running)
%%               - {error,device_not_exists} -> The device 'Dev_id' does not exist
%%               - {error,badarg}            -> Invalid arguments
%%
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

 case {mnesia:transaction(F),utils:is_running(janet_simulator)} of
  {{atomic,ok},true} ->
  
   % If the transaction was successfully and the JANET simulator is running, attempt to
   % delete the device manager via its 'sup_loc' supervisor and to remove its entry from
   % the 'devmanager' table, returning the result of the transaction and of such deletion
   {ok,catch(delete_devmanager(Dev_id))};
   
  {{atomic,ok},false} ->
	
   % If the transaction was successful but the JANET
   % simulator is not running, return just "ok"
   ok;
	 
  {{aborted,Reason},_} ->
	
   % If an error occured in the transaction, return it
   {error,Reason}
 end;

delete_device(_) ->
 {error,badarg}.


%% Attempts to delete a device manager via its 'sup_loc' supervisor and
%% remove its entry from the 'devmanager' table (delete_device(Dev_id) helper function)
delete_devmanager(Dev_id) ->

 % Attempt to retrieve the device's location ID from the 'devmanager' table
 {Loc_id,_,_} = get_manager_info(device,Dev_id),

 % Attempt to retrieve the PID of the location's 'sup_loc' supervisor
 Sup_pid = get_suploc_pid(Loc_id),
 
 % Prefix the device's ID
 Pref_Dev_id = utils:prefix_node_id(device,Dev_id),
 
 % Attempt to terminate the device manager via its 'sup_loc' supervisor
 case supervisor:terminate_child(Sup_pid,Pref_Dev_id) of
  {error,Reason} ->
  
   % If the termination reported an error, return it
   {error,Reason};
   
  ok ->
   
   % If the termination was successful, attempt to delete
   % the device manager child from its 'sup_loc' supervisor 
   case supervisor:delete_child(Sup_pid,Pref_Dev_id) of
    {error,Reason} ->
  
     % If the deletion reported an error, return it
     {error,Reason};

    ok ->
     % Otherwise delete the device manager entry from the
	 % 'devmanager' table and return the result of the operation
     {atomic,ok} = mnesia:transaction(fun() -> mnesia:delete({devmanager,Dev_id}) end),
	 ok
	 
	 %% [TODO]: Inform the controller that the device no longer exists
   
   end
 end.
 

%%====================================================================================================================================
%%                                                    PUBLIC UTILITY FUNCTIONS
%%==================================================================================================================================== 

%% DESCRIPTION:  Starts the Mnesia application and waits for its disc_copies tables (location,sublocation,device) to be loaded from disc
%%
%% ARGUMENTS:    none 
%%
%% RETURNS:      - ok                  -> Mnesia started and disc_copies tables successfully loaded
%%               - {timeout,[table()]} -> Timeout in loading the Mnesia Tables (probably the schema is not installed)
%%               - {error,Reason}      -> Internal Mnesia error
%%
%% NOTES:        1) If Mnesia is already running, the function returns 'ok'
%%               2) This function is for debugging purposes olny, and should not be called explicitly during the JANET Simulator operations
%%               3) This function is called on the JANET Simulator node at boot by the 'init' process so to automatically start Mnesia
%%
start_mnesia() ->
 
 % Check if Mnesia is already running
 case utils:is_running(mnesia) of
  
  false ->
   
   % If not, attempt to start it
   MnesiaStarted = application:start(mnesia),
   case MnesiaStarted of
	
    % If successfully started, do nothing
	ok ->
     ok;
	
    % Otherwise, report the error
    {error,Reason} ->
     io:format("Error in starting Mnesia (reason = ~w) (is the Mnesia scheme installed?...)~n",[Reason])
   end;	
	
  true ->

   % If already started, set the variable
   MnesiaStarted = ok
 end,
 
 case MnesiaStarted of
  ok ->
   % If Mnesia is running, wait for its disc_copies tables to be loaded from disc
   TablesLoaded = mnesia:wait_for_tables([location,sublocation,device],4000),
   case TablesLoaded of
   
    % If the tables were successfully loaded, do nothing
	ok ->
	 ok;
	 
	% If timeout in loading the tables, report the error
    {timeout,TableList} ->
	 io:format("Timeout in loading the Mnesia tables ~w (is the Mnesia scheme installed?...)~n",[TableList]);
	
    % If other error in loading the tables, report the error
    {error,LoadReason} ->
	 io:format("Error in loading the Mnesia tables (reason = ~w) (is the Mnesia scheme installed?...)~n",[LoadReason])	
   end,
   
   % Return the TablesLoaded variable
   TablesLoaded;
    
  _ ->
   % Otherwise, if Mnesia is not running, report its starting error
   MnesiaStarted
 end.


%% DESCRIPTION:  Stops the Mnesia application
%%
%% ARGUMENTS:    none
%%
%% RETURNS:      - ok             -> Mnesia successfully stopped
%%               - {error,Reason} -> Internal Mnesia error
%%
%% NOTES:        1) If Mnesia is already stopped, the function returns 'ok'
%%               2) This function is for debugging purposes olny, and should not be called explicitly during the JANET Simulator operations
%%
stop_mnesia() ->

 % Attempt to stop Mnesia
 case application:stop(mnesia) of
 
  % If stopped, do nothing
  ok ->
   ok;
   
  % If already stopped, do nothing
  {error,{not_started,mnesia}} ->
   ok;

  % If another error occured, report it
  {error,Reason} ->
   io:format("[Janet-Simulator]: Error in stopping the Mnesia application (reason = ~w)~n",[Reason]),
   {error,Reason}
 end.
 
 
%% DESCRIPTION:  Backups the entire Mnesia database to a file
%%
%% ARGUMENTS:    - (none): The default backup file is used ("priv/db/mnesia_backup.db")
%%               - (File): A custom backup file is used
%%
%% RETURNS:      - ok             -> Mnesia database successfully backed up to the specified file
%%               - {error,Reason} -> Error in creating the backup file (its directory must exist
%%                                   and its 'write' permission must be granted)
%% 
%% NOTE:         Mnesia backups can later be restored using the restore()/restore(File) function
%% 
backup() ->
 backup("priv/db/mnesia_backup.db").  % Default backup file
 
backup(File) ->

 % Ensure Mnesia to be running
 ok = start_mnesia(),

 % Attempt to backup the database contents to the specified file
 BackupResult = mnesia:backup(File),
 case BackupResult of
 
  % If the backup was successful, notify it
  ok ->
   io:format("Mnesia database backed up to file \"~s\"~n",[File]);
   
  % Otherwise, report the error
  _ ->
   io:format("Error in creating the backup file \"~s\" (check its directory to exist and its permissions)~n",[File]),
   BackupResult
 end.


%% DESCRIPTION:  Restores the database to the contents of a backup file
%%
%% ARGUMENTS:    - (none): The default backup file is used for restoring the database ("priv/db/mnesia_backup.db")
%%               - (File): A custom backup file is used
%%
%% RETURNS:      - ok                       -> Database successfully restored to the contents of the specified backup file
%%               - aborted                  -> The user aborted the operation
%%               - {error,janet_is_running} -> The operation cannot be performed while the JANET Simulator is running
%%               - {error,Reason}           -> Error in restoring the database from the backup file
%%                                             (check the file to exist and its read permission to be granted)
%%
%% NOTES:        1) The current database contents will be DISCARDED by calling this function
%%               2) Database backup files can be created via the db:backup()/db:backup(File) function
%%
restore() ->
 restore("priv/db/mnesia_backup.db").
 
restore(File) ->

 % Ensure the JANET simulator not to be running and ask for user confirmation for the operation
 CheckOp = check_db_operation("Restoring"),
 case CheckOp of
   
  % If the user confirmed the operation
  ok ->
  
   % Ensure Mnesia to be running
   ok = start_mnesia(),
   
   % Attempt to restore the database to the contents of the specified file
   RestoreDBResult = mnesia:restore(File,[{default_op,recreate_tables}]),
   case RestoreDBResult of
    
	% If the restoring was successful, notify it
	{atomic, _} ->
	 io:format("Mnesia database successfully restored to the contents of the \"~s\" file~n",[File]);
	
	% Otherwise return the error
	_ ->
	 RestoreDBResult
   end;

  % Otherwise return CheckOp
  _ ->
   CheckOp
 end.


%% DESCRIPTION:  Clears (empties) all database tables (but preserves the database's schema)
%%
%% ARGUMENTS:    none
%%
%% RETURNS:      - ok                       -> Database tables successfully cleared
%%               - aborted                  -> The user aborted the operation
%%               - {error,janet_is_running} -> The operation cannot be performed while the JANET Simulator is running
%%
%% NOTE:         This function is for debugging purposes olny, and should not be called explicitly during the
%%               JANET Simulator operations (use restore()/restore(File) to restore the database's contents)
%%
clear() ->

 % Ensure the JANET simulator not to be running and ask for user confirmation for the operation
 CheckOp = check_db_operation("Clearing"),

 case CheckOp of
 
  % If the user confirmed the operation
  ok ->
  
   % Ensure Mnesia to be running
   ok = start_mnesia(),
   
   % Clear all Mnesia tables and report the operation
   [{atomic,ok},{atomic,ok},{atomic,ok},{atomic,ok},{atomic,ok},{atomic,ok}] =
	[mnesia:clear_table(location),mnesia:clear_table(sublocation),mnesia:clear_table(device),
	 mnesia:clear_table(suploc),mnesia:clear_table(ctrmanager),mnesia:clear_table(devmanager)],
     
   % Return the result of the operation
   io:format("Mnesia tables successfully cleared~n");
	 
  % Otherwise return CheckOp
  _ ->
   CheckOp
 end.
 
 
%% DESCRIPTION:  Installs from scratch (schema + tables) the Mnesia database used by the Janet Simulator
%%
%% ARGUMENTS:    none
%%
%% RETURNS:      - ok                       -> Mnesia database successfully installed
%%               - aborted                  -> The user aborted the operation
%%               - {error,janet_is_running} -> The operation cannot be performed while the JANET Simulator is running
%%
%% NOTE:         This function is for debugging purposes olny, and should not be called explicitly during the
%%               JANET Simulator operations (use restore()/restore(File) to restore the database's contents)
%%
install() ->

 % Ensure the JANET simulator not to be running and ask for user confirmation for the operation
 CheckOp = check_db_operation("Installing"),

 case CheckOp of
  
  % If the user confirmed the operation
  ok ->
  
   % Ensure the Mnesia database not to be running
   ok = stop_mnesia(),
	 
   % Recreate the Mnesia Schema
   ok = mnesia:delete_schema([node()]),
   ok = mnesia:create_schema([node()]),
   
   % Start Mnesia and create its tables 
   ok = application:start(mnesia),
	 
   %% ---------------------------- disc_copies tables ---------------------------- %%   
   {atomic,ok} = mnesia:create_table(location,
                                     [
									  {attributes, record_info(fields, location)},
                                      {index, [#location.user,#location.port]},
                                      {disc_copies, [node()]}
									 ]),
									 
   {atomic,ok} = mnesia:create_table(sublocation,
                                     [
									  {attributes, record_info(fields, sublocation)},
					                  {type, ordered_set},
                                      {disc_copies, [node()]}
									 ]),
									 
   {atomic,ok} = mnesia:create_table(device,
                                     [
									  {attributes, record_info(fields, device)},
                                      {index, [#device.sub_id]},
                                      {disc_copies, [node()]}
									 ]),
   
   %% ---------------------------- ram_copies tables ---------------------------- %%  
   {atomic,ok} = mnesia:create_table(suploc,
                                     [
									  {attributes, record_info(fields, suploc)},
                                      {ram_copies, [node()]}
									 ]),
									 
   {atomic,ok} = mnesia:create_table(ctrmanager,
                                     [
									  {attributes, record_info(fields, ctrmanager)},
                                      {ram_copies, [node()]}
									 ]),
									 
   {atomic,ok} = mnesia:create_table(devmanager,
                                     [
									  {attributes, record_info(fields, devmanager)},
                                      {ram_copies, [node()]}
									 ]),
	
   % Report the result of the operation		
   io:format("Mnesia database successfully installed~n");
 
  % Otherwise return CheckOp
  _ ->
   CheckOp
 end.

 
%%====================================================================================================================================
%%                                                    PRIVATE HELPER FUNCTIONS
%%==================================================================================================================================== 
 
%% DESCRIPTION:  Ensures the JANET simulator to be stopped and asks user confirmation before attempting a database utility operation
%%               (restore(|File), clear(), install() helper function)
%%
%% ARGUMENTS:    - Operation: a string used in the user confirmation request
%%
%% RETURNS:      - ok                       -> The database utility operation can proceed
%%               - aborted                  -> The user aborted the operation
%%               - {error,janet_is_running} -> The operation cannot be performed while the JANET Simulator is running
%%  
check_db_operation(Operation) ->

 % Check the JANET simulator not to be running
 case utils:is_running(janet_simulator) of
 
  true ->
  
   % Inform the user that the JANET Simulator must be stopped before attempting utility operations on the database
   io:format("Please stop the JANET Simulator first ~n"),
   {error,janet_is_running};
   
  false ->
   
   % Ask user confirmation for the operation
   io:format("~s",[Operation]),
   {ok,[Ans]} = io:fread(" the Mnesia database may cause inconsistencies with the remote database. Are you sure you want to proceed? (y/N): ","~s"),
   if
   
    % If the user confirmed the operation
    Ans =:= "y"; Ans =:= "Y" ->
	 ok;
	 
	% If the user aborted the operation
	true ->
	 aborted
   end
 end. 

  
%% DESCRIPTION:  Returns the list of unique users in the database
%%
%% ARGUMENTS:    none
%%
%% RETURNS:      The list of unique users in the database (possibly empty)
%%
get_all_users() ->

 % Initialize select arguments
 MatchHead = #location{user='$1', _='_'}, % Consider all locations
 Guard = [],                              % No guard
 Result = '$1',                           % Return only the location's users
 
 % Retrieve the users of all locations in the database
 UnfilteredUserList = mnesia:dirty_select(location,[{MatchHead, Guard, [Result]}]),
 
 % Remove duplicates and return the list of users
 lists:usort(UnfilteredUserList).


%% DESCRIPTION:  Attempts a transaction defined in a database function
%%
%% ARGUMENTS:    - F -> The fun() associated with the transaction
%%
%% RETURNS:      - Result         -> The result of the transaction, if successful            ({atomic,Result})
%%               - {error,Reason} -> The error occured in the transaction, if it was aborted ({aborted,Reason})
%%
do_transaction(F) ->
 
 % Attempt the transaction
 case mnesia:transaction(F) of
 
  % If an error occured during the transaction, return it
  {aborted,Reason} ->
   {error,Reason};
   
  % Otherwise return the transaction's result
  {atomic,Result} ->
   Result
 end.


%% DESCRIPTION:  Returns the table name atom associated with its argument, also considering shorthand forms
%%
%% ARGUMENTS:    - Tabletype: A table name, possibly in a shorthand form, with the following being allowed:
%%                            
%%                            - location, loc                   -> location
%%                            - sublocation, subloc, sub        -> sublocation
%%                            - device, dev                     -> device
%%                            - suploc, sup                     -> suploc
%%                            - ctrmanager,ctrmgr,ctr,          -> ctrmanager
%%                              controllermgr,controllermanager
%%                            - devmanager,devmgr,devicemgr     -> devmanager
%%                              devicemanager
%%
%% RETURNS:      - Tableatom             -> The table atom name associated with Tabletype
%%               - {error,unknown_table} -> If no table name could be associated with Tabletype
%%               - {error,badarg}        -> Invalid arguments
%%
resolve_tabletype_shorthand(Tabletype) when is_atom(Tabletype) ->
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
  
  % Unknown Tabletype  
  true ->
   {error,unknown_table}
 end;
 
resolve_tabletype_shorthand(_) ->
 {error,badarg}. 