-module(db).
-export([add_location/4,add_sublocation/2,add_device/3,update_dev_subloc/2,update_dev_config/2,update_loc_name/2,update_subloc_name/2,subtry/0,read_location/1,install/0,clear/0,reset/0,reset/1,dump/0,dump/1,print_table/1,record_by_id/2,print_sublocation/1,print_location/1,recordnum/0]).

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


% -----------------------------------  TODO!!!!!!!!!!!!!

%addlocation(Id,Port,Name,User) ->
% F = fun() ->
%	 mnesia:write(#location{id=Id,port=Port,status=offline,name=Name,user=User,devlist=[],sublocations=[]})
%	 end,
% mnesia:transaction(F).
 
%findlocation(Id) ->
% F = fun() -> mnesia:read({location,Id}) end,
% case mnesia:transaction(F) of
%  {atomic,[]} ->
%   undefined;
%  {atomic,[#location{id=I,port=P,status=ST,name=N,user=U,sublocations=SU}]} ->
%   {I,P,ST,N,U,SU}
%  end.



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


%% DESCRIPTION:  Prints the contents of a specific or all tables in the database
%%
%% ARGUMENTS:    - (Table): The table whose contents are to be printed
%%               - (all):   Print all tables contents   
%%
%% RETURNS:      The list of records in the specified table
%%
%% THROWS:       none  
print_table(all) ->
 print_table(location),
 print_table(sublocation),
 print_table(device);
print_table(Table) ->
 print_table_header(Table),
 print_table_records(get_table_records(Table)).
 
print_table_header(location) ->
 io:format("~nLOCATION TABLE {loc_id,name,user,port}~n==============~n");
print_table_header(sublocation) ->
 io:format("~nSUBLOCATION TABLE {sub_id,name}~n=================~n");
print_table_header(device) -> 
 io:format("~nDEVICE TABLE {dev_id,sub_id,type,config}~n============~n").
 
print_table_records([]) ->
 io:format("~n");
print_table_records([H|T]) ->
 io:format("~s~n",[io_lib:format("~p",[H])]),
 print_table_records(T).
 

%% DESCRIPTION:  Search for a record in a table by id
%%
%% ARGUMENTS:    - Tabletype: The table where to search the record in (location, sublocation, device or their shorthand forms)
%%               - Id: The id of the record to search for
%%
%% RETURNS:      - The record, if it is found
%%               - not_found, if the device is not found
%%               - {error,unknown_table} if the table is unknown
%%               - {error,badarg} if wrong argument format
%%
%% THROWS:       none  
record_by_id(Tabletype,Id) when is_atom(Tabletype), is_number(Id), Id>=0 ->

 % Check the TableType to be valid, also considering the shorthand forms
 if
  Tabletype =:= loc orelse Tabletype =:= location ->
   Table = location;
  Tabletype =:= sub orelse Tabletype =:= subloc orelse Tabletype =:= sublocation ->
   Table = sublocation;
  Tabletype =:= dev orelse Tabletype =:= device ->
   Table = device;
  true ->
   Table = unknown
 end,
   
 case Table of
 
  % If the Tabletype is invalid, return an error
  unknown ->
   {error,unknown_table};
  
  % Otherwise, search for the record by Id
  _ ->
   case mnesia:transaction(fun() -> mnesia:read({Table,Id}) end) of
    
	% If it was found, return it
    {atomic,[Record]} ->
	 Record;
	 
	% Otherwise, return that it was not found 
	{atomic,[]} ->
	 not_exists
   end
 end;

record_by_id(_,_) ->
 {error,badarg}.


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


%% DESCRIPTION:  Print the information on a location along with all its sublocations and devices
%%
%% ARGUMENTS:    - Loc_id: The loc_id of the location to print information
%%
%% RETURNS:      - If found, information on the location along with all its sublocations and devices
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
		print_sub_in_loc(Subloclist);
		
	   [] ->
	    mnesia:abort(location_not_exists)
      end
     end,
 
 Result = mnesia:transaction(F),
 case Result of
  {atomic, ok} ->
   ok;
  _ ->
   Result
 end;
 
print_location(_) ->
 {error,badarg}.


print_sub_in_loc([]) ->
 io:format("~n");
print_sub_in_loc([Subloc|T]) ->

 % Retrieve the list of devices in the sublocation
 DevList = mnesia:match_object(#device{sub_id=Subloc#sublocation.sub_id, _ = '_'}),
 case DevList of
		
  % If the sublocation is empty, just print its information
  [] ->
   io:format("|--~s (empty)~n",[io_lib:format("~p",[Subloc])]);
		  
  % Otherwise, also print all its devices 
  _ ->
   io:format("|--~s~n",[io_lib:format("~p",[Subloc])]),
   print_dev_in_sub_in_loc(DevList)
 end,
 print_sub_in_loc(T).
 
print_dev_in_sub_in_loc([]) ->
 ok;
print_dev_in_sub_in_loc([Dev|T]) ->
 io:format("|  |--~s~n",[io_lib:format("~p",[Dev])]),
 print_dev_in_sub_in_loc(T).


%% DESCRIPTION:  Prints the number of records in each of the database's disc_copies tables
%%
%% ARGUMENTS:    none
%%
%% RETURNS:      The number of records in each of the database's disc_copies tables
%%
%% THROWS:       none  
recordnum() ->

 % Retrieve the number of records in each of the database's disc_copies tables and print it:
 LocationKeysNum = get_table_keys_num(location),
 SublocationKeysNum = get_table_keys_num(sublocation),
 DeviceKeysNum = get_table_keys_num(device),
 io:format("~p location(s), ~p sublocation(s), ~p device(s)~n",[LocationKeysNum,SublocationKeysNum,DeviceKeysNum]).
 
 
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