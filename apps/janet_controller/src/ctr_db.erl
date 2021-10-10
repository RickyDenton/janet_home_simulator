%% This module offers functions for interfacing with the Mnesia RAM-only database on a Janet Controller node %%

-module(ctr_db).

-include("ctr_mnesia_tables_definitions.hrl").  % Janet Controller Mnesia Tables Records Definitions

%% ------------------------------------- PUBLIC CRUD OPERATIONS ------------------------------------- %%
-export([add_sublocation/1,add_device/3]).                                             % Create
-export([print_tree/0,print_table/0,print_table/1,get_table_records/1,get_record/2]).  % Read
-export([update_dev_subloc/2,update_dev_config/3]).                                    % Update
-export([delete_sublocation/1,delete_device/1]).                                       % Delete

%% ------------------------------------ PUBLIC UTILITY FUNCTIONS ------------------------------------ %%
-export([init_mnesia/2]).


%%====================================================================================================================================
%%                                                     PUBLIC CRUD OPERATIONS                                                        
%%==================================================================================================================================== 

%% ========================================================== CREATE ===============================================================%%

%% DESCRIPTION:  Adds an empty sublocation to the location
%%
%% ARGUMENTS:    - Subloc_id: The ID of the sublocation in the location, which must not already exist and be >0
%%
%% RETURNS:      - ok                                 -> The sublocation was successfully added
%%               - {error,sublocation_already_exists} -> A sublocation with such 'sub_id' already exists
%%               - {error,badarg}                     -> Invalid arguments
%%
add_sublocation(Subloc_id) when is_number(Subloc_id), Subloc_id>0 ->
 F = fun() ->

	  % Check if a sublocation with the same Subloc_id already exists on the controller
	  case mnesia:read({ctr_sublocation,Subloc_id}) of
	   [] ->
		  
	    % If it doesn't, add the new sublocation to the 'ctr_sublocation' table'
	    mnesia:write(#ctr_sublocation{subloc_id = Subloc_id, devlist = []});
		  
	   [_SublocationRecord] ->
	   
	    % If a sublocation with such "subloc_id" already exists, return an error
	    mnesia:abort(sublocation_already_exists)
	  end
     end,
 do_transaction(F);

add_sublocation(_) ->
 {error,badarg}.
 
 
%% DESCRIPTION:  Adds a new device in one of the location's sublocations
%%
%% ARGUMENTS:    - Dev_id:    The ID of the device to add, which must not already exist and be >0
%%               - Subloc_id: The ID of the sublocation in the location where to put the device, which must exist be >=0
%%               - Type:      The device's type (fan|light|door|thermostat|conditioner)
%%
%% RETURNS:      - ok                             -> The device was successfully added to the specified sublocation
%%               - {error,invalid_devtype}        -> The device type is invalid
%%               - {error,device_already_exists}  -> A device with such 'dev_id' already exists 
%%               - {error,sublocation_not_exists} -> The 'sub_id' sublocation doesn't exist
%%               - {error,badarg}                 -> Invalid arguments
%%
add_device(Dev_id,Subloc_id,Type) when is_number(Dev_id), Dev_id>0, is_number(Subloc_id), Subloc_id>=0 ->
 F = fun() ->
 
      % Check the target sublocation to exist
	  case mnesia:wread({ctr_sublocation,Subloc_id}) of
	   [Sublocation] ->
	    
		% Check if a device with the same "Dev_id" already exists
		case mnesia:read({ctr_device,Dev_id}) of
		 [] ->
		 
		  % If it doesn't exist, add the new device in the 'ctr_device' table
	      ok = mnesia:write(#ctr_device{dev_id = Dev_id, subloc_id = Subloc_id, type = Type, config = '-', lastupdate = '-', handler_pid = '-'}),
		  
		  % Append the "Dev_id" to the list of devices in the sublocation	
		  UpdatedDevList = lists:append(Sublocation#ctr_sublocation.devlist,[Dev_id]),
		  mnesia:write(Sublocation#ctr_sublocation{devlist = UpdatedDevList});
	
		 [_DeviceRecord] ->
		 
		  % If a device with the same "Dev_id" already exists, return an error
		  mnesia:abort(device_already_exists)
		end;
		
	   [] ->
	    
		% If the target sublocation does not exist, return an error
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
   do_transaction(F)
 end;
 
add_device(_,_,_) ->
 {error,badarg}. 
 
 
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
 print_table_header(ctr_sublocation),
 print_table_records(get_table_records(ctr_sublocation)),
 print_table_header(ctr_device),
 print_table_records(get_table_records(ctr_device));

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
print_table_header(ctr_sublocation) ->
 io:format("CTR_SUBLOCATION TABLE {subloc_id,devlist}~n=====================~n");
print_table_header(ctr_device) ->
 io:format("CTR_DEVICE TABLE {dev_id,subloc_id,type,config,lastupdate,handler_pid}~n================~n").
  
%% Prints all records in a table, or "(empty)" if there are none (print_table(Table) helper function)
print_table_records([]) ->
 io:format("(empty)~n~n");
print_table_records(TableRecords) ->
 print_table_records_list(TableRecords).

%% Prints all records in a list (print_table_records(TableRecords) helper function)  
print_table_records_list([]) ->
 io:format("~n");
print_table_records_list([Record|NextRecord]) ->
 io:format("~s~n",[io_lib:format("~300p",[Record])]),
 print_table_records_list(NextRecord).


%% DESCRIPTION:  Prints all location's sublocations and their devices as a tree,
%%               along if whether the latters are paired with the controller 
%%
%% ARGUMENTS:    None
%%
%% RETURNS:      - ok -> The location's sublocations and devices tree was printed
%%
print_tree() ->

 % Retrieve the controller's location ID
 %
 % NOTE: The get_env/2 is used for allowing this function to be called from a process
 %       not belonging to the 'janet_controller' application (debugging purposes)
 %
 {ok,Loc_id} = application:get_env(janet_controller,loc_id),
 
 % Retrieve all record from the 'ctr_sublocation' table and sort them
 SortedSublocList = lists:sort(get_table_records(ctr_sublocation)),
 
 % Print the tree header
 io:format("~n{location: ~w}~n",[Loc_id]),
 
 % Print the rest of the tree
 print_tree_subloc(SortedSublocList),
 
 % Final newline
 io:format("~n").

%% Prints the location's sublocations and their devices as a tree (print_tree() helper function) 
print_tree_subloc([]) ->
 ok;
print_tree_subloc([SublocRecord|Next_SublocRecord]) ->
 
 % Retrieve the sublocation 'subloc_id'
 Subloc_id = SublocRecord#ctr_sublocation.subloc_id,
 
 % Retrieve the sublocation 'devlist' ordered by "Dev_id"
 SublocDevList = lists:sort(SublocRecord#ctr_sublocation.devlist),
 
 case SublocDevList of
  [] ->
  
   % If the sublocation is empty, just print its header
   io:format("|--{sublocation,~w,[]} (empty)~n",[Subloc_id]);
   
  _ ->
  
   % Otherwise print the sublocation header
   % and the list of devices in the sublocation
   io:format("|--{sublocation,~w,~w}~n",[Subloc_id,SublocDevList]),

   % Set the printing indentation accordingly on
   % whether this is the last DevAlloc entry   
   case Next_SublocRecord of
    [] ->
     print_tree_devlist(SublocDevList,"   ");
  
    _ ->
     print_tree_devlist(SublocDevList,"|  ")
   end
 end,
 
 % Proceed with the next sublocation record
 print_tree_subloc(Next_SublocRecord).
 
%% Prints the indented list of devices in a sublocation, along if whether they are paired
%% within the controller or not (print_tree_subloc([DevAlloc|Next_DevAlloc]) helper function)
print_tree_devlist([],_) ->
 ok;
print_tree_devlist([Dev_id|Next_Devid],Indentation) ->

 % Retrieve the 'ctr_device' record associated with "Dev_id"
 [DevRecord] = mnesia:dirty_read({ctr_device,Dev_id}),
 
 % Determine if the device is paired with the
 % controller via the 'handler_pid' field
 case DevRecord#ctr_device.handler_pid of
  '-' ->
   DevStatus = "OFFLINE";
  _ ->
   DevStatus = "ONLINE"
 end,
 
 % Determine the device's 'config' and 'lastupdate' values to be printed depending on
 % whether the controller received at least one configuration update from the device
 case DevRecord#ctr_device.config of
 
  % If no configuration updates were received from the device
  '-' ->
   Config = '-',
   LastUpdate = '-';
   
  % If at least one configuration update was received from the device
  _ ->
   Config = utils:deprefix_dev_config(DevRecord#ctr_device.config),
   LastUpdate = string:slice(calendar:system_time_to_rfc3339(DevRecord#ctr_device.lastupdate,[{time_designator,$\s}]),0,19)
 end,
 
 % Print the device information
 io:format("~s|--{device,~w,~w,~p,~s} - ~s~n",[Indentation,Dev_id,DevRecord#ctr_device.type,Config,LastUpdate,DevStatus]),
 
 % Proceed with the next device, preserving the indentation
 print_tree_devlist(Next_Devid,Indentation).


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
 
 
%% DESCRIPTION:  Returns a table record by key, if it exists
%%
%% ARGUMENTS:    - Tabletype: The table where to search the record, also considering shorthand forms
%%               - Key:       The record key (>=0)
%%
%% RETURNS:      - {ok,Record}           -> The record with key "Key" in table "Tabletype"
%%               - {error,not_found}     -> The record with key "Key" was not found in table "Tabletype"
%%               - {error,unknown_table} -> Unknown table
%%               - {error,badarg}        -> Invalid arguments
%%
get_record(Tabletype,Key) when is_atom(Tabletype) ->

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


%% ========================================================== UPDATE ===============================================================%% 
 
%% DESCRIPTION:  Updates a device's sublocation
%%
%% ARGUMENTS:    - Dev_id:    The ID of the device to change sublocation, which must exist and be >0
%%               - Subloc_id: The 'sub_id' of the sublocation where to put the device, which must exist and be {>0,>=0}
%%
%% RETURNS:      - ok                             -> Device sublocation successfully updated
%%               - {error,device_not_exists}      -> The device 'Dev_id' does not exist
%%               - {error,sublocation_not_exists} -> The sublocation 'subloc_id' does not exist
%%               - {error,badarg}                 -> Invalid arguments
%%
update_dev_subloc(Dev_id,Subloc_id) when is_number(Dev_id), Dev_id>0, is_number(Subloc_id), Subloc_id>=0 ->
 F = fun() ->
 
      % Check the device to exist
	  case mnesia:wread({ctr_device,Dev_id}) of      % wread = write lock
	   [Device] ->
		if 
		 Device#ctr_device.subloc_id =:= Subloc_id ->
         
		  % If the current and new sublocations coincide, return		 
		  ok;

         true ->
		 
		  % Otherwise, check the target sublocation to exist
		  case mnesia:wread({ctr_sublocation,Subloc_id}) of
		   [NewSubloc] ->
		   
		    % If it exists, remove the device from the 'devlist' of its current sublocation
			[CurrSubloc] = mnesia:wread({ctr_sublocation,Device#ctr_device.subloc_id}),	
			UpdatedCurrSublocDevList = lists:delete(Dev_id,CurrSubloc#ctr_sublocation.devlist),
			ok = mnesia:write(CurrSubloc#ctr_sublocation{devlist = UpdatedCurrSublocDevList}),
			
			% Insert the device in the 'devlist' of its new sublocation
			UpdatedNewSublocDevlist = lists:append(NewSubloc#ctr_sublocation.devlist,[Dev_id]),
			ok = mnesia:write(NewSubloc#ctr_sublocation{devlist = UpdatedNewSublocDevlist}),
			
			% Change the device's sublocation in the 'ctr_device' record
            ok = mnesia:write(Device#ctr_device{subloc_id = Subloc_id});
		  
		   [] ->
		   
		    % If the target sublocation does not exist, return an error
		    mnesia:abort(sublocation_not_exists)
		  end
	    end;
		
	   [] ->
	   
	    % If the device does not exist, return an error
	    mnesia:abort(device_not_exists)
      end
     end,
 do_transaction(F);
	 
update_dev_subloc(_,_) ->
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
	  case mnesia:wread({ctr_device,Dev_id}) of      % wread = write lock
	   [Device] ->
	   
	    % If it does, ensure the updated configuration to be valid
		case catch(utils:is_valid_devconfig(Config,Device#ctr_device.type)) of
		 ok ->
		     
	      % If it is, push it in the 'device' table along with its timestamp
		  UpdatedDevice = Device#ctr_device{config = Config, lastupdate = Timestamp},
		  mnesia:write(UpdatedDevice);
			
		 {error,invalid_devconfig} ->
		   
		  % Otherwise, if it is not valid (which SHOULD NOT happen), report the error and abort the transaction
		  io:format("[ctr_db:update_dev_config]: <WARNING> An invalid device configuration was passed (Config = ~p, DevType = ~p)~n",[Config,Device#ctr_device.type]),
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
 
 
%% ========================================================== DELETE ===============================================================%%  
 
%% DESCRIPTION:  Deletes a sublocation from the controller, moving all its devices to the location's default sublocation "0"
%%
%% ARGUMENTS:    - Subloc_id: The ID of the sublocation to be deleted (>0)
%%
%% RETURNS:      - ok                             -> The sublocation was successfully deleted and its devices
%%                                                   were moved to the location's default sublocation "0"
%%               - {error,sublocation_not_exists} -> The sublocation 'Subloc_id' does not exist
%%               - {error,badarg}                 -> Invalid arguments
%%
%% NOTE:         Default sublocations cannot be removed (Subloc_id > 0)
%%
delete_sublocation(Subloc_id) when is_number(Subloc_id), Subloc_id>0 -> 
 F = fun() ->
 
      % Check the sublocation to exist
	  case mnesia:wread({ctr_sublocation,Subloc_id}) of      % wread = write lock
	   [Sublocation] ->

		 % If it does, move all its devices to the location's default sublocation "0"
		 [DefaultSubloc] = mnesia:wread({ctr_sublocation,0}),
		 UpdatedDefaultSublocDevlist = lists:append(DefaultSubloc#ctr_sublocation.devlist,Sublocation#ctr_sublocation.devlist),
		 ok = mnesia:write(DefaultSubloc#ctr_sublocation{devlist = UpdatedDefaultSublocDevlist}),
		 
		 % Update the devices' 'subloc_id's in the 'ctr_device' table
		 ok = move_devlist_to_default_subloc(Sublocation#ctr_sublocation.devlist),
		 
		 % Remove the sublocation from the 'ctr_sublocation' table
		 ok = mnesia:delete({ctr_sublocation,Subloc_id});
		
	   [] ->
	    
		% If the sublocation does not exist, return an error
	    mnesia:abort(sublocation_not_exists)
      end
     end,
 do_transaction(F);

delete_sublocation(_) ->
 {error,badarg}.

%% Moves all devices in a list to the default sublocation "0" (delete_sublocation(Subloc_id) helper function)
move_devlist_to_default_subloc([]) ->
 ok;
move_devlist_to_default_subloc([Dev_id|Next_Devid]) ->

 % Retrieve the device's record from the 'ctr_device' table
 [Device] = mnesia:wread({ctr_device,Dev_id}),
 
 % Change the device to the location's default sublocation
 mnesia:write(Device#ctr_device{subloc_id = 0}),
 
 % Proceed with the next device
 move_devlist_to_default_subloc(Next_Devid). 
 
 
%% DESCRIPTION:  Deletes a device from the location, also terminating its handler if it paired
%%
%% ARGUMENTS:    - Dev_id: The 'dev_id' of the device to delete, which must exist and be >0
%%
%% RETURNS:      - ok                        -> The device was deleted from the location, and
%%                                              its handler was terminated if it was paired
%%               - {error,device_not_exists} -> The device 'Dev_id' does not exist
%%               - {error,badarg}            -> Invalid arguments
%%
delete_device(Dev_id) when is_number(Dev_id), Dev_id>0 ->
 F = fun() ->
 
      % Check the device to exist
	  case mnesia:wread({ctr_device,Dev_id}) of      % wread = write lock
	   [Device] ->

         % Remove the device from its sublocation's 'devlist'
		 [DeviceSubloc] = mnesia:wread({ctr_sublocation,Device#ctr_device.subloc_id}),
		 UpdatedSublocDevlist = lists:delete(Dev_id,DeviceSubloc#ctr_sublocation.devlist),
		 ok = mnesia:write(DeviceSubloc#ctr_sublocation{devlist = UpdatedSublocDevlist}),
		 
		 % Remove the device from the 'ctr_device' table
		 ok = mnesia:delete({ctr_device,Dev_id}),
		 
		 % Return the Devhandler_pid required for terminating the device handler
		 Device#ctr_device.handler_pid;
		 
	   [] ->
	   
	    % If the device does not exist, return an error
	    mnesia:abort(device_not_exists)
      end
     end,
 
 % Attempt the transaction
 case mnesia:transaction(F) of
  {atomic,'-'} ->
 
   % If the transaction was successful and the
   % device handler is not running, simply return
   ok;
   
  {atomic,Devhandler_pid} ->
  
   % If the transaction was successful and the device handler IS still running,
   % attempt to terminate it handler via the 'sup_devhandlers' supervisor	
   %
   % NOTE: Since the only error that can be returned by the following function call is
   %       "{error,not_found}", which is relative to the fact that the handler terminated
   %       before its execution (which is expected due to it monitoring a device node that
   %       supposedly stopped), the result of the handler's termination can be ignored
   supervisor:terminate_child(sup_devhandlers,Devhandler_pid),
   ok;
   
  {aborted,Reason} ->
  
   % If an error occured in the transaction, return it
   {error,Reason}
 end;
 
delete_device(_) ->
 {error,badarg}.

 
%%====================================================================================================================================
%%                                                    PUBLIC UTILITY FUNCTIONS
%%====================================================================================================================================  
 
%% DESCRIPTION:  Starts Mnesia in disc-less and permanent mode and initializes
%%               the tables used by the JANET Controller application
%%
%% ARGUMENTS:    - CtrSublocTable: The serialized controller's 'ctr_sublocation' table ([{subloc_id,devlist}])
%%               - CtrDeviceTable: The serialized controller's 'ctr_device' table      ([{dev_id,subloc_id,type,config,lastupdate,handler_pid}])

%% RETURNS:      - ok                               -> Mnesia started and tables successfully initialized
%%               - {error,janet_controller_running} -> The JANET Controller is already running
%%
init_mnesia(CtrSublocTable,CtrDeviceTable) ->

 % Check if the JANET Controller is running
 case utils:is_running(janet_controller) of
  true ->
  
   % If it is, return an error
   {error,janet_controller_running};
  
  false ->
   
   % Otherwise start Mnesia in disc-less mode (i.e. using a RAM schema)
   % and permanent mode (i.e. the node shuts down if it terminates)
   ok = application:set_env(mnesia,schema_location,ram),
   ok = application:start(mnesia,permanent),
   
   % Initialize the ram_copies tables used by the JANET Controller
   {atomic,ok} = mnesia:create_table(ctr_sublocation,
                                     [
									  {attributes, record_info(fields, ctr_sublocation)},
                                      {ram_copies, [node()]}
									 ]),
									
   {atomic,ok} = mnesia:create_table(ctr_device,
                                     [
									  {attributes, record_info(fields, ctr_device)},
                                      {ram_copies, [node()]}
									 ]),
   
   % Initialize the contents of the controller's 'ctr_sublocation' and 'ctr_device' tables
   ok = init_ctr_sublocation_table(CtrSublocTable),
   ok = init_ctr_device_table(CtrDeviceTable)
 end.
 
%% Initializes the contents of the controller's 'ctr_sublocation' table (init_mnesia(CtrSublocTable,CtrDeviceTable) helper function)
init_ctr_sublocation_table([]) ->
 ok;
init_ctr_sublocation_table([{Subloc_id,SublocDevList}|Next_Subloc]) ->
 
 % Write the sublocation record in the 'ctr_sublocation' table
 ok = mnesia:dirty_write(#ctr_sublocation{subloc_id = Subloc_id, devlist = SublocDevList}),
 
 % Proceed with the next sublocation record
 init_ctr_sublocation_table(Next_Subloc).
 
%% Initializes the contents of the controller's 'ctr_device' table (init_mnesia(CtrSublocTable,CtrDeviceTable) helper function) 
init_ctr_device_table([]) ->
 ok;
init_ctr_device_table([{Dev_id,Subloc_id,Type}|Next_Dev]) ->
 
 % Write the sublocation record in the 'ctr_sublocation' table
 ok = mnesia:dirty_write(#ctr_device{dev_id = Dev_id, subloc_id = Subloc_id, type = Type, config = '-', lastupdate = '-', handler_pid = '-'}),
 
 % Proceed with the next device record
 init_ctr_device_table(Next_Dev). 
 

%%====================================================================================================================================
%%                                                    PRIVATE HELPER FUNCTIONS
%%====================================================================================================================================  

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
%%                             - sub,subloc,sublocation,ctr_sub,ctr_subloc,ctr_sublocation -> ctr_sublocation
%%                             - dev,device,ctr_dev,ctr_device                             -> ctr_device
%%
%% RETURNS:      - Tableatom             -> The table atom name associated with Tabletype
%%               - {error,unknown_table} -> If no table name could be associated with Tabletype
%%               - {error,badarg}        -> Invalid arguments
%%
resolve_tabletype_shorthand(Tabletype) when is_atom(Tabletype) ->
 if
  % --- ram_copies tables --- %
  Tabletype =:= sub     orelse Tabletype =:= subloc     orelse Tabletype =:= sublocation     orelse
  Tabletype =:= ctr_sub orelse Tabletype =:= ctr_subloc orelse Tabletype =:= ctr_sublocation        ->
   ctr_sublocation;
  Tabletype =:= dev orelse Tabletype =:= device orelse Tabletype =:= ctr_dev orelse Tabletype =:= ctr_device ->
   ctr_device;

  % Unknown Tabletype  
  true ->
   {error,unknown_table}
 end;
 
resolve_tabletype_shorthand(_) ->
 {error,badarg}.
