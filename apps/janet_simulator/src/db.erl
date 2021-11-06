%% This module offers functions for interfacing with the Mnesia database on the Janet Simulator node %%

-module(db).

-include("sim_mnesia_tables_definitions.hrl").  % Janet Simulator Mnesia Tables Records Definitions

%% ------------------------------------- PUBLIC CRUD OPERATIONS ------------------------------------- %%
-export([add_location/5,add_sublocation/2,add_device/5]).                                                    % Create
-export([print_table/0,print_table/1,print_tree/0,print_tree/1,print_tree/2,get_record/2,                    % Read
         get_table_records/1,get_table_keys/0,get_table_keys/1,get_records_num/0,get_records_num/1,
		 get_loc_devs/1,get_subloc_devs/1,get_manager_info/2,get_suploc_pid/1]).  
-export([update_dev_subloc/2,update_dev_config/3,update_loc_name/2,update_subloc_name/2,update_dev_name/2]). % Update
-export([delete_location/1,delete_sublocation/1,delete_device/1]).                                           % Delete

%% ------------------------------------ PUBLIC UTILITY FUNCTIONS ------------------------------------ %%
-export([mnesia_startup/1,backup/0,backup/1,restore/0,restore/1,clear/0,install/0,stop_mnesia/0]). 


%%====================================================================================================================================
%%                                                     PUBLIC CRUD OPERATIONS                                                        
%%==================================================================================================================================== 

%% NOTE: All the public CRUD operations crash if Mnesia is NOT started on the node (which should be performed automatically at its start)

%% ========================================================== CREATE =============================================================== %%

%% DESCRIPTION:  Adds an empty location to the database with its (default) sublocation {Loc_id,0}, and starts up its controller
%%
%% ARGUMENTS:    - Loc_id:   The ID of the location to add, which must not already exist and be >0
%%               - Name:     The name of the location (optional)
%%               - User:     The username of the location's owner (optional)
%%               - Port:     The port by which the location's controller listens for REST requests (unique int >= 30000)
%%               - HostName: The hostname where to deploy the location controller node (a list)
%%
%% RETURNS:      - {ok,ok}                         -> The location was successfully added and its controller node was started
%%               - {ok,Error}                      -> The location was successfully added, but starting its controller returned an Error
%%               - ok                              -> The location was successfully added (but the controller node
%%                                                    was not started since the JANET Simulator is not running)
%%               - {error,location_already_exists} -> The loc_id already exists in the "location" table 
%%               - {error,port_already_taken}      -> The port is already used by another controller
%%               - {error,host_port_taken}         -> The port is already taken by another process in the host OS
%%               - {error,invalid_hostname}        -> The hostname does not belong to the list of
%%                                                    allowed hosts JANET nodes can be deployed in
%%               - {error,badarg}                  -> Invalid arguments
%%
add_location(Loc_id,Name,User,Port,HostName) when is_number(Loc_id), Loc_id>0, is_number(Port), Port >= 30000, is_list(HostName) ->
 F = fun() ->
 
      % Check if the location already exists
	  case mnesia:read({location,Loc_id}) of
	   [] ->
	    
		% Check if the port is already taken
		case mnesia:match_object(#location{port = Port, _ = '_'}) of
		 [] ->
		 
		  % Check the HostName to belong to the list of
		  % allowed hosts JANET nodes can be deployed in
		  case utils:is_allowed_node_host(HostName) of
		  
		   true ->
		  
	        % If it is, ensure the specified Port to be
			% currently available on the node host OS
		    case utils:is_remotehost_port_available(HostName,Port) of
			
			 true ->
			 
			  % If it is and no location name was provided, use a default one
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
              mnesia:write(#location{loc_id=Loc_id,name=LocName,user=LocUser,port=Port,hostname=HostName}),
		      mnesia:write(#sublocation{sub_id={Loc_id,0}, name="(default)", devlist=[]});
		  
		     false ->
			
			  % If the Port is NOT available in the host OS, return an error
		      mnesia:abort(host_port_taken)
		    end;
			
		   false ->
		  
		    % If the HostName does not belong to the list of allowed
		    % hosts JANET nodes can be deployed in, return an error
		    mnesia:abort(invalid_hostname)
		  end;		  
		  
		 [_LocationRecord] ->
		 
		  % If the port is already taken, return an error
		  mnesia:abort(port_already_taken)
		end;
		
	   [_LocationRecord] ->
	   
	    % If the location already exists, return an error
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
 
add_location(_,_,_,_,_) ->
 {error,badarg}.


%% DESCRIPTION:  Adds a new empty sublocation to the database
%%
%% ARGUMENTS:    - {Loc_id,Subloc_id}: The sub_id of the sublocation, which must not already exist and be >0
%%               - Name:               The name of the sublocation (optional)
%%
%% RETURNS:      - ok                                 -> The sublocation was successfully added to the database
%%               - {error,location_not_exists}        -> The location 'Loc_id' does not exist
%%               - {error,sublocation_already_exists} -> A sublocation with such 'sub_id' already exists
%%               - {error,badarg}                     -> Invalid arguments
%%
%% CONSISTENCY:  If the associated location controller is running, consistency with its own database is enforced
%%               either in the "jsim:add_sublocation()" (if the operation originated from the JANET Simulator) or
%%               in the "ctr_resthandler:add_sublocation_handler()" (if the operation originated from the JANET
%%               Controller) functions.
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
 do_transaction(F);
	  
add_sublocation(_,_) ->
 {error,badarg}.


%% DESCRIPTION:  Adds a new device with a predefined initial configuration into the
%%               database and, if JANET Simulator is running, also starts its device node
%%
%% ARGUMENTS:    - Dev_id:             The ID of the device, which must not already exist and be >0
%%               - Name:               The device's name (optional)
%%               - {Loc_id,Subloc_id}: The device's sub_id, which must exist and with Subloc_id >=0
%%               - Type:               The device's type, which must belong to the set of valid device types
%%               - HostName:           The hostname where to deploy the device node (a list)
%%
%% RETURNS:      - {ok,ok}                        -> The device was successfully added
%%                                                   and its device node was started
%%               - {ok,Error}                     -> The device was successfully added, but
%%                                                   an Error occured in starting its node
%%               - ok                             -> The device was successfully added, but the device node
%%                                                   was not started since the JANET Simulator is not running
%%               - {error,invalid_devtype}        -> The device type is invalid
%%               - {error,device_already_exists}  -> A device with such 'dev_id' already exists 
%%               - {error,sublocation_not_exists} -> The 'sub_id' sublocation doesn't exist
%%               - {error,invalid_hostname}       -> The hostname does not belong to the list of
%%                                                   allowed hosts JANET nodes can be deployed in
%%               - {error,badarg}                 -> Invalid arguments
%%
%% CONSISTENCY:  If the associated location controller is running, consistency with its own database is enforced either
%%               in the "jsim:add_device()" (if the operation originated from the JANET Simulator) or in the
%%               "ctr_resthandler:add_device_handler()" (if the operation originated from the JANET Controller) functions
%%
add_device(Dev_id,Name,{Loc_id,Subloc_id},Type,HostName) when is_number(Dev_id), Dev_id>0, is_number(Loc_id), Loc_id>0,
                                                               is_number(Subloc_id), Subloc_id>=0, is_list(HostName) ->
 F = fun() ->
 
      % Check the target sublocation to exist
	  case mnesia:wread({sublocation,{Loc_id,Subloc_id}}) of
	   [Sublocation] ->
	    
		% Check if a device with the same Dev_id already exists
		case mnesia:read({device,Dev_id}) of
		 [] ->
		 
		  % Check the HostName to belong to the list of
		  % allowed hosts JANET nodes can be deployed in
		  case utils:is_allowed_node_host(HostName) of
		  
		   true ->
 
            % If it does and its name is empty, use a default one
		    if
		     Name =:= "" ->
		      DevName = "dev-" ++ integer_to_list(Dev_id);
		     true ->
		      DevName = Name
		    end,
		  
		    % Insert the new device in the Device table
	        mnesia:write(#device{dev_id=Dev_id,name=DevName,sub_id={Loc_id,Subloc_id},type=Type,
			                     config=utils:get_devtype_default_config(Type),lastupdate=erlang:system_time(second),hostname = HostName}),
		  
		    % Update the "DevList" in the sublocation table  
		    UpdatedDevList = lists:append(Sublocation#sublocation.devlist,[Dev_id]),
		    UpdatedSublocation = Sublocation#sublocation{devlist=UpdatedDevList},
		    mnesia:write(UpdatedSublocation);
		  
		   false ->
		  
		    % If the HostName does not belong to the list of allowed
		    % hosts JANET nodes can be deployed in, return an error
		    mnesia:abort(invalid_hostname)
		  end;		
		  
		 [_DeviceRecord] ->
		 
		  % If a device with such "Dev_id"
		  % already exists, return an error
		  mnesia:abort(device_already_exists)
		end;
		
	   [] ->
	   
	    % If the target sublocation does
		% not exist, return an error
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
 
add_device(_,_,_,_,_) ->
 {error,badarg}.


%% Attempts to spawn a device manager under its location 'sup_loc' supervisor
%% (add_device(Dev_id,Name,{Loc_id,Subloc_id},Type) helper function)
spawn_devmanager(Dev_id,Loc_id) ->

 % Attempt to retrieve the PID of the location's 'sup_loc' supervisor
 case get_suploc_pid(Loc_id) of 
  {error,Reason} ->
   
    % If an error occured in attempting to retrieve the PID of the location's 'sup_loc'
	% supervisor, the location was deleted concurrently with this operation, or there is
	% a consistency error with the 'suploc' table, and in any case, return the error
	{error,Reason};
	
  Sup_pid ->
	
   % Retrieve the HostName of the location controller from its record in the 'location' table
   %
   % NOTE: A dirty_read is used being we interested in a single constant field
   %
   [{location,Loc_id,_Name,_User,_Port,HostName}] = mnesia:dirty_read({location,Loc_id}),
	
   % If the PID of the location's 'sup_loc' supervisor was successfully retrieved,
   % define the child specification of the device manager to be spawned
   DevMgrSpec = {
                 "dev-" ++ integer_to_list(Dev_id),                  % ChildID
                 {dev_manager,start_link,[Dev_id,Loc_id,HostName]},  % Child Start Function
	             transient,                                          % Child Restart Policy (transient to account for node host connection failures)
                 14000,                                              % Child Sub-tree Max Shutdown Time
	             worker,                  	                         % Child Type
	             [dev_manager]                                       % Child Modules (For Release Handling Purposes)
                },

   % Attempt to spawn the device manager as a child of the 'sup_loc' supervisor
   case supervisor:start_child(Sup_pid,DevMgrSpec) of
  
    % If the spawning was successful, return just its result 
    {ok,_DevMgrPid} ->
     ok;
   
    % Otherwise, return the error raised in the spawning
    SpawnError ->
     SpawnError
   end
 end.


%% =========================================================== READ ================================================================ %% 

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
 io:format("LOCATION TABLE {loc_id,name,user,port,hostname}~n==============~n");
print_table_header(sublocation) ->
 io:format("SUBLOCATION TABLE {sub_id,name,devlist}~n=================~n");
print_table_header(device) -> 
 io:format("DEVICE TABLE {dev_id,name,sub_id,type,config,lastupdate,hostname}~n============~n");
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
 io:format("~s~n",[io_lib:format("~300p",[Record])]),
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

%% Print the entire database contents
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

%% Returns the list of unique users in the database (print_tree(), print_tree(all) helper function)
get_all_users() ->

 % Initialize select arguments
 MatchHead = #location{user='$1', _='_'}, % Consider all locations
 Guard = [],                              % No guard
 Result = '$1',                           % Return only the location's users
 
 % Retrieve the users of all locations in the database
 UnfilteredUserList = mnesia:dirty_select(location,[{MatchHead, Guard, [Result]}]),
 
 % Remove duplicates and return the list of users
 lists:usort(UnfilteredUserList).


%% Print the contents associated with a specific user
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
  
  
%% Prints a location's or sublocation's contents
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
   
  ctrmanager ->
  
   % FIX: reidirect to location
   print_tree(location,Id);
 
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
 
 % Retrieve the list of user's locations and sort them by "Loc_id"
 Loclist = lists:sort(mnesia:dirty_match_object(#location{user = User, _ = '_'})),

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
 
 % Retrieve the list of sublocations in the location and sort them by "sub_id"
 %
 % NOTE: Each location contains at least the "(default)" sublocation {Loc_id,0} 
 %
 Subloclist = lists:sort(mnesia:dirty_match_object(#sublocation{sub_id = {Loc#location.loc_id,'_'}, _ = '_'})),
 
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
 io:format("~s{location,~w,~p,~p,~w,~s} - ~s~n",[Indent,Loc#location.loc_id,Loc#location.name,
                                                 Loc#location.user,Loc#location.port,Loc#location.hostname,CtrMgrStatus]),
 
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


%% Prints all devices in a list of sublocations as a tree (print_tree(sublocation,Sub_id), print_tree_location([Loc|NextLoc],Indent) helper function)
print_tree_sublocation([],_,_) ->
 ok;
print_tree_sublocation([Subloc|NextSubloc],Indent1,Indent2) ->

 % Retrieve the list of devices in the sublocation and sort them by "Dev_id"
 Devlist = lists:sort(mnesia:dirty_match_object(#device{sub_id=Subloc#sublocation.sub_id, _ = '_'})),
 
 case Devlist of
  [] ->
   
   % If the sublocation is empty, just print its information
   io:format("~s{sublocation,~w,~p,[]} (empty)~n",[Indent1++Indent2,Subloc#sublocation.sub_id,Subloc#sublocation.name]);
		  
  _Devices ->
   
   % Otherwise also print the devices within the location as a tree, taking the indentation into account
   io:format("~s{sublocation,~w,~p,~w}~n",[Indent1++Indent2,Subloc#sublocation.sub_id,Subloc#sublocation.name,Subloc#sublocation.devlist]),
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
 io:format("~s{device,~w,~s,~w,~w,~p,~s,~s} - ~s~n",[Indent, Dev#device.dev_id,io_lib:format("~p",[Dev#device.name]), Dev#device.sub_id, Dev#device.type, utils:deprefix_dev_config(Dev#device.config),
										     		 string:slice(calendar:system_time_to_rfc3339(Dev#device.lastupdate,[{time_designator,$\s}]),0,19),Dev#device.hostname, DevMgrStatus]),
 % Parse the next device in the list
 print_tree_device(NextDev,Indent).


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
 
 
%% ========================================================== UPDATE =============================================================== %% 

%% DESCRIPTION:  Updates a device's sublocation
%%
%% ARGUMENTS:    - Dev_id:             The ID of the device to change sublocation, which must exist and be >0
%%               - {Loc_id,Subloc_id}: The 'sub_id' of the sublocation where to put the device, where "Loc_id" must be >0
%%                                     and coincide with the current device's location, and the "Subloc_id" must be >=0
%%
%% RETURNS:      - ok                             -> The device's sublocation was successfully updated
%%               - {error,device_not_exists}      -> The device 'Dev_id' does not exist
%%               - {error,sublocation_not_exists} -> The sublocation 'sub_id' does not exist
%%               - {error,different_locations}    -> The specified and the current sublocations are in different locations
%%               - {error,badarg}                 -> Invalid arguments
%%
%% CONSISTENCY:  If the associated location controller is running, consistency with its own database is enforced either
%%               in the "jsim:update_dev_subloc()" (if the operation originated from the JANET Simulator) or in the
%%               "ctr_resthandler:update_dev_subloc_handler()" (if the operation originated from the JANET Controller) functions.
%%
update_dev_subloc(Dev_id,{Loc_id,Subloc_id}) when is_number(Dev_id), Dev_id>0, is_number(Loc_id), Loc_id>0, is_number(Subloc_id), Subloc_id>=0 ->
 F = fun() ->
 
      % Check the device to exist
	  case mnesia:wread({device,Dev_id}) of      % wread = write lock
	   [Device] ->
	   
	    % Retrieve the device's current sublocation
	    CurrSublocID = Device#device.sub_id,
	
		if 
		
		 % If the current and new sublocations coincide, return
		 CurrSublocID =:= {Loc_id,Subloc_id} ->  
		  ok;

         % If the current and new sublocations are in different locations, return an error
		 element(1,CurrSublocID) =/= Loc_id ->
		  mnesia:abort(different_locations);

         % Otherwise, check the target sublocation to exist
		 true ->
		  case mnesia:wread({sublocation,{Loc_id,Subloc_id}}) of
		   [NewSubloc] ->
		   
		    % If it exists, remove the device from the 'devlist' of its current sublocation
			[CurrSubloc] = mnesia:wread({sublocation,CurrSublocID}),		
			UpdatedCurrSublocDevlist = lists:delete(Dev_id,CurrSubloc#sublocation.devlist),
			UpdatedCurrSubloc = CurrSubloc#sublocation{devlist=UpdatedCurrSublocDevlist},
			mnesia:write(UpdatedCurrSubloc),
			
			% Insert the device in the 'devlist' of its new sublocation
			UpdatedNewSublocDevList = lists:append(NewSubloc#sublocation.devlist,[Dev_id]),
			UpdatedNewSubloc = NewSubloc#sublocation{devlist = UpdatedNewSublocDevList},
			mnesia:write(UpdatedNewSubloc),
			
			% Change the device's sublocation in the 'device' record
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
 
%% ========================================================== DELETE =============================================================== %% 

%% DESCRIPTION:  Deletes a location, along with all its sublocations and devices, from the
%%               database, also stopping their associated controller and devices nodes
%%
%% ARGUMENTS:    - Loc_id: The ID of the location to delete from the database
%%
%% RETURNS:      - {ok,ok}                     -> The location and all its sublocations and devices were
%%                                                successfully deleted, and their associated nodes were stopped
%%               - {ok,Error}                  -> The location and all its sublocation and devices were successfully
%%                                                deleted, but an Error raised in stopping their associated nodes
%%               - ok                          -> The location and all its sublocations and devices were
%%                                                successfully deleted, while their associated nodes
%%                                                were not since the JANET Simulator is not running
%%               - {error,location_not_exists} -> The location 'Loc_id' does not exist
%%               - {error,badarg}              -> Invalid arguments
%%
%% NOTE:         Use with caution, for locations, sublocations and devices in this way CANNOT be recovered
%%
delete_location(Loc_id) when is_number(Loc_id), Loc_id>0 ->
 F = fun() ->
 
      % Check the location to exist
	  case mnesia:wread({location,Loc_id}) of      % wread = write lock
	   [_Location] ->
		
		% Delete all devices in the location from the device table
		LocationDevList = mnesia:match_object(#device{sub_id = {Loc_id,'_'}, _ = '_'}),
		ok = delete_device_records(LocationDevList),
		
		% Delete all sublocations in the location from the sublocation table
		LocationSublocList = mnesia:match_object(#sublocation{sub_id = {Loc_id,'_'}, _ = '_'}),
		ok = delete_subloc_records(LocationSublocList),
		
		% Depending on whether the JANET Simulator is running
		case utils:is_running(janet_simulator) of
		 true ->
		
		  % If it is, attempt to delete the location 'sup_loc' supervisor and all
		  % its managers' records from the 'ctrmanager' and 'devmanager' tables
		  DeleteSuplocRes = catch(delete_suploc(Loc_id)),
		
		  % Delete the location from the location table
		  ok = mnesia:delete({location,Loc_id}),
		
		  % Return 'ok' and the result of deleting the location 'sup_loc' supervisor, its 'suploc'
		  % record, and all its managers' records from the 'ctrmanager' and 'devmanager' tables
		  {ok,DeleteSuplocRes};
		  
		 false ->
		 
		  % Otherwise if the JANET Simulator is not running,
		  % simply delete the location from the 'location' table
		  ok = mnesia:delete({location,Loc_id})
		end;
		
	   [] ->
	    mnesia:abort(location_not_exists)
      end
     end,
 do_transaction(F);

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
 
 % Attempt to terminate the 'sup_loc' supervisor via its own 'sup_locs' supervisor, 
 supervisor:terminate_child(sup_locs,SupLocPid),
 
 % Remove the 'sup_loc' supervisor entry from the 'suploc' table
 mnesia:delete({suploc,Loc_id}),

 % Remove the location controller manager entry from the 'ctrmanager' table
 mnesia:delete({ctrmanager,Loc_id}),
 
 % Retrieve the 'dev_id's of all device managers in the location
 MatchHead = #devmanager{dev_id='$1', loc_id=Loc_id, _='_'}, % Select all 'dev_id
 Guard = [],
 Result = '$1',
 LocDevIds = mnesia:select(devmanager,[{MatchHead,Guard,[Result]}]),
		
 % Delete the entries associated with devices in the location from the 'devmanager' table
 delete_devmgr_records(LocDevIds),

 % Return the success of the operation
 ok.

%% Deletes a list of devmanager records from the devmanager table (delete_location(Loc_id) -> delete_suploc(Loc_id) helper function)
delete_devmgr_records([]) ->
 ok;
delete_devmgr_records([Dev_id|NextDevId]) ->
 mnesia:delete({devmanager,Dev_id}),
 delete_devmgr_records(NextDevId). 


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
%% CONSISTENCY:  If the associated location controller is running, consistency with its own database is enforced either
%%               in the "jsim:delete_sublocation()" (if the operation originated from the JANET Simulator) or in the
%%               "ctr_resthandler:delete_sublocation_handler()" (if the operation originated from the JANET Controller) functions.
%%
delete_sublocation({Loc_id,Subloc_id}) when is_number(Loc_id), Loc_id>0, is_number(Subloc_id), Subloc_id>0 -> 
 F = fun() ->
 
      % Check the sublocation to exist
	  case mnesia:wread({sublocation,{Loc_id,Subloc_id}}) of      % wread = write lock
	   [Sublocation] ->

		 % If it does, move all its devices to the location's default sublocation {Loc_id,0}
		 SublocDevList = Sublocation#sublocation.devlist,
		 [DefaultSubloc] = mnesia:wread({sublocation,{Loc_id,0}}),
		 UpdatedDefaultSublocDevlist = lists:append(DefaultSubloc#sublocation.devlist,SublocDevList),
		 UpdatedDefaultSubloc = DefaultSubloc#sublocation{devlist=UpdatedDefaultSublocDevlist},
		 mnesia:write(UpdatedDefaultSubloc),
		 
		 % Update the devices' 'sub_id's in the 'device' table
		 move_devlist_to_default_subloc(SublocDevList,Loc_id),
		 
		 % Remove the sublocation from the sublocation table
		 mnesia:delete({sublocation,{Loc_id,Subloc_id}});
		
	   [] ->
	    mnesia:abort(sublocation_not_exists)
      end
     end,
 do_transaction(F);

delete_sublocation(_) ->
 {error,badarg}.

%% Moves all devices in a list to the default sublocation {Loc_id,0} (delete_sublocation(Loc_id,Subloc_id) helper function)
move_devlist_to_default_subloc([],_) ->
 ok;
move_devlist_to_default_subloc([Dev_id|NextDev_id],Loc_id) ->

 % Retrieve the device's record from the 'device' table
 [Device] = mnesia:wread({device,Dev_id}),
 
 % Change the device to the location's default sublocation
 mnesia:write(Device#device{sub_id={Loc_id,0}}),
 
 % Proceed with the next device
 move_devlist_to_default_subloc(NextDev_id,Loc_id).


%% DESCRIPTION:  Deletes a device from the database and, if it is running, stops its device node
%%
%% ARGUMENTS:    - Dev_id: The 'dev_id' of the device to delete, which must exist and be >0
%%
%% RETURNS:      - {ok,ok}                   -> The device was deleted from the
%%                                              database and its node was stopped
%%               - {ok,Error}                -> The device was deleted from the database,  
%%                                              but an Error was raised in stopping its node
%%               - ok                        -> The device was deleted from the database, while its node
%%                                              was not stopped since the JANET Simulator is not running
%%               - {error,device_not_exists} -> The device 'Dev_id' does not exist
%%               - {error,badarg}            -> Invalid arguments
%%
%% CONSISTENCY:  If the associated location controller is running, consistency with its own database is enforced
%%               either in the "jsim:delete_device()" (if the operation originated from the JANET Simulator) or
%%               in the "ctr_resthandler:delete_device_handler()" (if the operation originated from the JANET
%%               Controller) functions.
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
		 
		 % Depending on whether the JANET Simulator is running
		 case utils:is_running(janet_simulator) of
		  true ->
		
		   % If it is, retrieve the device's location ID
		   {Loc_id,_} = Device#device.sub_id,
		
		   % Attempt to stop the device manager via its 'sup_loc'
		   % supervisor and delete its entry from the 'devmanager' table
		   StopDevMgrRes = catch(stop_devmanager(Dev_id,Loc_id)),
		
		   % Delete the device from the 'device' table
		   ok = mnesia:delete({device,Dev_id}),
		 
		   % Return 'ok' and the result of attempting to stop the device manager via
		   % its 'sup_loc' supervisor and delete its entry from the 'devmanager' table 
		   {ok,StopDevMgrRes};
		   
		  false ->
		 
		   % Otherwise if the JANET Simulator is not running,
		   % simply delete the device from the 'device' table
		   ok = mnesia:delete({device,Dev_id})
		 end;
		 
	   [] ->
	    mnesia:abort(device_not_exists)
      end
     end,
 do_transaction(F);

delete_device(_) ->
 {error,badarg}.

%% Attempts to stop a device manager via its 'sup_loc' supervisor and delete its
%% entry from the 'devmanager' table (delete_device(Dev_id) helper function)
stop_devmanager(Dev_id,Loc_id) ->

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
	
     % Otherwise delete the device manager entry from the 'devmanager' table
     mnesia:delete({devmanager,Dev_id})
   end
 end.
 

%%====================================================================================================================================
%%                                                    PUBLIC UTILITY FUNCTIONS
%%==================================================================================================================================== 

%% DESCRIPTION:  Ensures the Mnesia database to be running and in a consistent state to be used by the JANNET Simulator application
%%
%% ARGUMENTS:    - AutoInstall: A boolean() specifying whether the Mnesia database should be automatically installed if it is missing
%%
%% RETURNS:      - ok -> The Mnesia database is ready to be used by the JANET Simulator application
%%
%% THROWS:       - {error,{mnesia,{mnesia_dir,no_read_permission}}} -> The current OS user lacks the 'read' permission on the Mnesia directory
%%               - {error,{mnesia,{install,MnesiaInstallError}}}    -> Error in installing the Mnesia database
%%               - {error,{mnesia,{start,StartMnesiaRes}}}          -> Internal error in starting the Mnesia application
%%               - {error,{mnesia,{load,timeout}}}                  -> Timeout in loading the Mnesia tables
%%               - {error,{mnesia,{load,LoadErrorReason}}}          -> Internal error in loading the Mnesia tables
%%               - {error,badarg}                                   -> Invalid arguments
%%
mnesia_startup(AutoInstall) when is_boolean(AutoInstall) ->

 try
 
  % Check the JANET Simulator Mnesia database to be installed,
  % possibly automatically installing it if it is missing
  ok = check_install_mnesia(AutoInstall),
  
  % If stopped, attempt to start the Mnesia application
  ok = check_start_mnesia(),
  
  % Wait for the Mnesia 'disc_copies' tables
  % used by the JANET Simulator to be loaded
  ok = wait_mnesia_tables()
  
  % At this point the Mnesia database is ready
  % to be used by the JANET Simulator application

 catch

  %% -------------------------------- Mnesia Startup Errors -------------------------------- %%
  
  % NOTE: Most errors are logged and all are rethrown as {error,{mnesia,Reason}}
  % ----
  
  % The current OS user lacks the 'read' permission on the Mnesia directory
  {error,no_read_permission} ->
   io:format("<FATAL> The current OS user lacks the 'read' permission on the Mnesia directory, the Mnesia application cannot be started~n"),
   throw({error,{mnesia,{mnesia_dir,no_read_permission}}});
   
  % Error in automatically installing the Mnesia database
  {error,{mnesia_install,MnesiaInstallError}}  ->
   throw({error,{mnesia,{install,MnesiaInstallError}}});
   
  % The Mnesia database is not installed and its automatic installation is disabled
  {error,mnesia_not_installed} ->
   io:format("<FATAL> The Mnesia database is not installed~n"),
   throw({error,{mnesia,not_installed}});
   
  % Internal error in starting the Mnesia application
  {error,{mnesia_start,StartMnesiaRes}} ->
   io:format("<FATAL> Internal error in starting the Mnesia application (~w)~n",[StartMnesiaRes]),
   throw({error,{mnesia,{start,StartMnesiaRes}}});
   
  % Timeout in loading the Mnesia tables
  {error,{mnesia_load,timeout}} ->
   io:format("<FATAL> Timeout in loading the Mnesia tables required by the JANET Simulator~n"),
   throw({error,{mnesia,{load,timeout}}});

  % Internal error in loading the Mnesia tables
  {error,{mnesia_load,LoadErrorReason}} ->
   io:format("<FATAL> Internal error in loading the Mnesia tables required by the JANET Simulator (~w)~n",[LoadErrorReason]),
   throw({error,{mnesia,{load,LoadErrorReason}}})

 end;
 
% Non-boolean argument
mnesia_startup(_NonBooleanAutoInstall) ->
 {error,bararg}.


%% Checks for the JANET Simulator database to be installed, possibly installing it
%% automatically if it is missing (mnesia_startup(AutoInstall) helper function)
%%
%% THROWS:  - {error,{mnesia_install,MnesiaInstallError}} -> Internal error in installing the Mnesia database
%%          - {error,mnesia_not_installed}                -> The Mnesia database is not installed (and
%%                                                           it should not be automatically installed)
%%
check_install_mnesia(AutoInstall) ->

 % Depending on whether the JANET Simulator database is installed
 % and if it should be automatically installed if missing
 case {check_mnesia_installed(),AutoInstall} of
  {installed,_} ->
  
   % If it is installed, return 'ok'
   ok;
   
  {not_installed,true} ->
  
   % If it is not installed and should be automatically
   % installed, inform the user of the installation attempt
   io:format("The Mnesia database is not installed, installing it now~n"),
	
   % Attempt to install the JANET Simulator Mnesia database
   case install() of
 	ok ->
	 
	 % If it was successfully installed, return 'ok'
	 ok;
	 
	{error,MnesiaInstallError} ->
	
     % Otherwise throw the error occured in its installation
	 throw({error,{mnesia_install,MnesiaInstallError}})
   end;
	
  {not_installed,false} ->
  
   % If the JANET Simulator Mnesia database is not installed
   % and it should not automatically installed, throw an error
   throw({error,mnesia_not_installed})
 end.


%% Checks the JANET Simulator Mnesia database to be installed
%% (mnesia_startup(AutoInstall) -> check_install_mnesia(AutoInstall) helper function
%%
%% THROWS:  - {error,no_read_permission} -> The current OS user lacks the 'read'
%%                                          permission on the Mnesia directory
%%
check_mnesia_installed() ->

 % Retrieve the Mnesia "dir" environment variable
 {ok,MnesiaDir} = application:get_env(mnesia,dir),

 % Attempt to read the contents of the Mnesia directory
 case file:list_dir(MnesiaDir) of
 
  {error,eacces} ->
  
   % If the current OS user lacks the 'read' permission
   % on the Mnesia directory, throw an error
   throw({error,no_read_permission}); 
  
  {error,enoent} ->
  
   % If the directory does not exist, the Mnesia database is not installed
   not_installed;
   
  {ok,MnesiaDirFiles} ->
  
   % Otherwise if the contents of the Mnesia directory were retrieved, define the list of files
   % that must be in it for the JANET Simulator database to be installed (schema + tables)
   JANETDBFiles = ["schema.DAT","location.DCD","sublocation.DCD","device.DCD"],

   % Subtract from the list of files required by the JANET
   % Simulator application all files found in the Mnesia directory
   case lists:subtract(JANETDBFiles,MnesiaDirFiles) of
    [] ->
 
     % If no file remains it means that all files required by the JANET Simulator
	 % application were found in the Mnesia directory, and so the database is installed
     installed;
 
    _LeftoverFiles ->
	
	 % Otherwise if one or more file required by the JANET Simulator
	 % application are missing, the database is NOT installed
	 not_installed
   end
 end.	
	
	
%% If stopped, attempts to start the Mnesia application (mnesia_startup(AutoInstall) helper function)
%%
%% THROWS:  - {error,{mnesia_start,StartMnesiaRes}} -> Internal error in starting the Mnesia application
%%
check_start_mnesia() ->

 % Whathever its current state, attempt to start the Mnesia application
 StartMnesiaRes = application:start(mnesia),
  
 if
  StartMnesiaRes =/= 'ok' andalso StartMnesiaRes =/= {error,{already_started,mnesia}} ->
   
   % If the Mnesia application is still not running, throw the associated internal error
   throw({error,{mnesia_start,StartMnesiaRes}});
	
  true ->
   
   % If it is running, return 'ok'
   ok
 end.
 

%% Waits for the Mnesia 'disc_copies' tables used by the JANET
%% Simulator to be loaded (mnesia_startup(AutoInstall) helper function)
wait_mnesia_tables() -> 
  
 % Wait for the Mnesia 'disc_copies' tables used by the JANET
 % Simulator to be loaded up to a predefined timeout (3 seconds)
 case mnesia:wait_for_tables([location,sublocation,device],3000) of
	
  % If a timeout occured in loading the Mnesia tables, throw an error
  {timeout,_TableList} ->
   throw({error,{mnesia_load,timeout}});
	  
  % If another error occured in loading the Mnesia tables, throw it
  {error,LoadErrorReason} ->
  throw({error,{mnesia_load,LoadErrorReason}});
	
  % Otherwise, if the tables were successfully loaded, return 'ok'	
   ok ->
  ok
 end.
 
 
%% DESCRIPTION:  Backups the entire Mnesia database to a file
%%
%% ARGUMENTS:    - (none):   The default backup file is used ("db/mnesia_backup.db")
%%               - FileName: The backup is saved to the custom file "FileName" under
%%                           the default Mnesia directory ("db/")
%%
%% RETURNS:      - ok                      -> Mnesia database successfully
%%                                            backed up to the specified file
%%               - {file_error,Reason}     -> Error in creating the backup file (its directory must
%%                                            exist and its 'write' permission must be granted)
%%               - {error,{mnesia,Reason}} -> The Mnesia database is not in a consistent state
%%               - {error,badarg}          -> Invalid argument(s)
%% 
%% NOTE:         Mnesia backups can be restored using the restore()/restore(File) functions
%%

% Backup to the default file
backup() ->
 backup("mnesia_backup.db").
 
% Safeguard if an atom FileName was passed
backup(FileName) when is_atom(FileName) ->
 backup(atom_to_list(FileName)); 

% Backup to a custom file 
backup(FileName) when is_list(FileName) ->

 try 
 
  % Ensure the JANET Simulator Mnesia database
  % to be running and in a consistent state
  ok = mnesia_startup(false),

  % Append the "db" directory to the file name
  FilePath = "db/" ++ FileName,
  
  % Attempt to backup the database contents to the specified file
  case mnesia:backup(FilePath) of
   
   ok -> 

    % If the backup was successful, notify it
	io:format("Mnesia database successfully backed up to file \"~s\"~n",[FilePath]);

   {error,BackupReason} ->
	
	% Otherwise, notify the error
    io:format("<WARNING> Error in creating the backup file \"~s\" (check the directory to exist and its 'write' permission to be granted)~n",[FilePath]),
    {file_error,BackupReason}
  end
   
 catch
  {error,{mnesia,Reason}} ->
  
   % If the Mnesia database cannot be started or is not in a consistent state, abort the backup
   io:format("The Mnesia database is not in a consistent state, the backup cannot be created~n"),
   {error,{mnesia,Reason}}
 end;

% The FileName is neither an atom or a list
backup(_) ->
 {error,badarg}.


%% DESCRIPTION:  Restores the Mnesia database to the contents of a backup file
%%
%% ARGUMENTS:    - (none):   The default backup file is used for
%%                           restoring the database ("db/mnesia_backup.db")
%%               - FileName: The database is restored to the contents of the custom
%%                           file "FileName" under the default mnesia directory ("db/")
%%
%% RETURNS:      - ok                       -> Database successfully restored to the
%%                                             contents of the specified backup file
%%               - {error,janet_running}    -> The operation cannot be performed
%%                                             while the JANET Simulator is running
%%               - {error,{restore,Reason}} -> Error in restoring the database from the
%%                                             backup file (check the file to exist and
%%                                             its 'read' permission to have been granted)
%%               - {error,{mnesia,Reason}}  -> The Mnesia database is not in a consistent state
%%               - {error,badarg}           -> Invalid argument(s)
%%
%% NOTES:        1) The current database contents will be DISCARDED by calling this function
%%               2) Database backup files can be created via the backup()/backup(File) functions
%%

% Restore from the default backup file
restore() ->
 restore("mnesia_backup.db").
 
% Safeguard if an atom FileName was passed
restore(FileName) when is_atom(FileName) ->
 restore(atom_to_list(FileName)); 
 
% Restore from a custom backup file
restore(FileName) when is_list(FileName) ->

 try 
 
  % Ensure the JANET Simulator to be stopped
  utils:ensure_jsim_state(stopped),
 
  % Ensure the JANET Simulator Mnesia database
  % to be running and in a consistent state
  ok = mnesia_startup(true),
  
  % Append the "db" directory to the file name
  FilePath = "db/" ++ FileName,
  
  % Attempt to restore its tables to the contents of the specified file 
  case mnesia:restore(FilePath,[{default_op,recreate_tables}]) of
  
   % If the database was successfully restored, report it
   {atomic,_} ->  
	io:format("Mnesia database successfully restored to the contents of the \"~s\" file~n",[FilePath]);
 
   % The backup file does not exist 
   {aborted,enoent} ->
    io:format("<ERROR> The backup file \"~s\" was not found, please ensure the file to be placed in the Mnesia directory (\"db/\")~n",[FilePath]);
   
   % Other error in restoring the database
   {aborted,RestoreReason} ->
	io:format("<FATAL> Error in restoring the Mnesia database (reason = ~w)~n",[RestoreReason]),
	{error,{restore,RestoreReason}}
  end
	
 catch

  % The Mnesia database cannot be restored while the JANET Simulator is running
  {error,janet_running} ->
   io:format("Please stop the JANET Simulator before restoring the Mnesia database~n"),
   {error,janet_running};
  
  % The Mnesia database cannot be started or is not in a consistent state
  {error,{mnesia,Reason}} ->
   io:format("The Mnesia database is not in a consistent state, its contents cannot be restored~n"),
   {error,{mnesia,Reason}}
 end;

% The FileName is neither an atom or a list
restore(_) ->
 {error,badarg}.


%% DESCRIPTION:  Clears (empties) all database tables (but preserves the database's schema)
%%
%% ARGUMENTS:    none
%%
%% RETURNS:      - ok                       -> Database tables successfully cleared
%%               - {error,janet_running}    -> The operation cannot be performed
%%                                             while the JANET Simulator is running
%%               - {error,{mnesia,Reason}}  -> The Mnesia database is not in a consistent state
%%
%% NOTE:         This function is for debugging purposes olny, and should not be called explicitly during the
%%               JANET Simulator operations (use restore()/restore(File) to restore the database's contents)
%%
clear() ->

 try 
 
  % Ensure the JANET Simulator to be stopped
  utils:ensure_jsim_state(stopped),
  
  % Ensure the JANET Simulator Mnesia database
  % to be running and in a consistent state
  ok = mnesia_startup(false),
	
  % Clear all Mnesia tables used by the JANET Simulator application
  [{atomic,ok},{atomic,ok},{atomic,ok},{atomic,ok},{atomic,ok},{atomic,ok}] =
   [mnesia:clear_table(location),mnesia:clear_table(sublocation),mnesia:clear_table(device),
    mnesia:clear_table(suploc),mnesia:clear_table(ctrmanager),mnesia:clear_table(devmanager)],
     
  % Report the success of the operation
  io:format("JANET Simulator Mnesia tables successfully cleared~n")

 catch

  % The Mnesia database cannot be cleared while the JANET Simulator is running
  {error,janet_running} ->
   io:format("Please stop the JANET Simulator before clearing the Mnesia database~n"),
   {error,janet_running};
  
  % The Mnesia database cannot be started or is not in a consistent state
  {error,{mnesia,Reason}} ->
   io:format("The Mnesia database is not in a consistent state, its tables cannot be cleared~n"),
   {error,{mnesia,Reason}}
 end.

 
%% DESCRIPTION:  Installs the JANET Simulator Mnesia database (schema + tables) 
%%
%% ARGUMENTS:    none
%%
%% RETURNS:      - ok                                  -> JANET Simulator Mnesia database successfully installed
%%               - {error,janet_running}               -> The operation cannot be performed
%%                                                        while the JANET Simulator is running
%%               - {error,{mnesia_dir,PosixError}}     -> File system error in reading/writing the Mnesia directory
%%                                                        (check for the apporiate permissions to have been granted)
%%               - {error,{mnesia_schema,SchemaError}} -> Error in attempting to create the Mnesia schema
%%               - {error,{mnesia_stop,StopError}}     -> Internal error in stopping the Mnesia application
%%               - {error,{mnesia_start,StartError}}   -> Internal error in starting the Mnesia application
%%
%% NOTE:         This function is for debugging purposes olny, and should not be called explicitly during the
%%               JANET Simulator operations (use restore()/restore(File) to restore the database's contents)
%%
install() ->

 try

  % Ensure the JANET Simulator to be stopped
  utils:ensure_jsim_state(stopped),
  
  % If it is running, attempt to stop the Mnesia application
  case stop_mnesia() of
  
   % If an error occured in stopping the Mnesia application, throw it
   {error,StopErrorReason} ->
	throw({error,{mnesia_stop,StopErrorReason}});

   % Otherwise, if the Mnesia application is now stopped, continue
   _ ->
    ok
  end,
  
  % Retrieve the Mnesia "dir" environment variable
  {ok,MnesiaDir} = application:get_env(mnesia,dir),
 
  % Attempt to delete the Mnesia directory for removing its schema
  % (as of mnesia:delete_schema([node()])) and any leftover files
  DelMnesiaDirRes = file:del_dir_r(MnesiaDir),
   
  if
  
   % If an error occured in attempting to delete the Mnesia director
   % (apart from the directory not existing), retrieve and throw it  
   DelMnesiaDirRes =/= ok andalso DelMnesiaDirRes =/= {error,enoent} ->
    {error,PosixErrorReason} = DelMnesiaDirRes,
    throw({error,{mnesia_dir,PosixErrorReason}});

   % Otherwise if the Mnesia directory was deleted
   % (or never existed in the first place), continue
   true ->
    ok
  end,
  
  % Attempt to recreate the Mnesia directory with
  % the schema used by the JANET Simulator application
  case mnesia:create_schema([node()]) of
  
   % If an error occured in creating
   % the Mnesia schema, throw it
   {error,SchemaErrorReason} ->
    throw({error,{mnesia_schema,SchemaErrorReason}});
   
   % Otherwise is the schema was
   % successfully created, continue
   ok ->
    ok
  end,
  
  % Attempt to start the Mnesia application
  case application:start(mnesia) of
  
   % If an error occured in starting
   % the application, throw it
   {error,StartErrorReason} ->
    throw({error,{mnesia_start,StartErrorReason}});
	
   % Otherwise if the Mnesia
   % application was started, continue
   ok ->
    ok
  end,

  %% Create the Mnesia tables used by the JANET Simulator application
	 
  % ------------------------------ disc_copies tables ------------------------------ % 
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
   
  % ------------------------------ ram_copies tables ------------------------------ %  
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
	
  % Report that the JANET Simulator Mnesia database was successfully installed	
  io:format("JANET Simulator Mnesia database successfully installed~n")

 catch
 
  %% -------------------------------- Mnesia Install Errors -------------------------------- %%	
  
  % The Mnesia database cannot be reinstalled while the JANET Simulator is running
  {error,janet_running} ->
   io:format("Please stop the JANET Simulator before reinstalling the Mnesia database~n"),
   {error,janet_running};
   
  % Internal error in stopping the Mnesia application
  {error,{mnesia_stop,StopError}} ->
   io:format("The JANET Simulator database cannot be installed while the Mnesia application is running~n"),
   {error,{mnesia_stop,StopError}};
   
  % Error in attempting to delete the Mnesia directory (apart from it not existing)
  {error,{mnesia_dir,PosixError}} ->
   io:format("<FATAL> Error in deleting the Mnesia directory (~w), the JANET Simulator database cannot be installed~n",[PosixError]),
   {error,{mnesia_dir,PosixError}};  
   
  % Error in attempting to create the Mnesia schema
  {error,{mnesia_schema,SchemaError}} ->
   io:format("<FATAL> Error in creating the Mnesia schema (~w), the JANET Simulator database cannot be installed~n",[SchemaError]),
   {error,{mnesia_schema,SchemaError}};   
 
  % Error in attempting to start the Mnesia application
  {error,{mnesia_start,StartError}} ->
   io:format("<FATAL> Error in starting the Mnesia application (~w), the JANET Simulator database cannot be installed~n",[StartError]),
   {error,{mnesia_start,StartError}}

 end.   


%% DESCRIPTION:  Stops the Mnesia application
%%
%% ARGUMENTS:    none
%%
%% RETURNS:      - ok                -> Mnesia successfully stopped
%%               - {error,StopError} -> Internal error in stopping the Mnesia application
%%
%% NOTES:        1) If Mnesia is already stopped, the function returns 'ok'
%%               2) This function is for debugging purposes olny, and should not
%%                  be called explicitly during the JANET Simulator operations
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
  {error,StopError} ->
   io:format("<FATAL> Error in stopping the Mnesia application (reason = ~w)~n",[StopError]),
   {error,StopError}
 end.
 
 
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