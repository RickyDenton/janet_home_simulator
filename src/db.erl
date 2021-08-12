-module(db).
-export([add_location/4,add_sublocation/2,subtry/0,read_location/1,install/0,clear/0,reset/0,reset/1,dump/0,dump/1,print_table/1,info/0]).

%%====================================================================================================================================%%
%%                                               MNESIA TABLES RECORDS DEFINITIONS                                                    %%
%%====================================================================================================================================%%

%% --- disc_copies tables --- %%

-record(location,     	% A location
        {
		 loc_id,       	% The location/controller's ID (must be unique)
		 name,        	% The location's name          (optional)
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
	    
		% Check if the sublocation already exists
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
%%               - File   -> a custom file path is used
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


%% DESCRIPTION:  Prints all records in a specified table
%%
%% ARGUMENTS:    The table which record are to be print
%%
%% RETURNS:      The list of records in the specified table
%%
%% THROWS:       none  
print_table(Table) ->
 mnesia:dirty_select(Table,[{'_',[],['$_']}]). % The second argument is a "catch-all" clause
 
 
%% DESCRIPTION:  Prints the number of records in each of the database's disc_copies tables
%%
%% ARGUMENTS:    none
%%
%% RETURNS:      The number of records in each of the database's disc_copies tables
%%
%% THROWS:       none  
info() ->

 % Start Mnesia, if it is not running
 PrevStatus = jsim:is_running(mnesia),
 if
  PrevStatus =:= false ->
   jsim:start_mnesia();
  true ->
   ok
 end,
 
  % Retrieve the number of records in each of the database's disc_copies tables
  %timer:sleep(10),      % This sleep is required to solve a race condition in Mnesia loading the sublocation scheme (even if wait_for_tables/2 is used...)
  LocationKeysNum = get_table_keys_num(location),
  SublocationKeysNum = get_table_keys_num(sublocation),
  DeviceKeysNum = get_table_keys_num(device),
  
 % If it was not running, stop Mnesia again 
 if
  PrevStatus =:= false ->
   jsim:stop_mnesia();
  true ->
   ok
 end,
 
 % Print the number of records in each of the database's disc_copies tables
 io:format("~nDATABASE CONTENTS~n=================~n- ~p location(s)~n- ~p sublocation(s)~n- ~p device(s)~n",[LocationKeysNum,SublocationKeysNum,DeviceKeysNum]).
 
 
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