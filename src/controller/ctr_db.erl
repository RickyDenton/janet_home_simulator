%% This module offers functions for interfacing with the Mnesia RAM-only database on a Janet Controller node %%

-module(ctr_db).
-export([init_mnesia/1]).
-export([print_tree/0,print_table/0,print_table/1,get_table_records/1,get_record/2]).

-include("ctr_mnesia_tables_definitions.hrl").  % Janet Controller Mnesia Tables Records Definitions


%% DESCRIPTION:  Starts Mnesia in disc-less and permanent mode and initializes
%%               the tables used by the JANET Controller application
%%
%% ARGUMENTS:    - CtrSublocTable: The serialized controller's 'ctr_sublocation' table ([{subloc_id,subloc_devlist}])
%%
%% RETURNS:      - ok                               -> Mnesia started and tables successfully initialized
%%               - {error,janet_controller_running} -> The JANET Controller is already running
%%
init_mnesia(CtrSublocTable) ->

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
   
   % Initialize the contents of the two tables via the "CtrSublocTable" variable
   ok = init_ctr_tables(CtrSublocTable)
 end.
   
 
%% Initializes the contents of the controller's 'sublocation' and 'device' tables (init_mnesia(CtrSublocTable) helper function)
%%
%% NOTE: dirty_writes are used for improving performance since at this time
%%       no other process is using the Mnesia database on the controller node
%%
init_ctr_tables([]) ->
 ok;
init_ctr_tables([{Subloc_id,SublocDevList}|Next_SubAlloc]) ->

 % Write the {Subloc_id,SublocDevlist} entry in the 'ctr_sublocation' table
 ok = mnesia:dirty_write(#ctr_sublocation{subloc_id = Subloc_id, devlist = SublocDevList}),
 
 % Initialize the 'ctr_device' records of all devices in the sublocation 
 ok = init_ctr_dev_records(SublocDevList,Subloc_id),
 
 % Initialize the next 'ctr_sublocation' entry
 init_ctr_tables(Next_SubAlloc).

%% Initializes the 'ctr_device' records associated with the list of devices in a sublocation (init_ctr_tables([{Subloc_id,SublocDevList}|Next_SubAlloc]) helper function)
init_ctr_dev_records([],_) ->
 ok;
init_ctr_dev_records([Dev_id|Next_Devid],Subloc_id) ->

 % Write the {Dev_id,Subloc_id,-} entry to the 'ctr_device' table
 ok = mnesia:dirty_write(#ctr_device{dev_id = Dev_id, subloc_id = Subloc_id, handler_pid = '-'}),
 
 % Initialize the next 'ctr_device' entry
 init_ctr_dev_records(Next_Devid,Subloc_id).


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

print_table() ->
 print_table(all).
 
%% Prints a table's header (print_table() helper function) 
print_table_header(ctr_sublocation) ->
 io:format("CTR_SUBLOCATION TABLE {subloc_id,devlist}~n=====================~n");
print_table_header(ctr_device) ->
 io:format("CTR_DEVICE TABLE {dev_id,subloc_id,handler_pid}~n================~n").
  
%% Prints all records in a table, or "(empty)" if there are none (print_table(Table) helper function)
print_table_records([]) ->
 io:format("(empty)~n~n");
print_table_records(TableRecords) ->
 print_table_records_list(TableRecords).

%% Prints all records in a list (print_table_records(TableRecords) helper function)  
print_table_records_list([]) ->
 io:format("~n");
print_table_records_list([Record|NextRecord]) ->
 io:format("~s~n",[io_lib:format("~p",[Record])]),
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
 io:format("~n{location ~w}~n",[Loc_id]),
 
 % Print the rest of the tree
 print_tree_subloc(SortedSublocList),
 
 % Final newline
 io:format("~n").

%% Prints the location's sublocations and their devices as a tree (print_tree() helper function) 
print_tree_subloc([]) ->
 ok;
print_tree_subloc([SublocRecord|Next_SublocRecord]) ->
 
 % Retrieve the 'subloc_id' and 'devlist' of the "SublocRecord"
 Subloc_id = SublocRecord#ctr_sublocation.subloc_id,
 SublocDevList = SublocRecord#ctr_sublocation.devlist,
 
 case SublocDevList of
  [] ->
  
   % If the sublocation is empty, just print its header
   io:format("|--{sublocation ~w} (empty)~n",[Subloc_id]);
   
  _ ->
  
   % Otherwise print the sublocation header
   % and the list of devices in the sublocation
   io:format("|--{sublocation ~w}~n",[Subloc_id]),

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
 
 % Print the device "Dev_id" along on whether he is paired with the controller
 io:format("~s|--{dev-~w} - ~s~n",[Indentation,Dev_id,DevStatus]),
 
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
  unknown ->
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
  unknown ->
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
 
 
%% DESCRIPTION:  Returns the table name atom associated with its argument, also considering shorthand forms
%%
%% ARGUMENTS:    - Tabletype: A table name, possibly in a shorthand form, with the following being allowed:
%%
%%                             - sub,subloc,sublocation,ctr_sub,ctr_subloc,ctr_sublocation -> ctr_sublocation
%%                             - dev,device,ctr_dev,ctr_device                             -> ctr_device
%%
%% RETURNS:      - Tableatom      -> The table atom name associated with Tabletype
%%               - unknown        -> If no table name could be associated with Tabletype
%%               - {error,badarg} -> Invalid arguments
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
   unknown
 end;
 
resolve_tabletype_shorthand(_) ->
 {error,badarg}.