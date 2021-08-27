%% This module offers functions for interfacing with the Mnesia RAM-only database on a Janet Controller node %%

-module(ctr_db).
-export([init_mnesia/1]).
-export([print_tree/0,print_table/0,print_table/1,get_table_records/1]).

-include("ctr_mnesia_tables_definitions.hrl").  % Janet Cotnroller Mnesia Tables Records Definitions


%% DESCRIPTION:  Starts Mnesia in disc-less and permanent mode and initializes
%%               the tables used by the JANET Controller application
%%
%% ARGUMENTS:    - DevAlloc: The location devices' sublocations allocation
%%
%% RETURNS:      - ok                               -> Mnesia started and tables successfully initialized
%%               - {error,janet_controller_running} -> The JANET Controller is already running
%%
init_mnesia(DevAlloc) ->

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
   {atomic,ok} = mnesia:create_table(devalloc,
                                     [
									  {attributes, record_info(fields, devalloc)},
                                      {ram_copies, [node()]}
									 ]),
									
   {atomic,ok} = mnesia:create_table(devhandler,
                                     [
									  {attributes, record_info(fields, devhandler)},
                                      {ram_copies, [node()]}
									 ]),
									
   % Initialize the contents of the 'devalloc' table via the "DevAlloc" variable
   ok = initialize_devalloc(DevAlloc),

   % Add the special 'all' record to the 'devalloc' table
   % containing the 'dev_id's of all location devices
   LocDevIds = lists:flatten([ SublocDevList || {_,SublocDevList} <- DevAlloc ]),
   ok = mnesia:dirty_write(#devalloc{subloc_id = all,devlist = LocDevIds})
 end.
   
 
%% Initializes the contents of the 'devalloc' table (init_mnesia(DevAlloc) helper function)
initialize_devalloc([]) ->
 ok;
initialize_devalloc([{Subloc_id,SublocDevList}|Next_SubAlloc]) ->

 % Create and write the devalloc entry in the 'devalloc' table (note that a
 % dirty_write is used since at this moment no other process is using Mnesia)
 ok = mnesia:dirty_write(#devalloc{subloc_id = Subloc_id,devlist = SublocDevList}),
 
 % Continue with the next devalloc entry
 initialize_devalloc(Next_SubAlloc).


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
 print_table_header(devalloc),
 print_table_records(get_table_records(devalloc)),
 print_table_header(devhandler),
 print_table_records(get_table_records(devhandler));

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
print_table_header(devalloc) ->
 io:format("DEVALLOC TABLE {subloc_id,devlist}~n==============~n");
print_table_header(devhandler) ->
 io:format("DEVHANDLER TABLE {dev_id,handler_pid,devserver_pid}~n================~n").

%% Prints all records in a table, or "(empty)" if there are none (print_table(Table) helper function)
print_table_records(TableRecords) when TableRecords == [] ->
 io:format("(empty)~n~n");
print_table_records(TableRecords) ->
 print_table_records_list(TableRecords).

%% Prints all records in a list (print_table_records(TableRecords) helper function)  
print_table_records_list([]) ->
 io:format("~n");
print_table_records_list([Record|NextRecord]) ->
 io:format("~s~n",[io_lib:format("~p",[Record])]),
 print_table_records_list(NextRecord).


%% DESCRIPTION:  Prints the location devices' sublocations allocation as a tree, along
%%               if whether they are currently registered within the controller
%%
%% ARGUMENTS:    None
%%
%% RETURNS:      - ok -> The location devices' sublocations allocation was printed
%%
print_tree() ->

 % Retrieve the location ID of the controller
 {ok,Loc_id} = application:get_env(loc_id),
 
 % Retrieve all records in the 'devalloc' table and:
 %
 % 1) Sort them (by subloc_id)
 % 2) Remove the last element (the special 'all' record)
 % 
 FilteredDevAlloc = lists:droplast(lists:sort(get_table_records(devalloc))),
 
 % Print the tree header
 io:format("~n{location ~w}~n",[Loc_id]),
 
 % Print the rest of the tree
 print_tree_subloc(FilteredDevAlloc),
 
 % Final newline
 io:format("~n").
 
%% Prints the location devices' sublocations allocation tree (print_tree() helper function) 
print_tree_subloc([]) ->
 ok;
print_tree_subloc([DevAlloc|Next_DevAlloc]) ->
 
 % Retrieve the 'subloc_id' and 'devlist' of the DevAlloc record
 Subloc_id = DevAlloc#devalloc.subloc_id,
 SublocDevList = DevAlloc#devalloc.devlist,
 
 case SublocDevList of
  [] ->
  
   % If the sublocation is empty, just print its
   % header and proceed with the next DevAlloc record
   io:format("|--{sublocation ~w} (empty)~n",[Subloc_id]);
   
  _ ->
  
   % Otherwise print the sublocation header
   % and the devices in the sublocation
   io:format("|--{sublocation ~w}~n",[Subloc_id]),

   % Set the printing indentation accordingly on
   % whether this is the last DevAlloc entry   
   case Next_DevAlloc of
    [] ->
     print_tree_devlist(SublocDevList,"   ");
  
    _ ->
     print_tree_devlist(SublocDevList,"|  ")
   end
 end,
 
 % Proceed with the next DevAlloc entry
 print_tree_subloc(Next_DevAlloc).
 
%% Prints the indented list of devices in a sublocation, along if whether they are registered
%% within the controller or not (print_tree_subloc([DevAlloc|Next_DevAlloc]) helper function)
print_tree_devlist([],_) ->
 ok;
print_tree_devlist([Dev_id|Next_Devid],Indentation) ->

 % Determine if the device "Dev_id" is currently registered within the controller
 case mnesia:dirty_read({devhandler,Dev_id}) of
  [] ->
   DevRegStatus = "NOT_REGISTERED";
   
  _ ->
   DevRegStatus = "REGISTERED"
 end,
 
 % Print the device "Dev_id" along with its registration status
 io:format("~s|--{dev-~w} - ~s~n",[Indentation,Dev_id,DevRegStatus]),
 
 % Proceed with the next device, maintaining the indentation
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
 
 
%% DESCRIPTION:  Returns the table name atom associated with its argument, also considering shorthand forms
%%
%% ARGUMENTS:    - Tabletype: A table name, possibly in a shorthand form, with the following being allowed:
%%
%%                             - deva,devall,alloc,devalloc      -> devalloc
%%                             - devh,devhand,devhandler,handler -> devhandler
%%
%% RETURNS:      - Tableatom      -> The table atom name associated with Tabletype
%%               - unknown        -> If no table name could be associated with Tabletype
%%               - {error,badarg} -> Invalid arguments
%%
resolve_tabletype_shorthand(Tabletype) when is_atom(Tabletype) ->
 if
  % --- ram_copies tables --- %
  Tabletype =:= deva orelse Tabletype =:= devall orelse Tabletype =:= alloc orelse Tabletype =:= devalloc ->
   devalloc;
  Tabletype =:= devh orelse Tabletype =:= devhand orelse Tabletype =:= handler orelse Tabletype =:= devhandler ->
   devhandler;

  % Unknown Tabletype  
  true ->
   unknown
 end;
 
resolve_tabletype_shorthand(_) ->
 {error,badarg}.