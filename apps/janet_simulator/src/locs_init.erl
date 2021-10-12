%% This module represents the locations' tree boot initializer in the Janet Simulator application %%

-module(locs_init).

-export([locs_init/0]).   % Process Body
-export([spawn_link/0]).  % Start Function

%%====================================================================================================================================
%%                                                         PROCESS BODY                                                        
%%==================================================================================================================================== 

%% DESCRIPTION:  Initializes the locations' tree at boot from the contents of the Mnesia 'location' table
%%
%% ARGUMENTS:    none
%%
%% RETURNS:      - ok                   -> Locations' tree successfully initialized
%%               - {error,no_locations} -> The Mnesia location table is empty, no location was started
%%
locs_init() ->

 % Retrieve the number of records in the database disc_copies tables 
 {N_Loc,N_Subloc,N_Dev,_,_,_} = db:get_records_num(all),
 if
 
  % If there is at least a location in the database
  N_Loc > 0 ->
   
   % Report that the initialization process is starting by printing the number of records in the database disc_copies tables
   io:format("[locs_init]: The database contains ~w location(s), ~w sublocation(s) and ~w device(s), spawning the locations' trees...~n",[N_Loc,N_Subloc,N_Dev]),
   
   % Retrieve the IDs of all locations in the database and use them for spawning their associated
   % location supervisors (sup_loc) under the locations' tree top supervisor (sup_locs)
   LocationIDs = db:get_table_keys(location),
   spawn_sup_loc(LocationIDs);
   
  % Otherwise, if the location table is empty
  true ->
   
   % Report that no location tree can be started
   io:format("[locs_init]: <WARNING> No location is present in the database, no location tree will be started~n"),
   {error,no_locations}
 end.
  
%% Spawns the location supervisors (sup_loc) associated to each 'loc_id' in a list (locs_init() helper function)
spawn_sup_loc([]) ->
 ok;  
spawn_sup_loc([Loc_id|NextLoc_Id]) ->
 {ok,_Sup_pid} = supervisor:start_child(sup_locs,[Loc_id]),
 spawn_sup_loc(NextLoc_Id).


%%====================================================================================================================================
%%                                                         START FUNCTION                                                        
%%====================================================================================================================================

%% Called by the Janet Simulator top-level supervisor (sup_jsim) at boot time
spawn_link() ->
 {ok,proc_lib:spawn_link(?MODULE,locs_init,[])}.  % The first 'ok' parameter is for attuning to standard interface of an OTP supervision tree