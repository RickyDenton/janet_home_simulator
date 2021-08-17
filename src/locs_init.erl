-module(locs_init).
-export([spawn_link/0,locs_init/0]). 


%% ======================================================= MODULE FUNCTIONS ======================================================= %%

%% DESCRIPTION:  Initializes the location supervisors (sup_loc) of every location at startup
%%
%% ARGUMENTS:    none
%%
%% RETURNS:      - ok: location supervisors successfully initialized
%%               - {error,no_locations}: no locations are present in the database, and so no
%%                                       location supervisor can be started
%%
%% THROWS:       none 
locs_init() ->

 % Retrieve the number of records in the database disc_copies tables 
 {N_Loc,N_Subloc,N_Dev,_,_,_} = db:get_records_num(all),
 if
 
  % If there is at least a location in the database
  N_Loc > 0 ->
   
   % Print a summary of the database's contents
   io:format("[locs_init]: The database contains ~w location(s), ~w sublocation(s) and ~w device(s), initializing the location supervisors...~n",[N_Loc,N_Subloc,N_Dev]),
   
   % Retrieve all location IDs in the database and spawn the associated "sup_loc" supervisors
   LocationIDs = db:get_table_keys(location),
   spawn_sup_loc(LocationIDs);
   
  true ->
   
   % Print a warning that no location is present in the database, and so no location supervisor can be started
   io:format("[locs_init]: WARNING: the database contains no location, no location supervisor will be started~n"),
   {error,no_locations}
 end.
  
%% Spawns the location supervisors (sup_loc) associated to a list of loc_ids (locs_init() helper function)
spawn_sup_loc([]) ->
 ok;  
spawn_sup_loc([Loc_id|NextLoc_Id]) ->
 {ok,_Sup_pid} = supervisor:start_child(sup_locs,[Loc_id]),
 spawn_sup_loc(NextLoc_Id).


%% ======================================================== START FUNCTION ======================================================== %%

%% Called by the "sup_jsim" supervisor following its initialization
spawn_link() ->
 {ok,spawn_link(?MODULE,locs_init,[])}.