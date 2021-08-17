-module(sup_loc).
-behaviour(supervisor).
-export([init/1,start_link/1]).
-include("table_records.hrl").   % Mnesia table records definition

%% ================================================ SUPERVISOR CALLBACK FUNCTIONS ================================================ %%

%% DESCRIPTION:  Initializes the location supervisor, registering its PID into the suploc table
%%
%% ARGUMENTS:    - [Loc_id]: The loc_id of the location associated to this location supervisor
%%
%% RETURNS:      - The "SupFlags" and "ChildSpec" parameters as of supervisor behaviour specification
%%                 (in particular, the loc_init process is created)
%%
%% THROWS:       none 
init([Loc_id]) ->

 % Register the location supervisor in the suploc table
 {atomic,ok} = mnesia:transaction(fun() -> mnesia:write(#suploc{loc_id=Loc_id,sup_pid=self()}) end),

 % {ok,SupFlags,ChildSpec} supervisor behaviour return parameters
 {ok,
  {{one_for_one,1,5},                                   % {RestartStrategy, MaxRestarts, Time Period}
   [
    % -- loc_init Location Initializer -- %
    {
     loc_init,                                     		% ChildID
     {loc_init,spawn_link,[Loc_id,self()]},             % Child Start Function
 	 temporary,                        				    % Child Restart Policy 
	 200,                                               % Sub-tree Max Shutdown Time
	 worker,                                            % Child Type
	 [loc_init]                                         % Child Modules (For Release Handling Purposes)
    }
   ]
  }
 }.
  
%% ======================================================== START FUNCTION ======================================================== %%

%% Called by "locs_init" at initialization and dynamically when starting new locations (TODO: add function name when ready)
start_link(Loc_id) ->
 supervisor:start_link(?MODULE,[Loc_id]).