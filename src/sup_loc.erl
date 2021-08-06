-module(sup_loc).
-export([init/1,start_link/1]).
-behaviour(supervisor).

%% --- Supervisor Callback Functions --- %%

%% Sets its children' general specifications (called by sup_jsim at initialization)
init(LocID) ->

% TODO: ADD REGISTRATION IN THE SUP_LOC_TABLE

{ok,
 {{one_for_one,5,60},     % {RestartStrategy, MaxRestarts, Time Period}
  [
   % -- Location Initializer -- %
   {
    loc_init,                                     		% ChildID
    {loc_init,start_link,[LocID,self()]},               % Child Start Function
	temporary,                        				    % Child Restart Policy
	10000,                                              % Sub-tree Max Shutdown Time
	worker,                                             % Child Type
	[loc_init]                                          % Child Modules (For Release Handling Purposes)
   }
  ]
 }
}.

%% --- Exported API --- %%

start_link(LocID) ->
 supervisor:start_link(?MODULE,[LocID]).