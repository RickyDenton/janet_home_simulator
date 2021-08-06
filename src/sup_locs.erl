-module(sup_locs).
-export([init/1,start_link/0]).
-behaviour(supervisor).

%% --- Supervisor Callback Functions --- %%

%% Sets its children' general specifications (called by sup_jsim at initialization)
init(_) ->
{ok,
 {{simple_one_for_one,5,60},     % {RestartStrategy, MaxRestarts, Time Period}
  [
   % -- Children General Specification -- %
   {
    sup_loc,                           % ChildID
    {sup_loc,start_link,[]},           % Child Start Function
	temporary,                         % Child Restart Policy
	10000,                             % Sub-tree Max Shutdown Time
	supervisor,                        % Child Type
	[sup_loc]                          % Child Modules (For Release Handling Purposes)
   }
  ]
 }
}.

%% --- Exported API --- %%

start_link() ->
 supervisor:start_link({local,?MODULE},?MODULE,[]).