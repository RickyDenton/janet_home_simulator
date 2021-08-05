-module(sup_locations).
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
    sup_location,                      % ChildID
    {sup_location,start_link,[]},      % Child Start Function
	temporary,                         % Child Restart Policy
	10000,                             % Sub-tree Max Shutdown Time
	supervisor,                        % Child Type
	[sup_location]                     % Child Modules (For Release Handling Purposes)
   }
  ]
}}.

%% --- Exported API --- %%

start_link() ->
 supervisor:start_link({local,?MODULE},?MODULE,[]).       % NOTE: using dynamic atoms for registration purposes is BAD, find a solution