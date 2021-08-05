-module(sup_jsim).
-export([init/1,start_link/0]).
-behaviour(supervisor).

%% --- Supervisor Callback Functions --- %%

%% Initializes the application's main components (called at the application start)
init(_) ->
{ok,
 {{one_for_one,5,60},     % {RestartStrategy, MaxRestarts, Time Period}
  [
   % -- sup_locations (the locations' top supervisor)
   {
    sup_locations,                     % ChildID
    {sup_locations,start_link,[]},     % Child Start Function
	permanent,                         % Child Restart Policy
	10000,                             % Sub-tree Max Shutdown Time
	supervisor,                        % Child Type
	[sup_locations]                    % Child Modules (For Release Handling Purposes)
   },
   
   % -- Is sup_locations initializer necessary? -- %
   
   % -- sim_restserver (the simulator's REST server)
   {
    sim_restserver,		               % ChildID
    {sim_restserver,start_link,[]},    % Child Start Function
	permanent,                         % Child Restart Policy
	5000,                              % Sub-tree Max Shutdown Time
	worker,                  	       % Child Type
	[sim_restserver]                   % Child Modules (For Release Handling Purposes)
   }
  ]
}}.

%% --- Exported API --- %%

start_link() ->
 supervisor:start_link(?MODULE,[]).

