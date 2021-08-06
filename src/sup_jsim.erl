-module(sup_jsim).
-export([init/1,start_link/0]).
-behaviour(supervisor).

%% --- Supervisor Callback Functions --- %%

%% Initializes the application's main components (called at the application start)
init(_) ->
{ok,
 {{rest_for_one,5,60},     % {RestartStrategy, MaxRestarts, Time Period} (rest_for_one for reinitializing everything should sup_location fail)
  [
   % -- sim_restserver (the simulator's REST server)
   {
    sim_restserver,		               % ChildID
    {sim_restserver,start_link,[]},    % Child Start Function
	permanent,                         % Child Restart Policy
	5000,                              % Sub-tree Max Shutdown Time
	worker,                  	       % Child Type
	[sim_restserver]                   % Child Modules (For Release Handling Purposes)
   },
  
   % -- sup_locs (the locations' top supervisor)
   {
    sup_locs,                          % ChildID
    {sup_locs,start_link,[]},          % Child Start Function
	permanent,                         % Child Restart Policy
	10000,                             % Sub-tree Max Shutdown Time
	supervisor,                        % Child Type
	[sup_locs]                         % Child Modules (For Release Handling Purposes)
   },
   
   % -- locs_init (initializes the locations at startup)
   {
    locs_init,		                   % ChildID
    {locs_init,start_link,[]},         % Child Start Function
	transient,                         % Child Restart Policy       (transient because it must succeed)
	5000,                              % Sub-tree Max Shutdown Time
	worker,                  	       % Child Type
	[locs_init]                        % Child Modules (For Release Handling Purposes)
   }
  ] 
 }
}.

%% --- Exported API --- %%

start_link() ->
 supervisor:start_link(?MODULE,[]).