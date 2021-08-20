-module(sup_devs).
-export([init/1,start_link/0]).
-behaviour(supervisor).

%% --- Supervisor Callback Functions --- %%

%% Sets its children' general specifications (called by sup_jctr at initialization)
init(_) ->

%% [TODO_NOW]: Reset the contents of the 'devserver' table to ensure consistency in case the supervisor is restarted

{ok,
 {{simple_one_for_one,5,60},     % {RestartStrategy, MaxRestarts, Time Period}
  [
   % -- Children General Specification -- %
   {
    dev_server,                        % ChildID
    {dev_server,start_link,[]},        % Child Start Function
	temporary,                         % Child Restart Policy
	1000,                              % Sub-tree Max Shutdown Time
	worker,                            % Child Type
	[dev_server]                       % Child Modules (For Release Handling Purposes)
   }
  ]
 }
}.

%% --- Exported API --- %%

start_link() ->
 supervisor:start_link({local,?MODULE},?MODULE,[]).