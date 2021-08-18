-module(sup_jctr).
-export([init/1,start_link/0]).
-behaviour(supervisor).

%% --- Supervisor Callback Functions --- %%

%% Initializes the controller's main components (called at the application start)
init(_) ->

 % Initialize the ETS DevAlloc table from the "devalloc" environment variable
 ets:new(devalloc,[set,public,named_table]),
 {ok,DevAlloc} = application:get_env(devalloc),
 ets:insert(devalloc,DevAlloc),

 % Initialize the ETS devserver table
 ets:new(devserver,[set,public,named_table]),
 
 % Return the supervisor list of child specifications
 {ok,
  {{rest_for_one,5,60},     % {RestartStrategy, MaxRestarts, Time Period} (rest_for_one for reinitializing everything should sup_location fail)
   [
    % -- ctr_restserver (the controller's REST server)
    {
     ctr_restserver,		            % ChildID
     {ctr_restserver,start_link,[]},    % Child Start Function
 	 permanent,                         % Child Restart Policy 
	 3000,                              % Sub-tree Max Shutdown Time
	 worker,                  	        % Child Type
	 [ctr_restserver]                   % Child Modules (For Release Handling Purposes)
    },
  
    % -- sup_devs (the devices' listeners supervisor)
    {
     sup_devs,                          % ChildID
     {sup_devs,start_link,[]},          % Child Start Function
 	 permanent,                         % Child Restart Policy 
	 2000,                              % Sub-tree Max Shutdown Time
	 supervisor,                        % Child Type
	 [sup_devs]                         % Child Modules (For Release Handling Purposes)
    },
   
    % -- ctr_regserver (the devices' register process)
    {
     ctr_regserver,		                % ChildID
     {ctr_regserver,start_link,[]},     % Child Start Function
 	 permanent,                         % Child Restart Policy
	 1000,                              % Sub-tree Max Shutdown Time
	 worker,                  	        % Child Type
	 [ctr_regserver]                    % Child Modules (For Release Handling Purposes)
    }
   ] 
  }
 }.

%% --- Exported API --- %%

start_link() ->
 supervisor:start_link(?MODULE,[]).