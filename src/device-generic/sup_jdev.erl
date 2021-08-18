-module(sup_jdev).
-export([init/1,start_link/0]).
-behaviour(supervisor).

%% --- Supervisor Callback Functions --- %%

%% Initializes the device's main components (called at the application start)
init(_) ->

 % Determine the device-specific FSM to be created according to the device's type
 {ok,Type} = application:get_env(type),
 DevFSM = {
           dev_fsm,		         % ChildID
           {Type,start_link,[]}, % Child Start Function
 	       permanent,            % Child Restart Policy 
	       1000,                 % Sub-tree Max Shutdown Time
	       worker,               % Child Type
	       [Type]                % Child Modules (For Release Handling Purposes)
          },
 
 % Return the supervisor list of child specifications
 {ok,
  {{rest_for_one,5,60},     % {RestartStrategy, MaxRestarts, Time Period} (rest_for_one for reinitializing everything should sup_location fail)
   [
    DevFSM,    % -- dev_fsm (the device's FSM)
   
    % -- dev_server (the device's server process)
    {
     dev_server,		             % ChildID
     {dev_server,start_link,[]},     % Child Start Function
 	 permanent,                      % Child Restart Policy
	 1000,                           % Sub-tree Max Shutdown Time
	 worker,                  	     % Child Type
	 [dev_server]                    % Child Modules (For Release Handling Purposes)
    }
   ] 
  }
 }.

%% --- Exported API --- %%

start_link() ->
 supervisor:start_link(?MODULE,[]).