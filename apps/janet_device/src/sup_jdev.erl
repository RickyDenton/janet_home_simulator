%% This module represents the top-level supervisor of the Janet Device application %%

-module(sup_jdev).
-behaviour(supervisor).

-export([init/1]).        % Supervisor Behaviour Callback Function
-export([start_link/0]).  % Start Function

%%====================================================================================================================================
%%                                                SUPERVISOR INIT CALLBACK FUNCTION                                                        
%%====================================================================================================================================
init(_) ->

 % Retrieve the 'type' and 'config' environment variables
 {ok,DevType} = application:get_env(type),
 {ok,Config} = application:get_env(config),

 % Ensure the device configuration to be valid according to its type
 ok = utils:is_valid_devconfig(Config,DevType),

 % Return the supervisor flags and the list of children specifications
 {ok,
  {
   %% ==================================================== SUPERVISOR FLAGS ==================================================== %%
   {
	one_for_all,  % RestartStrategy
	0,            % MaxRestarts
	1             % TimePeriod for MaxRestarts
   },
   
   %% =========================================== SUPERVISOR CHILDREN SPECIFICATIONS =========================================== %%
   [
    %% ----------------------- The device's state machine (dev_statem) ----------------------- %%
    {
     dev_statem,		                        % ChildID
     {dev_statem,start_link,[Config,DevType]},  % Child Start Function
 	 permanent,                                 % Child Restart Policy 
	 2000,                                      % Child Sub-tree Max Shutdown Time
	 worker,                                    % Child Type
	 [dev_statem]                               % Child Modules (For Release Handling Purposes)
    },
   
    %% ------------------- The device's communication server (dev_server) ------------------- %%
    {
     dev_server,		             % ChildID
     {dev_server,start_link,[]},     % Child Start Function
 	 permanent,                      % Child Restart Policy
	 4500,                           % Child Sub-tree Max Shutdown Time
	 worker,                  	     % Child Type
	 [dev_server]                    % Child Modules (For Release Handling Purposes)
    }
   ] 
  }
 }.


%%====================================================================================================================================
%%                                                         START FUNCTION                                                        
%%====================================================================================================================================

%% Called by the application_master at boot time
start_link() ->
 supervisor:start_link(?MODULE,[]).