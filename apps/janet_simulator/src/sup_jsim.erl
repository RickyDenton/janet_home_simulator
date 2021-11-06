%% This module represents the top-level supervisor of the Janet Simulator application %%

-module(sup_jsim).
-behaviour(supervisor).

-export([init/1]).        % Supervisor Behaviour Callback Function
-export([start_link/0]).  % Start Function

%%====================================================================================================================================
%%                                                SUPERVISOR INIT CALLBACK FUNCTION                                                        
%%====================================================================================================================================
init(_) ->
 {ok,
  {
   %% ==================================================== SUPERVISOR FLAGS ==================================================== %%
   {
    rest_for_one,  % RestartStrategy (rest_for_one for reinitializing the sup_locs tree via the locs_init module should it fail)
    2,             % MaxRestarts
	30             % TimePeriod for MaxRestarts
   },                 

   %% =========================================== SUPERVISOR CHILDREN SPECIFICATIONS =========================================== %%
   [   
    %% ---------------- The locations' tree top supervisor (sup_locs) ---------------- %%
    {
     sup_locs,                          % ChildID
     {sup_locs,start_link,[]},          % Child Start Function
 	 permanent,                         % Child Restart Policy 
	 15000,                             % Child Sub-tree Max Shutdown Time
	 supervisor,                        % Child Type
	 [sup_locs]                         % Child Modules (For Release Handling Purposes)
    },
   
    %% -------------- The locations' tree boot initializer (locs_init) -------------- %%
	{
     locs_init,		                    % ChildID
     {locs_init,spawn_link,[]},         % Child Start Function
 	 transient,                         % Child Restart Policy (transient for it must complete with exit reason 'normal') 
	 5000,                              % Child Sub-tree Max Shutdown Time
	 worker,                  	        % Child Type
	 [locs_init]                        % Child Modules (For Release Handling Purposes)
    },
	
	%% --------- The JANET Simulator remote hosts monitor (sim_hostsmonitor) --------- %%
    {
     sim_hostsmonitor,		            % ChildID
     {sim_hostsmonitor,start_link,[]},  % Child Start Function
 	 permanent,                         % Child Restart Policy 
	 8000,                              % Child Sub-tree Max Shutdown Time
	 worker,                  	        % Child Type
	 [sim_hostsmonitor]                 % Child Modules (For Release Handling Purposes)
    },
	
	%% ------------- The JANET Simulator REST handler (sim_resthandler) ------------- %%
    {
     sim_resthandler,		            % ChildID
     {sim_resthandler,start_link,[]},   % Child Start Function
 	 transient,                         % Child Restart Policy (transient to account for port allocation conflicts) 
	 5000,                              % Child Sub-tree Max Shutdown Time
	 worker,                  	        % Child Type
	 [sim_resthandler]                  % Child Modules (For Release Handling Purposes)
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