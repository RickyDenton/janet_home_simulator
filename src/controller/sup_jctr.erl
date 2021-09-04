%% This module represents the top-level supervisor of the Janet Controller application %%

-module(sup_jctr).
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
    one_for_one,  % RestartStrategy
	1,            % MaxRestarts
	30            % TimePeriod for MaxRestarts
   },
   
   %% =========================================== SUPERVISOR CHILDREN SPECIFICATIONS =========================================== %%
   [
    %% ----------------- The controller REST server (ctr_restserver) ----------------- %%
    {
     ctr_restserver,		            % ChildID
     {ctr_restserver,start_link,[]},    % Child Start Function
 	 permanent,                         % Child Restart Policy 
	 5000,                              % Child Sub-tree Max Shutdown Time
	 worker,                  	        % Child Type
	 [ctr_restserver]                   % Child Modules (For Release Handling Purposes)
    },
  
    %% -------------- The device handlers supervisor (sup_devhandlers) -------------- %%
    {
     sup_devhandlers,                   % ChildID
     {sup_devhandlers,start_link,[]},   % Child Start Function
 	 permanent,                         % Child Restart Policy 
	 5000,                              % Child Sub-tree Max Shutdown Time
	 supervisor,                        % Child Type
	 [sup_devhandlers]                  % Child Modules (For Release Handling Purposes)
    },
   
    %% ---------------- The devices' pairing server (ctr_pairserver) ---------------- %%
	{
     ctr_pairserver,		            % ChildID
     {ctr_pairserver,start_link,[]},    % Child Start Function
 	 permanent,                         % Child Restart Policy
	 1000,                              % Child Sub-tree Max Shutdown Time
	 worker,                  	        % Child Type
	 [ctr_pairserver]                   % Child Modules (For Release Handling Purposes)
    },
	
	%% -------------- The controller simulation server (ctr_simserver) -------------- %%
	{
     ctr_simserver,		                % ChildID
     {ctr_simserver,start_link,[]},     % Child Start Function
 	 permanent,                         % Child Restart Policy
	 4000,                              % Child Sub-tree Max Shutdown Time
	 worker,                  	        % Child Type
	 [ctr_simserver]                    % Child Modules (For Release Handling Purposes)
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