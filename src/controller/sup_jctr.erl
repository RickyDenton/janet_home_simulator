%% This module represents the top-level supervisor of the Janet Controller application %%

-module(sup_jctr).
-behaviour(supervisor).

-export([init/1]).        % Supervisor Behaviour Callback Function
-export([start_link/0]).  % Start Function

%%====================================================================================================================================
%%                                                SUPERVISOR INIT CALLBACK FUNCTION                                                        
%%====================================================================================================================================
init(_) ->

 % Initialize the ETS "devalloc" table from the contents of the "devalloc" environment variable
 {ok,DevAlloc} = application:get_env(devalloc),
 ets:new(devalloc,[set,public,named_table]),
 ets:insert(devalloc,DevAlloc),

 % Initialize the ETS "devserver" table
 ets:new(devserver,[set,public,named_table]),
 
 % Return the supervisor flags and the list of children specifications
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
    %% ------------- The Janet Controller's REST server (ctr_restserver) ------------- %%
    {
     ctr_restserver,		            % ChildID
     {ctr_restserver,start_link,[]},    % Child Start Function
 	 permanent,                         % Child Restart Policy 
	 3000,                              % Child Sub-tree Max Shutdown Time
	 worker,                  	        % Child Type
	 [ctr_restserver]                   % Child Modules (For Release Handling Purposes)
    },
  
    %% ------------------ The device servers' supervisor (sup_devs) ------------------ %%
    {
     sup_devs,                          % ChildID
     {sup_devs,start_link,[]},          % Child Start Function
 	 permanent,                         % Child Restart Policy 
	 2000,                              % Child Sub-tree Max Shutdown Time
	 supervisor,                        % Child Type
	 [sup_devs]                         % Child Modules (For Release Handling Purposes)
    },
   
    %% -------------- The devices' registration server (ctr_regserver) -------------- %%
	{
     ctr_regserver,		                % ChildID
     {ctr_regserver,start_link,[]},     % Child Start Function
 	 permanent,                         % Child Restart Policy
	 1000,                              % Child Sub-tree Max Shutdown Time
	 worker,                  	        % Child Type
	 [ctr_regserver]                    % Child Modules (For Release Handling Purposes)
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