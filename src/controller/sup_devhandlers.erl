%% This module represents the supervisor of registered device handlers (ctr_devhandler) in the Janet Controller application %%

-module(sup_devhandlers).
-behaviour(supervisor).

-export([init/1]).        % Supervisor Behaviour Callback Function
-export([start_link/0]).  % Start Function

%%====================================================================================================================================
%%                                                SUPERVISOR INIT CALLBACK FUNCTION                                                        
%%====================================================================================================================================
init(_) ->

 % Reset the contents of the 'devhandler' table for enforcing consistency in case this supervisor is restarted
 ets:delete_all_objects(devhandler),
 
 % Return the supervisor flags and the list of children specifications
 {ok,
  {
   %% ==================================================== SUPERVISOR FLAGS ==================================================== %%
   {
    simple_one_for_one,   % RestartStrategy (simple_one_for_one for optimization purposes since all its children are of the same type)
	2,                    % MaxRestarts
	30                    % TimePeriod for MaxRestarts
   },
   
   %% =========================================== SUPERVISOR CHILDREN SPECIFICATIONS =========================================== %%
   [
    %% --------------------- A device's handler (ctr_devhandler) --------------------- %%
    {
     ctr_devhandler,                     % ChildID
     {ctr_devhandler,start_link,[]},     % Child Start Function
 	 temporary,                          % Child Restart Policy
	 4800,                               % Child Sub-tree Max Shutdown Time
	 worker,                             % Child Type
	 [ctr_devhandler]                    % Child Modules (For Release Handling Purposes)
    }
   ]
  }
 }.

%%====================================================================================================================================
%%                                                         START FUNCTION                                                        
%%====================================================================================================================================

%% Called by the Janet Controller top-level supervisor (sup_jctr) at boot time
start_link() ->
 supervisor:start_link({local,?MODULE},?MODULE,[]).  % The spawned process is also registered locally under the 'sup_devhandlers' name