%% This module represents the locations' tree top supervisor in the Janet Simulator application %%

-module(sup_locs).
-behaviour(supervisor).

-export([init/1]).        % Supervisor Behaviour Callback Function
-export([start_link/0]).  % Start Function

%%====================================================================================================================================
%%                                                SUPERVISOR INIT CALLBACK FUNCTION                                                        
%%====================================================================================================================================
init(_) ->

 % Clear the Mnesia ram_copies tables for enforcing consistency in case this supervisor is restarted
 [{atomic,ok},{atomic,ok},{atomic,ok}] = [mnesia:clear_table(suploc),mnesia:clear_table(ctrmanager),mnesia:clear_table(devmanager)],
 
 % Return the supervisor flags and the list of children specifications
 {ok,
  {
   %% ==================================================== SUPERVISOR FLAGS ==================================================== %% 
   {
    simple_one_for_one,  % RestartStrategy (simple_one_for_one for optimization purposes since all its children are of the same type)
	2,                   % MaxRestarts
	30                   % TimePeriod for MaxRestarts
   },
   
   %% =========================================== SUPERVISOR CHILDREN SPECIFICATIONS =========================================== %%
   [
    %% ---------------------- A location's supervisor (sup_loc) ---------------------- %%
    {
     sup_loc,                    % ChildID
     {sup_loc,start_link,[]},    % Child Start Function
 	 permanent,                  % Child Restart Policy 
	 14500,                      % Child Sub-tree Max Shutdown Time
	 supervisor,                 % Child Type
	 [sup_loc]                   % Child Modules (For Release Handling Purposes)
    }
   ]
  }
 }.

%%====================================================================================================================================
%%                                                         START FUNCTION                                                        
%%====================================================================================================================================

%% Called by the Janet Simulator top-level supervisor (sup_jsim) at boot time
start_link() ->
 supervisor:start_link({local,?MODULE},?MODULE,[]).  % The spawned process is also registered locally under the 'sup_locs' name