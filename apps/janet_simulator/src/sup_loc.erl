%% This module represents a location's supervisor in the Janet Simulator application %%

-module(sup_loc).
-behaviour(supervisor).

-export([init/1]).              % Supervisor Behaviour Callback Function
-export([start_link/1]).        % Start Function

-include("sim_mnesia_tables_definitions.hrl").  % Janet Simulator Mnesia Tables Records Definitions

%%====================================================================================================================================
%%                                                SUPERVISOR INIT CALLBACK FUNCTION                                                        
%%====================================================================================================================================

%% DESCRIPTION:  Initializes the location supervisor, registering its PID into the suploc table
%%
%% ARGUMENTS:    - [Loc_id]: The loc_id of the location associated to this location supervisor
%%
%% RETURNS:      - The "SupFlags" and "ChildSpec" parameters as of supervisor behaviour specification
%%                 (in particular, the loc_init process is created)
%%
%% THROWS:       none 
init(Loc_id) ->
  
 % Register the PID of the location supervisor in the 'suploc' table using the location ID
 {atomic,ok} = mnesia:transaction(fun() -> mnesia:write(#suploc{loc_id=Loc_id,sup_pid=self()}) end),

 % Return the supervisor flags and the list of children specifications
 {ok,
  {
   %% ==================================================== SUPERVISOR FLAGS ==================================================== %%
   {
    one_for_one,  % RestartStrategy
	2,            % MaxRestarts
	30            % TimePeriod for MaxRestarts
   },
   
   %% =========================================== SUPERVISOR CHILDREN SPECIFICATIONS =========================================== %%
   [
    %% --------------- The location controller's manager (ctr_manager) --------------- %%
    {
     "ctr-" ++ integer_to_list(Loc_id),             % ChildID
     {ctr_manager,start_link,[Loc_id]},             % Child Start Function
 	 transient,                                     % Child Restart Policy (transient to account for node host connection failures)
	 14000,                                         % Child Sub-tree Max Shutdown Time
	 worker,                                        % Child Type
	 [loc_devs_init]                                % Child Modules (For Release Handling Purposes)
    },
   
    %% -------------- The location devices' initializer (loc_devs_init) -------------- %%
    {
     "loc_devs_init",                               % ChildID
     {loc_devs_init,spawn_link,[Loc_id,self()]},    % Child Start Function
 	 transient,                          		    % Child Restart Policy (transient for it must complete with exit reason 'normal')  
	 200,                                           % Child Sub-tree Max Shutdown Time
	 worker,                                        % Child Type
	 [loc_devs_init]                                % Child Modules (For Release Handling Purposes)
    }
   ]
  }
 }.
  
%%====================================================================================================================================
%%                                                         START FUNCTION                                                        
%%====================================================================================================================================

%% Called by the locations' tree top supervisor 'sup_locs' whenever a new location tree is created, which may happen:
%%  - At boot time by the locations' tree boot initializer     (locs_init:spawn_sup_loc([Loc_id|NextLoc_Id]))
%%  - At run time when a new location is added to the database (db:add_location(Loc_id,Name,User,Port))
start_link(Loc_id) ->
 supervisor:start_link(?MODULE,Loc_id).
