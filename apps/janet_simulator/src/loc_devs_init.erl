%% This module represents a location devices' initializer in the Janet Simulator application %%

-module(loc_devs_init).

-export([loc_devs_init/2]).     % Process Body
-export([spawn_link/2]).        % Start Function (spawn_link/2 instead of start_link/1 because this is no OTP Behaviour callback module)
 
-include("include/sim_mnesia_tables_definitions.hrl").  % Janet Simulator Mnesia Tables Records Definitions

%%====================================================================================================================================
%%                                                         PROCESS BODY                                                        
%%====================================================================================================================================

%% DESCRIPTION:  Initializes all devices' managers in a location at boot
%%
%% ARGUMENTS:    - Loc_id:  The Location ID
%%               - Sup_pid: The PID of the location (and its) 'sup_loc' supervisor 
%%
%% RETURNS:      - ok -> Location device managers succesfully initialized
%%
loc_devs_init(Loc_id,Sup_pid) ->

 % Retrieve the list of dev_ids of devices in the location
 LocDevIdList = db:get_loc_devs(Loc_id),

 % Initialize the location devices' managers
 init_devs_mgrs(LocDevIdList,Loc_id,Sup_pid). 
 
 
%% Initializes the device managers associated with a list of 'dev_id's (loc_devs_init(Loc_id,Sup_pid) helper function)
init_devs_mgrs([],_,_) ->
 ok;
init_devs_mgrs([Dev_id|NextDev_id],Loc_id,Sup_pid) -> 
 
 % Check if the device manager process already exists as a child of the 'sup_loc' supervisor
 % (this is to take into account possible crashes of the 'loc_devs_init' and/or of the 'sup_loc' supervisor)
 case supervisor:get_childspec(Sup_pid,utils:prefix_node_id(device,Dev_id)) of
  {error,not_found} ->
 
   % If the device manager process doesn't exist, spawn it as a child of the 'sup_loc' supervisor
   {ok,_DevMgrPid} = supervisor:start_child(Sup_pid,
                                            {
                                             "dev-" ++ integer_to_list(Dev_id),         % ChildID
                                             {dev_manager,start_link,[Dev_id,Loc_id]},  % Child Start Function
	                                         permanent,                                 % Child Restart Policy
	                                         8000,                                      % Child Sub-tree Max Shutdown Time
	                                         worker,                  	                % Child Type
	                                         [dev_manager]                              % Child Modules (For Release Handling Purposes)
                                            });
											
  {ok,_ChildSpec} ->

    % If the device manager process already exists it means that it was spawned by a
	% previous instance of this process that crashed, and so skip its initialization
    ok
 end, 
 % Initialize the next device manager
 init_devs_mgrs(NextDev_id,Loc_id,Sup_pid).


%%====================================================================================================================================
%%                                                         START FUNCTION                                                        
%%====================================================================================================================================

%% Called by its 'sup_loc' location supervisor when a new location management tree is created
spawn_link(Loc_id,Sup_pid) ->
 {ok,spawn_link(?MODULE,loc_devs_init,[Loc_id,Sup_pid])}.  % The first 'ok' parameter is for attuning to standard interface of an OTP supervision tree
