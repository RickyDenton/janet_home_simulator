%% This module represents a location devices' initializer in the Janet Simulator application %%

-module(loc_devs_init).

-export([loc_devs_init/2]).     % Process Body
-export([spawn_link/2]).        % Start Function (spawn_link/2 instead of start_link/1 because this is no OTP Behaviour callback module)
 
-include("table_records.hrl").  % Mnesia Table Records Definitions

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
 {atomic,LocDevIdList} = db:get_loc_devs(Loc_id),

 % Initialize the location devices' managers
 init_devs_mgrs(LocDevIdList,Sup_pid). 
 
%% Initializes the device managers associated with a list of 'dev_id's (loc_devs_init(Loc_id,Sup_pid) helper function)
init_devs_mgrs([],_) ->
 ok;
init_devs_mgrs([Dev_id|NextDev_id],Sup_pid) -> 
 
 % Check the device manager associated to Dev_id not to be already registered in the 'devmanager' table
 case db:get_record(devmanager,Dev_id) of
   
  % If it is not, spawn it under the location 'sup_loc' supervisor
  {error,not_found} ->
   {ok,_DevMgrPid} = supervisor:start_child(Sup_pid,
                                            {
                                             "dev-" ++ integer_to_list(Dev_id),  % ChildID
                                             {dev_manager,start_link,[Dev_id]},  % Child Start Function
	                                         permanent,                          % Child Restart Policy
	                                         8000,                               % Child Sub-tree Max Shutdown Time
	                                         worker,                  	         % Child Type
	                                         [dev_manager]                       % Child Modules (For Release Handling Purposes)
                                            });
											
  % Otherwise, if it is already registered, it means that it was spawned by a
  % previous instance of this process that crashed, and so skip its initialization
  {ok,_DevMgrRecord} ->
   ok
   
 end,
 
 % Initialize the next device manager
 init_devs_mgrs(NextDev_id,Sup_pid).


%%====================================================================================================================================
%%                                                         START FUNCTION                                                        
%%====================================================================================================================================

%% Called by its 'sup_loc' location supervisor when a new location tree is created
spawn_link(Loc_id,Sup_pid) ->
 {ok,spawn_link(?MODULE,loc_devs_init,[Loc_id,Sup_pid])}.  % The first 'ok' parameter is for attuning to standard interface of an OTP supervision tree