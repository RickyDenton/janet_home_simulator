-module(loc_init).
-export([spawn_link/2,loc_init/2]). 
-include("table_records.hrl").       % Mnesia table records definition


%% ======================================================= MODULE FUNCTIONS ======================================================= %%

%% DESCRIPTION:  Initializes the location controller and device managers at startup
%%
%% ARGUMENTS:    - Loc_id:  The Location id
%%               - Sup_pid: The PID of its sup_loc supervisor 
%%
%% RETURNS:      - ok: location controller and device managers successfully initialized
%%
%% THROWS:       none 
loc_init(Loc_id,Sup_pid) ->
 init_ctr_mgr(Loc_id,Sup_pid),
 init_dev_mgrs(Loc_id,Sup_pid).
 
%% Initializes the location controller's manager (loc_init() helper function) 
init_ctr_mgr(Loc_id,Sup_pid) ->
 
 % Retrieve the location record and use it to create the location controller's manager under the father sup_loc supervisor
 {atomic,[LocationRecord]} = mnesia:transaction(fun() -> mnesia:read({location,Loc_id}) end),
 {ok,_Ctr_mgr_pid} = supervisor:start_child(Sup_pid,
                                            {
                                             "ctr-" ++ integer_to_list(Loc_id),		    % ChildID
                                             {ctr_manager,start_link,[LocationRecord]}, % Child Start Function
	                                         temporary,                                 % Child Restart Policy
	                                         4500,                                      % Sub-tree Max Shutdown Time
	                                         worker,                  	                % Child Type
	                                         [ctr_manager]                              % Child Modules (For Release Handling Purposes)
                                            }).

%% Initializes the location devices' managers (loc_init(Loc_id,Sup_pid) helper function)  
init_dev_mgrs(Loc_id,Sup_pid) ->
 
 % Retrieve the list of device records and use them to create the associated device managers under the father sup_loc supervisor
 {atomic,LocDevList} = mnesia:transaction(fun() -> mnesia:match_object(#device{sub_id = {Loc_id,'_'}, _ = '_'}) end), 
 init_dev_mgrs_list(LocDevList,Sup_pid).

%% Initializes the manager of each device in a list (loc_init(Loc_id,Sup_pid)->init_dev_mgrs(Loc_id,Sup_pid) helper function) 
init_dev_mgrs_list([],_) ->
 ok;
init_dev_mgrs_list([Dev|NextDev],Sup_pid) ->
 {ok,_Dev_mgr_pid} = supervisor:start_child(Sup_pid,
                                            {
                                             "dev-" ++ integer_to_list(Dev#device.dev_id), % ChildID
                                             {dev_manager,start_link,[Dev]},               % Child Start Function
	                                         temporary,                                    % Child Restart Policy
	                                         4500,                                         % Sub-tree Max Shutdown Time
	                                         worker,                  	                   % Child Type
	                                         [dev_manager]                                 % Child Modules (For Release Handling Purposes)
                                            }),
 init_dev_mgrs_list(NextDev,Sup_pid).


%% ======================================================== START FUNCTION ======================================================== %%

%% Called by the "sup_loc" supervisor following its initialization
spawn_link(Loc_id,Sup_pid) ->
 {ok,spawn_link(?MODULE,loc_init,[Loc_id,Sup_pid])}.