%% This is the callback module of the JANET Simulator (janet_simulator) application %%

-module(jsim).
-behaviour(application). 

%% ---------------------------------- JANET SIMULATOR RUN AND STOP ---------------------------------- %%
-export([run/0,run/2,stop/0,shutdown/0]).

%% ---------------------------------- JANET NODES STOP AND RESTART ---------------------------------- %%
-export([stop_node/2,restart_node/2]).               % Per-node stop/restart
-export([stop_subloc/1,restart_subloc/1]).           % Per-sublocation stop/restart

%% ---------------------------- APPLICATION BEHAVIOUR CALLBACK FUNCTIONS ---------------------------- %%
-export([start/2,stop/1]). 		    

-include("table_records.hrl").  % Mnesia Table Records Definitions



-export([try_fun/1]).



%%====================================================================================================================================
%%                                                  JANET SIMULATOR RUN AND STOP                                                       
%%====================================================================================================================================

%% DESCRIPTION:  Prepares the configuration parameters and starts the JANET Simulator application
%%
%% ARGUMENTS:    - RestPort:   The port that will be used by the JANET Simulator for binding its REST server on the host OS
%%                             (must be >=30000 for preventing port allocation conflicts)
%%               - RemoteHost: The IP address of the host where JANET controllers will forward state updates
%%               - ():         A default RestPort and RemoteHost are used (testing purposes olny) 
%%
%% RETURNS:      - ok                      -> JANET Simulator succesfully started
%%               - {error,already_running} -> The janet_simulator application is already running on the node
%%               - {error,Reason}          -> Internal error in starting the application
%%               - {error,badarg}          -> Invalid arguments
%%
run(RestPort,RemoteHost) when is_number(RestPort), RestPort>=30000 ->

 % Check if the JANET Simulator is already running
 case utils:is_running(janet_simulator) of
  true ->
  
   % If it is, return an error
   {error,already_running};
   
  false ->
  
   % Otherwise, initialize the JANET Simulator configuration parameters as for the arguments
   application:set_env(janet_simulator,rest_port,RestPort),
   application:set_env(janet_simulator,remotehost,RemoteHost),
   
   % Ensure the Mnesia database to be running
   case db:start_mnesia() of
 
    ok ->
     % If Mnesia is running, start the JANET Simulator and its tables
     %% [TODO]: logger:set_primary_config(#{level => warning}),  (hides the == APPLICATION INFO === messages when supervisors stop components, uncomment before release)	
     application:start(janet_simulator);
	 
	{error,_} ->
     % Otherwise notify that the JANET Simulator cannot be started
     io:format("Mnesia is required for running the JANET Simulator~n")
	 
   end
 end;

%% Invalid function invocations (print help messages)
run(RestPort,_) when is_number(RestPort) ->
 io:format("Please use a port of value >= 30000 for the Simulator rest server for preventing port allocation conflicts on the host OS~n"),
 {error,badarg};
run(_,_) ->
 io:format("usage: run(RestPort,RemoteHost) (Port >= 30000)~n"),
 {error,badarg}.
 
%% Start the JANET Simulator with the default configuration (testing purposes olny) 
run() ->
 run(55555,"somehost.com:1240").


%% DESCRIPTION:  Stops the JANET Simulator
%%
%% ARGUMENTS:    none 
%%
%% RETURNS:      - ok                  -> JANET Simulator succesfully stopped
%%               - {error,not_running} -> The JANET Simulator is not running on the node
%%               - {error,Reason}      -> Internal error in stopping the application
%%
stop() ->

 % Check if the JANET Simulator is running
 case utils:is_running(janet_simulator) of
  false ->
  
   % If it is not, return an error
   {error,not_running};
  
  true ->
  
   % Otherwise, attempt to stop the JANET Simulator
   StopSimStatus = application:stop(janet_simulator),
   case StopSimStatus of
    ok ->
	 
	 % If stopped, clear all Mnesia ram_copies tables and report the operation
     [{atomic,ok},{atomic,ok},{atomic,ok}] = 
	  [mnesia:clear_table(suploc),mnesia:clear_table(ctrmanager),mnesia:clear_table(devmanager)],
	 timer:sleep(5),                            %% [TODO]: This sleep is for output ordering purposes (it will not be necessary once the primary logger level will be set to "warning")
     io:format("Janet Simulator stopped~n");
	 
	{error,Reason} ->
	 
	 % Otherwise, notify the error
     io:format("Error in stopping the Janet Simulator (reason = ~w)~n",[Reason])
   end,
   StopSimStatus
 end.


%% DESCRIPTION:  Stops the Janet Simulator and Mnesia application, as well as the erlang node
%%
%% ARGUMENTS:    none 
%%
%% RETURNS:      - ok -> JANET Simulator node succesfully stopped
%% 
shutdown() ->
 
 % Attempt to stop both the Janet Simulator and Mnesia applications
 stop(),
 
 % Shut down the node
 init:stop("shutdown").


%%====================================================================================================================================
%%                                                  JANET NODES STOP AND RESTART
%%====================================================================================================================================

%% ==================================================== PER-NODE STOP/RESTART ==================================================== %%

%% DESCRIPTION:  Stops a running controller or device node, shutting down its manager and so VM
%%
%% ARGUMENTS:    - NodeTypeShorthand: An atom indicating the type of node to be stopped, also taking
%%                                    into account shorthand forms, with the following being allowed:
%%                                     - controller,ctr,contr -> controller node
%%                                     - device, dev          -> device node
%%
%%               - Node_id: The ID of the node to stop, which must consist in the 'loc_id'
%%                          for controller nodes and in the 'dev_id' for device nodes (>0) 
%%
%% RETURNS:      - ok                          -> Node successfully stopped
%%               - {error,not_running}         -> The node is already stopped
%%               - {error,janet_not_running}   -> The Janet Simulator is not running
%%               - {error,location_not_exists} -> The location associated with the specified
%%                                                controller node does not exist
%%               - {error,device_not_exists}   -> The specified device does not exist
%%               - {error,unknown_nodetype}    -> Unknown node type
%%               - {error,badarg}              -> Invalid arguments
%%               - {error,{internal,_}}        -> Internal error (should not happen)
%%
stop_node(NodeTypeShortHand,Node_id) when is_atom(NodeTypeShortHand), is_number(Node_id), Node_id>0 ->
 catch(change_node_status(NodeTypeShortHand,Node_id,stop));
 
stop_node(_,_) ->
 io:format("usage: stop_node(ctr|dev,loc_id|dev_id)~n"),
 {error,badarg}. 
 
 
%% DESCRIPTION:  Restarts a stopped controller or device node, reinstantiating its manager and so VM
%%
%% ARGUMENTS:    - NodeTypeShorthand: An atom indicating the type of node to be restarted, also taking
%%                                    into account shorthand forms, with the following being allowed:
%%                                     - controller,ctr,contr -> controller node
%%                                     - device, dev          -> device node
%%
%%               - Node_id: The ID of the node to restart, which must consist in the 'loc_id'
%%                          for controller nodes and in the 'dev_id' for device nodes (>0) 
%%
%% RETURNS:      - ok                              -> Node successfully restarted
%%               - {error,already_running}         -> The node is already running
%%               - {error,janet_not_running}       -> The Janet Simulator is not running
%%               - {error,location_not_exists}     -> The location associated with the specified
%%                                                    controller node does not exist
%%               - {error,device_not_exists}       -> The specified device does not exist
%%               - {error,unknown_nodetype}        -> Unknown node type
%%               - {error,node_already_restarting} -> The node is already restarting
%%               - {error,badarg}                  -> Invalid arguments
%%               - {error,{internal,_}}            -> Internal error (should not happen)
%% 
restart_node(NodeTypeShortHand,Node_id) when is_atom(NodeTypeShortHand), is_number(Node_id), Node_id>0 ->
 catch(change_node_status(NodeTypeShortHand,Node_id,restart));
 
restart_node(_,_) ->
 io:format("usage: restart_node(ctr|dev,loc_id|dev_id)~n"),
 {error,badarg}. 


%% ================================================ PER-SUBLOCATION STOP/RESTART ================================================ %%

%% DESCRIPTION:  Stops all device nodes in a sublocation, shutting down their managers and so VMs
%%
%% ARGUMENTS:    - {Loc_id,Subloc_id}: The sub_id of the sublocation whose devices are to be stopped
%%
%% RETURNS:      - ok                             -> Information on the devices that were stopped, were
%%                                                   already stopped and failed to stop is reported.
%%                                                   This also includes the cases where:
%%                                                    - The sublocation is empty
%%                                                    - All sublocation devices were already stopped
%%               - {error,janet_not_running}      -> The Janet Simulator is not running
%%               - {error,sublocation_not_exists} -> The specified sublocation does not exist
%%               - {error,badarg}                 -> Invalid arguments
%%               - {error,{internal,_}}           -> Internal error (should not happen)
%%
stop_subloc({Loc_id,Subloc_id}) when is_number(Loc_id), Loc_id>0, is_number(Subloc_id), Subloc_id>=0 ->

 % The operation is enclosed in a try expression for catching throws not associated with functional errors
 try change_subloc_status({Loc_id,Subloc_id},stop)
 catch
 
  % If the sublocation is empty
  subloc_empty ->
   io:format("The sublocation is empty~n");
   
  % If all devices in the sublocation are already stopped
  all_devs_stopped ->
   io:format("All devices in the sublocation are already stopped~n");
   
  % All other throws consist in functional errors {error,Error}, and should be returned as they are
  Error ->
   Error
 end;   
   
stop_subloc(_) ->
 io:format("usage: stop_subloc({loc_id,subloc_id})~n"),
 {error,badarg}. 


%% DESCRIPTION:  Restarts all device nodes in a sublocation, reinstantiating their managers and so VMs
%%
%% ARGUMENTS:    - {Loc_id,Subloc_id}: The sub_id of the sublocation whose devices are to be restarted
%%
%% RETURNS:      - ok                             -> Information on the devices that were restarted, were
%%                                                   already running and failed to restart is reported.
%%                                                   This also includes the cases where:
%%                                                    - The sublocation is empty
%%                                                    - All sublocation devices were already running
%%               - {error,janet_not_running}      -> The Janet Simulator is not running
%%               - {error,sublocation_not_exists} -> The specified sublocation does not exist
%%               - {error,badarg}                 -> Invalid arguments
%%               - {error,{internal,_}}           -> Internal error (should not happen)
%%
restart_subloc({Loc_id,Subloc_id}) when is_number(Loc_id), Loc_id>0, is_number(Subloc_id), Subloc_id>=0 ->

 % The operation is enclosed in a try expression for catching throws not associated with functional errors
 try change_subloc_status({Loc_id,Subloc_id},restart)
 catch
 
  % If the sublocation is empty
  subloc_empty ->
   io:format("The sublocation is empty~n");
  
  % If all devices in the sublocation are already stopped  
  all_devs_running ->
   io:format("All devices in the sublocation are already running~n");
   
  % All other throws consist in functional errors {error,Error}, and should be returned as they are
  Error ->
   Error
 end;   
 
restart_subloc(_) ->
 io:format("usage: restart_subloc({loc_id,subloc_id})~n"),
 {error,badarg}. 


%%====================================================================================================================================
%%                                       PRIVATE JANET NODES STOP AND RESTART UTILITY FUNCTIONS
%%====================================================================================================================================

%% ==================================================== PER-NODE STOP/RESTART ==================================================== %%

%% Changes a node status by halting or restarting its manager process (halt_node(NodeTypeShortHand,Node_id), restart_node(NodeTypeShortHand,Node_id) helper function)
change_node_status(NodeTypeShortHand,Node_id,Mode) -> 
 
 % Ensure the JANET Simulator to be running
 case utils:is_running(janet_simulator) of
  false ->
  
   % If it is not, throw an error
   throw({error,janet_not_running});
  
  true ->
  
   % If it is running, determine the passed node type, taking shorthand forms into account
   NodeType = utils:resolve_nodetype_shorthand(NodeTypeShortHand),
   
   % Retrieve the node's manager process status and its location ID
   {MgrStatus,Loc_id} = db:get_manager_info(NodeType,Node_id),
   
   % Verify that the node status change does not consist in stopping an already stopped or restarting an already running node
   ok = verify_node_status_change(MgrStatus,Mode),
   
   % Retrieve the PID of the node manager's 'sup_loc' supervisor
   Sup_pid = db:get_suploc_pid(Loc_id),
  
   % Attempt to halt or restart the node's manager child of the 'sup_loc' supervisor
   ok = change_manager_status(Sup_pid,NodeType,Node_id,Mode)
 end.
   

%% Verifies that a node status change doesn't consist in stopping an already stopped or restarting an already running node
%% (change_node_status(NodeTypeShortHand,Node_id,Mode) helper function)		
verify_node_status_change(MgrStatus,Mode) ->
 if
 
  % If attempting to stop an already stopped node, throw an error
  MgrStatus =:= "STOPPED" andalso Mode =:= stop ->
   throw({error,not_running});
	
  % If attempting to restart an already running node, throw an error
  MgrStatus =/= "STOPPED" andalso Mode =:= restart ->
   throw({error,already_running});
	 
  % Otherwise the node status change is valid
  true ->
   ok
 end.


%% Attempts to stop or restart a node by terminating or restarting its associated manager via its 'sup_loc' supervisor
%% (change_node_status(NodeTypeShortHand,Node_id,Mode) helper function)
change_manager_status(Sup_pid,NodeType,Node_id,stop) ->
 
 % Retrieve the node's manager prefix, being it used as ChildID under its 'sup_loc' supervisor
 ChildID = utils:prefix_node_id(NodeType,Node_id),

 % Attempt to terminate the node's manager via its 'sup_loc' supervisor
 case supervisor:terminate_child(Sup_pid,ChildID) of
 
  % If the node's manager was succesfully terminated
  ok ->
   ok;
  
  % If the 'sup_loc' supervisor doesn't recognize the manager child, there is a consistency 
  % error between its internal list of children and the 'ctrmanager' or 'devmanager' table
  {error,not_found} ->
   throw({error,{internal,unknown_suploc_child}})
 end;
	
change_manager_status(Sup_pid,NodeType,Node_id,restart) ->
 
 % Retrieve the node's manager prefix, being it used as ChildID under its 'sup_loc' supervisor
 ChildID = utils:prefix_node_id(NodeType,Node_id),
 
 % Attempt to restart the node's manager via its 'sup_loc' supervisor
 case supervisor:restart_child(Sup_pid,ChildID) of
 
  % If the node's manager was succesfully restarted
  {ok,_MgrPid} ->
   ok;
 
  % If the 'sup_loc' supervisor is already attempting to restart
  % the manager child, return the error (probable deadlock condition)
  {error,restarting} ->
   throw({error,{node_already_restarting}});
  
  % If the 'sup_loc' supervisor doesn't recognize the manager child, there is a consistency 
  % error between its internal list of children and the 'ctrmanager' or 'devmanager' table
  {error,not_found} ->
   throw({error,{internal,unknown_suploc_child}});
  
  % Other supervisor internal error
  {error,Error} ->
   throw({error,{internal,Error}})
 end.
	
	
%% ================================================ PER-SUBLOCATION STOP/RESTART ================================================ %%

%% Attempts to change the statuses of all devices in a sublocation
%% (stop_subloc({Loc_id,Subloc_id}),restart_subloc({Loc_id,Subloc_id}) helper function)
change_subloc_status({Loc_id,Subloc_id},Mode) ->

 % Ensure the JANET Simulator to be running
 case utils:is_running(janet_simulator) of
  false ->
  
   % If it is not, throw an error
   throw({error,janet_not_running});
  
  true ->
  
   % Retrieve the list of devices in the sublocation
   DevIdList = get_subloc_devs_throw({Loc_id,Subloc_id}),
   
   % Retrieve the PID of the managers' 'sup_loc' supervisor
   Sup_pid = db:get_suploc_pid(Loc_id),
   
   % Attempt to change the statuses of all devices in the sublocation, obtaining the following lists in return:
   %
   %  - StoppedMgrs:          The list of device managers in the sublocation that were already stopped
   %  - RunningMgrs:          The list of device managers in the sublocation that were already running
   %  - ChangedMgrsSuccesses: The list of device managers in the sublocation that were successfully restarted or stopped as of "Mode"
   %  - AllMgrsFail:          The list of device managers in the sublocation that raised errors in retrieving or changing their status 
   {StoppedMgrs,RunningMgrs,ChangedMgrsSuccesses,AllMgrsFail} = change_suploc_devs_statuses(DevIdList,Sup_pid,Mode),
   
   % Print a summary of the operation
   change_devices_statuses_summary(StoppedMgrs,RunningMgrs,ChangedMgrsSuccesses,AllMgrsFail,Mode)   
 end.
   

%% Attempts to change the statuses of a list of device nodes via their 'sup_loc' supervisor (change_subloc_status({Loc_id,Subloc_id},Mode) helper function)
change_suploc_devs_statuses(DevIdList,Sup_pid,Mode) ->

 % Retrieve the statuses of all device managers associated with the 'dev_id's in the "DevIdList"
 MgrsStatuses = [ {element(1,catch(db:get_manager_info(device,Dev_id))),Dev_id} || Dev_id <- DevIdList ],
	
 % Filter possible errors occured while retrieving the device managers' statuses
 MgrsStatusesFails = [ {MgrStatusFail,Dev_id} || {MgrStatusFail,Dev_id} <- MgrsStatuses, MgrStatusFail =:= 'error' ],
 MgrsStatusesFailsError = [ {{error,{internal,devmanager_missing}},Dev_id} || {_,Dev_id} <- MgrsStatusesFails],
 MgrsStatusesSuccesses = lists:subtract(MgrsStatuses,MgrsStatusesFails),

 % Derive the lists of running and stopped device managers from the "MgrsStatusesSuccesses" list
 StoppedMgrs = [ {MgrStatus,Dev_id} || {MgrStatus,Dev_id} <- MgrsStatusesSuccesses, MgrStatus=="STOPPED" ],
 RunningMgrs = lists:subtract(MgrsStatusesSuccesses,StoppedMgrs),
 
 % Verify that at least one device in the list can be stopped or restarted as of "Mode"
 ok = verify_nodes_statuses_change(StoppedMgrs,RunningMgrs,Mode),
   
 % Attempt to stop the RunningMgrs or restart the StoppedMgrs as of "Mode"
 ChangedMgrs = change_managers_states(Sup_pid,StoppedMgrs,RunningMgrs,Mode),
   
 % Filter possible errors occured while changing the device managers' statuses
 ChangedMgrsFails = [ {ChangeStatus,Dev_id} || {ChangeStatus,Dev_id} <- ChangedMgrs, ChangeStatus =/= 'ok' ],
 ChangedMgrsSuccesses = [ Dev_id || {_,Dev_id}<-lists:subtract(ChangedMgrs,ChangedMgrsFails) ],   
 
 % Derive the list of managers which raised an error in the operation (either in retrieving or changing their status)
 AllMgrsFail = lists:append(MgrsStatusesFailsError,ChangedMgrsFails),
 
 % Return the following lists:
 %
 %  - StoppedMgrs:          The list of device managers in the "DevIdList" that were already stopped
 %  - RunningMgrs:          The list of device managers in the "DevIdList" that were running
 %  - ChangedMgrsSuccesses: The list of device managers in the "DevIdList" that were successfully restarted or stopped as of mode
 %  - AllMgrsFail:          The list of device managers in the "devIdList" that raised errors in retrieving or changing their status  
 {StoppedMgrs,RunningMgrs,ChangedMgrsSuccesses,AllMgrsFail}.  
   
  
%% Prints a summary of the statuses change operation of multiple devices (change_subloc_status({Loc_id,Subloc_id},Mode) helper function)
change_devices_statuses_summary(AlreadyStoppedMgrs,_,SuccessStoppedMgrs,FailedStoppedMgrs,stop) ->
 if
 
  % If one or more devices were successfully stopped
  length(SuccessStoppedMgrs) > 0 ->
   
   % Prefix all devices that were successfully stopped and print them
   SuccessStoppedMgrsStr = [ utils:prefix_node_id(device,Dev_id) || Dev_id <- SuccessStoppedMgrs ],
   io:format("The following devices were successfully stopped: ~p~n",[SuccessStoppedMgrsStr]);
 
  true ->
   ok
 end,
 
 if
  
  % If one or more devices were already stopped
  length(AlreadyStoppedMgrs) > 0 ->
  
   % Prefix all devices that were already stopped and print them
   AlreadyStoppedMgrsStr = [ utils:prefix_node_id(device,Dev_id) || {_,Dev_id} <- AlreadyStoppedMgrs ],
   io:format("The following devices were already stopped: ~p~n",[AlreadyStoppedMgrsStr]);
   
  true ->
   ok
 end,
 
 if
 
  % If one or more devices raised an error while stopping
  length(FailedStoppedMgrs) > 0 ->
  
   % Prefix all devices that raised an error while stopping and print them via the print_failed_mgrs_changes() function
   FailedStoppedMgrsStr = [ {FailReason,utils:prefix_node_id(device,Dev_id)} || {FailReason,Dev_id} <- FailedStoppedMgrs ],
   io:format("The following devices raised an error in stopping:~n"),
   print_failed_mgrs_changes(FailedStoppedMgrsStr);
   
  true ->
   ok
 end;
 
change_devices_statuses_summary(_,AlreadyRunningMgrs,SuccessRestartMgrs,FailedRestartMgrs,restart) ->
 if
 
  % If one or more devices were successfully restarted
  length(SuccessRestartMgrs) > 0 ->
  
   % Prefix all devices that were successfully restarted and print them
   SuccessRestartMgrsStr = [ utils:prefix_node_id(device,Dev_id) || Dev_id <- SuccessRestartMgrs ],
   io:format("The following devices were successfully restarted: ~p~n",[SuccessRestartMgrsStr]);
   
  true ->
   ok
 end,
 
 if
 
  % If one or more devices were already running
  length(AlreadyRunningMgrs) > 0 ->
  
   % Prefix all devices that were already running and print them
   AlreadyRunningMgrsStr = [ utils:prefix_node_id(device,Dev_id) || {_,Dev_id} <- AlreadyRunningMgrs ],
   io:format("The following devices were already running: ~p~n",[AlreadyRunningMgrsStr]);
   
  true ->
   ok
 end,
 
 if
 
  % If one or more devices raised an error while stopping
  length(FailedRestartMgrs) > 0 ->
  
   % Prefix all devices that raised an error while stopping and print them via the print_failed_mgrs_changes() function
   FailedRestartMgrsStr = [ {FailReason,utils:prefix_node_id(device,Dev_id)} || {FailReason,Dev_id} <- FailedRestartMgrs ],
   io:format("The following devices raised an error in restarting:~n"),
   print_failed_mgrs_changes(FailedRestartMgrsStr);
   
  true ->
   ok
 end.
 
 
%% Prints the prefixed 'dev_id' and reason of each of a list of manager status change failures
%% (change_devices_statuses_summary(StoppedMgrs,RunningMgrs,ChangedMgrsSuccesses,AllMgrsFail,Mode) helper function 
print_failed_mgrs_changes([]) ->
 ok; 
print_failed_mgrs_changes([{Reason,Node_id}|NextFailedMgr]) ->
 io:format(" - ~p: ~p~n",[Node_id,Reason]),
 print_failed_mgrs_changes(NextFailedMgr).
 
 
%% Attempts to change the statuses of a set of device managers via their 'sup_loc' supervisor
%% (change_suploc_devs_statuses(DevIdList,Sup_pid,Mode) helper function)
change_managers_states(Sup_pid,_,RunningMgrs,stop) ->
 [ {catch(change_manager_status(Sup_pid,device,Dev_id,stop)),Dev_id} || {_,Dev_id} <- RunningMgrs ];
change_managers_states(Sup_pid,StoppedMgrs,_,restart) ->
 [ {catch(change_manager_status(Sup_pid,device,Dev_id,restart)),Dev_id} || {_,Dev_id} <- StoppedMgrs ].
 

%% Verifies that at least one node in the list can be stopped or restarted depending on "Mode"
verify_nodes_statuses_change(_,RunningMgrs,stop) ->
 if 
 
  % If attempting to stop a list of nodes that are all already stopped
  length(RunningMgrs) =:= 0 ->
   throw(all_devs_stopped);
   
  % Otherwise the nodes' statuses change operation can continue
  true ->
   ok
 end;
 
verify_nodes_statuses_change(StoppedMgrs,_,restart) ->
 if 
 
  % If attempting to restart a list of nodes that are all already running
  length(StoppedMgrs) =:= 0 ->
   throw(all_devs_running);
   
  % Otherwise the nodes' statuses change operation can continue
  true ->
   ok
 end.
 

%% Retrieves the list of devices in a sublocation, raising throws in case the sublocation
%% does not exist or is empty (change_subloc_status({Loc_id,Subloc_id},Mode) helper function)
get_subloc_devs_throw({Loc_id,Subloc_id}) ->
    
 % Retrieve the list of devices in the sublocation
 case db:get_subloc_devs({Loc_id,Subloc_id}) of

    % Empty sublocation
    {atomic,[]} ->
	 throw(subloc_empty);
	 
    % If the sublocation does not exist, return an error
    {aborted,sublocation_not_exists} ->
     throw({error,sublocation_not_exists});
	 
	% If the list of devices was successfully retrieved
    {atomic,DevIdList} ->
     DevIdList
 end.










try_fun(Loc_id) ->

 case db:get_record(suploc,Loc_id) of

  % If the node's location supervisor was not found, there is a consistency 
  % error between the 'suploc' and Mgrtable tables, and so return an error
  {error,not_found} ->
   {error,{internal,suploc_not_started}};
   
  % If the node's location supervisor was found	
  {ok,SuplocRecord} -> 
	  
   % Retrieve the supervisor's PID
   Sup_pid = SuplocRecord#suploc.sup_pid,
   
   % Retrieve the sup_loc child specifications
   ChildSpecs = supervisor:which_children(Sup_pid),
   
   SuplocChildren = decode_suploc_children(ChildSpecs,[],[]),
   
   io:format("~w",[SuplocChildren])
   
 end.

     
decode_suploc_children([],CtrNode,DevNodes) ->
 {CtrNode,DevNodes};	 
decode_suploc_children([ChildSpec|Next_ChildSpec],CtrNode,DevNodes) ->
 SupChildID = element(1,ChildSpec),
 case SupChildID of
 
  "loc_devs_init" ->
   decode_suploc_children(Next_ChildSpec,CtrNode,DevNodes);
   
  _ ->
   SupChildType = string:sub_string(SupChildID,1,3),
   case SupChildType of
    "ctr" ->
     Loc_id = string:sub_string(SupChildID,5),
     decode_suploc_children(Next_ChildSpec,list_to_integer(Loc_id),DevNodes);
  
    "dev" ->
     Dev_id = string:sub_string(SupChildID,5),
     decode_suploc_children(Next_ChildSpec,CtrNode,DevNodes ++ [list_to_integer(Dev_id)])
   end
 end.
 
 
%%====================================================================================================================================
%%                                             APPLICATION BEHAVIOUR CALLBACK FUNCTIONS                                                        
%%====================================================================================================================================

%% Starts the JANET Simulator
start(normal,_Args) ->
 sup_jsim:start_link().
 
%% Called once the JANET Simulator has been stopped
stop(_State) ->
 ok.