%% This is the callback module of the JANET Simulator (janet_simulator) application %%

-module(jsim).
-behaviour(application). 

-export([run/0,run/2,stop/0,shutdown/0]).       % Application Start and Stop
-export([start/2,stop/1]). 		                % Application Behaviour Callback Functions

-export([halt_node/2,restart_node/2]).          % Controller and Device Nodes Halt and Restart

-include("table_records.hrl").  % Mnesia Table Records Definitions



-export([try_fun/1]).

%%====================================================================================================================================
%%                                                   APPLICATION START AND STOP                                                        
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
%%                                                    PUBLIC UTILITY FUNCTIONS
%%====================================================================================================================================

%% DESCRIPTION:  Halts a running controller or device node, shutting down its manager and so VM
%%
%% ARGUMENTS:    - NodeTypeShorthand: An atom indicating the node type to be shut down, also
%%                                    considering shorthand forms, with the following being allowed:
%%
%%                                    - controller,ctr,contr -> controller node
%%                                    - device, dev          -> device node
%%
%%               - Node_id: The ID of the node to shut down, which must consist in the 'loc_id' for
%%                          controller nodes and in the 'dev_id' for device nodes (>0) 
%%
%% RETURNS:      - ok                          -> Node successfully halted
%%               - {error,not_running}         -> The node is not running (it was stopped before)
%%               - {error,janet_not_running}   -> The Janet Simulator is not running
%%               - {error,location_not_exists} -> The location, and so the controller node, does not
%%                                                exist (NodeType = controller)
%%               - {error,device_not_exists}   -> The device does not exist (NodeType = device)
%%               - {error,unknown_nodetype}    -> Unknown node type
%%               - {error,badarg}              -> Invalid arguments
%%               - {error,{internal,_}}        -> Internal error (should not happen)
%%
halt_node(NodeTypeShortHand,Node_id) when is_atom(NodeTypeShortHand), is_number(Node_id), Node_id>0 ->
 catch(change_node_status(NodeTypeShortHand,Node_id,'halt'));
 
halt_node(_,_) ->
 io:format("usage: halt_node(ctr|dev,loc_id|dev_id)~n"),
 {error,badarg}. 
 
 
%% DESCRIPTION:  Restarts a previously halted controller or device node, recreating its manager and so VM
%%
%% ARGUMENTS:    - NodeTypeShorthand: An atom indicating the node type to be restarted, also
%%                                    considering shorthand forms, with the following being allowed:
%%
%%                                    - controller,ctr,contr -> controller node
%%                                    - device, dev          -> device node
%%
%%               - Node_id: The ID of the node to restart, which must consist in the 'loc_id' for
%%                          controller nodes and in the 'dev_id' for device nodes (>0) 
%%
%% RETURNS:      - ok                              -> Node successfully halted
%%               - {error,already_running}         -> The node is already running
%%               - {error,janet_not_running}       -> The Janet Simulator is not running
%%               - {error,location_not_exists}     -> The location, and so the controller node, does not
%%                                                    exist (NodeType = controller)
%%               - {error,device_not_exists}       -> The device does not exist (NodeType = device)
%%               - {error,unknown_nodetype}        -> Unknown node type
%%               - {error,node_already_restarting} -> The node is already restarting
%%               - {error,badarg}                  -> Invalid arguments
%%               - {error,{internal,_}}            -> Internal error (should not happen)
%% 
restart_node(NodeTypeShortHand,Node_id) when is_atom(NodeTypeShortHand), is_number(Node_id), Node_id>0 ->
 catch(change_node_status(NodeTypeShortHand,Node_id,'restart'));
 
restart_node(_,_) ->
 io:format("usage: restart_node(ctr|dev,loc_id|dev_id)~n"),
 {error,badarg}. 


%%====================================================================================================================================
%%                                                    PRIVATE HELPER FUNCTIONS
%%==================================================================================================================================== 

%% Performs preliminary checks before attempting to change a node's status (halt_node(NodeTypeShortHand,Node_id), restart_node(NodeTypeShortHand,Node_id) helper function)
change_node_status(NodeTypeShortHand,Node_id,Mode) -> 
 
 % Ensure the JANET Simulator to be running
 case utils:is_running(janet_simulator) of
  false ->
  
   % If it is not, return an error
   throw({error,janet_not_running});
  
  true ->
  
   % If it is, determine the node type, also considering shorthand forms
   NodeType = utils:resolve_nodetype_shorthand(NodeTypeShortHand),
   
   % Retrieve the node's manager status and its location ID
   {MgrStatus,Loc_id} = get_manager_data(NodeType,Node_id),
   
   % Ensure that this is not an attempt to halt an already stopped or restart an already running node 
   ok = check_node_status_change(MgrStatus,Mode),
   
   % Retrieve the PID of the manager's 'sup_loc' supervisor
   SupPid = get_suploc_pid(Loc_id),
   
   % Attempt to terminate or restart the 'sup_loc' manager child
   ok = change_manager_status(SupPid,NodeType,Node_id,Mode)
 end.
   

change_manager_status(Sup_pid,NodeType,Node_id,'halt') ->
 
 ChildID = get_suploc_child_id(NodeType,Node_id),

 case supervisor:terminate_child(Sup_pid,ChildID) of
 
  ok ->
   ok;
  
  % The supervisor doesn't recognize its child
  % (consistency error between the 'suploc' and Mgrtable tables)  
  {error,not_found} ->
   throw({error,{internal,child_not_recognized}})
 end;
	
change_manager_status(Sup_pid,NodeType,Node_id,'restart') ->
 
 ChildID = get_suploc_child_id(NodeType,Node_id),
 
 case supervisor:restart_child(Sup_pid,ChildID) of
 
  % Ok
  {ok,_MgrPid} ->
   ok;
 
  % Error
  {error,Error} ->
   throw({error,{internal,Error}})
 end.
	

get_suploc_child_id(NodeType,Node_id) ->
 
 case NodeType of
  controller -> 
   ChildIDType = "ctr-";
  device ->
   ChildIDType = "dev-"
 end,
 
 ChildIDType ++ integer_to_list(Node_id).
   

get_suploc_pid(Loc_id) ->
 case db:get_record(suploc,Loc_id) of
   
  % If the record was not found, it means that there is a consistency
  % error between the 'suploc' and the 'ctrmanager'/'devmanager' tables
  {error,not_found} ->
   throw({error,{internal,suploc_terminated}});
	
  % Otherwise, return the PID of the 'sup_loc' supervisor	
  {ok,SuplocRecord} -> 
   SuplocRecord#suploc.sup_pid
 end.
	


check_node_status_change(MgrStatus,Mode) ->
 if
  MgrStatus =:= "STOPPED" andalso Mode =:= 'halt' ->
   throw({error,not_running});
	
  MgrStatus =/= "STOPPED" andalso Mode =:= 'restart' ->
   throw({error,already_running});
	 
  true ->
   ok
 end.


get_manager_data(controller,Loc_id) ->

 % Retrieve the record associated to the controller's manager in the 'ctrmanager' table
 case db:get_record(ctrmanager,Loc_id) of
	  
  {error,not_found} ->
  
   % If the record was not found, it means that a non-existing location was passed,
   % or there is a consistency error between the database and the 'ctrmanager' table
   case db:get_record(location,Loc_id) of
   
    % If the location associated to the controller doesn't exist
    {error,not_found} ->
	 throw({error,location_not_exists});
	
	% Otherwise, if it does exist, there is a consistency error
    {ok,_LocationRecord} ->
	 throw({error,{internal,ctrmanager_terminated}})
   end;
	
  {ok,CtrMgrRecord} ->
	  
   % Otherwise, return the controller's manager status and the Loc_id
   {CtrMgrRecord#ctrmanager.status,Loc_id}
 end;


get_manager_data(device,Dev_id) ->

 % Retrieve the record associated to the device's manager in the 'devmanager' table
 case db:get_record(devmanager,Dev_id) of
	  
  {error,not_found} ->
  
   % If the record was not found, it means that a non-existing device was passed,
   % or there is a consistency error between the database and the 'devmanager' table
   case db:get_record(device,Dev_id) of
   
    % If the device doesn't exist
    {error,not_found} ->
	 throw({error,device_not_exists});
	
	% Otherwise, if it does exist, there is a consistency error
    {ok,_DeviceRecord} ->
	 throw({error,{internal,devmanager_terminated}})
   end;
	
  {ok,DevMgrRecord} ->
	  
   % Otherwise, return the device's manager status and the Loc_id
   {DevMgrRecord#devmanager.status,DevMgrRecord#devmanager.loc_id}
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