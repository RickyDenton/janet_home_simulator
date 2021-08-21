%% This is the callback module of the JANET Simulator (janet_simulator) application %%

-module(jsim).
-behaviour(application). 

-export([run/0,run/2,stop/0,shutdown/0]).       % Application Start and Stop
-export([start/2,stop/1]). 		                % Application Behaviour Callback Functions

-export([halt_node/2,restart_node/2]).          % Controller and Device Nodes Halt and Restart

-include("table_records.hrl").  % Mnesia Table Records Definitions


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
 change_node_status(NodeTypeShortHand,Node_id,'halt');
 
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
 change_node_status(NodeTypeShortHand,Node_id,'restart');
 
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
   {error,janet_not_running};
  
  true ->
  
   % If it is, determine the node type, also considering shorthand forms
   NodeType = utils:resolve_nodetype_shorthand(NodeTypeShortHand),
   case NodeType of
    unknown ->
  
     % If unknown node type, return an error
     {error,unknown_nodetype};
  
    controller ->
  
     % If it's a controller, ensure that its location exists
     case db:get_record(location,Node_id) of
   
      % If it doesn't, return an error
      {error,not_found} ->
	   {error,location_not_exists};
	
	  % If it does, attempt to change its status as requested
      {ok,_LocationRecord} ->
	   change_node_status(ctrmanager,Node_id,Node_id,"ctr-" ++ integer_to_list(Node_id),Mode)
     end;
   
    device ->
  
     % If it's a device, ensure that it exists
     case db:get_record(device,Node_id) of
   
      % If it doesn't, return an error
      {error,not_found} ->
	   {error,device_not_exists};

      % If it does, retrieve its loc_id and attempt to change its status as requested
      {ok,DeviceRecord} ->
	   {Loc_id,_} = DeviceRecord#device.sub_id,
	   change_node_status(devmanager,Node_id,Loc_id,"dev-" ++ integer_to_list(Node_id),Mode)
     end
   end
 end.


%% Attempts to change a node's status (halt_node(NodeTypeShortHand,Node_id),restart_node(NodeTypeShortHand,Node_id) -> change_node_status(NodeTypeShortHand,Node_id,Mode) helper function)
change_node_status(Mgrtable,Node_id,Loc_id,ChildRef,Mode) ->

 % Check if the node manager is running or not by querying
 % the appropriate table ('ctrmanager' or devmanager')
 NodeStatus = db:get_record(Mgrtable,Node_id),
 
 case {NodeStatus,Mode} of
 
  % If attempting to halt a node that is not running, return an error
  {{error,not_found},'halt'} ->
   {error,not_running};
  
  % If attempting to restart a node that is already running, return an error
  {{ok,_MgrtableRecord},'restart'} ->
   {error,already_running};
   
  _ ->
  
   % Attempt to retrieve the ID of the node manager's 'sup_loc' supervisor from the 'suploc' table
   case db:get_record(suploc,Loc_id) of
   
    % If the node's location supervisor was not found, there is a consistency
	% error between the 'suploc' and Mgrtable tables, and so return an error
    {error,not_found} ->
	 {error,{internal,suploc_terminated}};
	
    % If the node's location supervisor was found	
	{ok,SuplocRecord} -> 
	  
	 % Retrieve the supervisor's PID
	 Sup_pid = SuplocRecord#suploc.sup_pid,
	 
	 % Ask the location supervisor to halt or restart the node's manager depending on Mode
	 case Mode of
	  'halt' ->
	    SupRes = supervisor:terminate_child(Sup_pid,ChildRef);
	   
	  'restart' ->
	    SupRes = supervisor:restart_child(Sup_pid,ChildRef)
	 end,
	 
	 % Process the result of the operation
	 case SupRes of
	 
      %% -------------------------- Halt Success -------------------------- %%
	  ok ->
	  
	   % Delete the node manager record from the Mgrtable for consistency
	   {atomic,ok} = mnesia:transaction(fun() -> mnesia:delete({Mgrtable,Node_id}) end),
	   ok;
	  
	  %% ------------------------ Restart Success ------------------------ %%
      {ok,_MgrPid} ->
	   ok;	 
	   
	  %% ------------------------- Shared Errors ------------------------- %%
	  % The supervisor doesn't recognize its child
	  % (consistency error between the 'suploc' and Mgrtable tables)
	  {error,not_found} ->
	   {error,{internal,child_not_recognized}};
	  
	  %% ---------------------- Restart-olny Errors ---------------------- %%
	  % The supervisor reports that the manager is already running
	  % (consistency error between the 'suploc' and Mgrtable tables)
	  {error,running} ->
	   {error,{internal,child_already_running}};
	   
	  % The supervisor reports that it was already restarting the manager 
	  {error,restarting} ->
	   {error,node_already_restarting};
	   
	  % Other internal error
	  {error,Error} ->
	   {error,{internal,Error}}
	 end
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