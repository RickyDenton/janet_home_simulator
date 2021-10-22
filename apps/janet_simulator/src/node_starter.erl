%% This module represents a client process spawned by a controller or device manager for attempting to start and link with its node %%

-module(node_starter).

-export([node_starter/10]). % Process Body
-export([spawn_link/8]).    % Process Spawn Function

% Maximum number of node start attempts
% before returning an error to its manager		
-define(Node_start_max_attempts,3).         % Default: 3

% Added delay in seconds between a failed node start
% attempt with error Reason =/= 'timeout' and the next
-define(Nodes_start_attempts_delay_sec,3).  % Default: 3

%%====================================================================================================================================
%%                                                         PROCESS BODY                                                        
%%==================================================================================================================================== 

%% DESCRIPTION:  Body function of the omonymous process spawned by a controller or a device manager for  
%%               attempting to start and link with its managed node up to a predefined maximum number of attempts
%%
%% ARGUMENTS:    - NodeHost:     The hostname where to deploy the node       (a list)
%%               - NodeName:     The name of the node to be deployed         (a list)
%%               - NodeArgs:     The VM arguments of the node to be deployed (a list)
%%               - AppInfo:      A map containing information on the JANET application to be started on the node,
%%                               which must be returned to the manager once the node has been successfully started
%%               - Node_id:      The node ID ("Loc_id" for controller and "Dev_id" for device nodes)
%%               - NodeType:     The node type as a list ("controller"|"device"), logging purposes
%%               - MgrPid:       The PID of the node's manager
%%               - MgrType:      The manager type as a list ("ctr_mgr"|"dev_mgr"), logging purposes
%%               - ProgName:     The name of the script that launched this ERTS (a list, should be "erl")
%%               - AttemptsLeft: The remaining number of attempts for starting the node before returning an error to its manager
%%
%% RETURNS:      - (can only exit, sending to its manager parent the result of the operation via a cast())
%%
node_starter(NodeHost,NodeName,NodeArgs,AppInfo,Node_id,NodeType,MgrPid,MgrType,ProgName,AttemptsLeft) ->
  
 % Attempt to start the JANET node and lik it to its manager
 %
 % NOTE: Starting a node and linking it to a process different than the caller relies on
 % exploiting the exported but UNDOCUMENTED 'start/5' function of the 'slave' library
 %
 StartRes = slave:start(NodeHost,NodeName,NodeArgs,MgrPid,ProgName),

 % Depending on the result of the operation and whether there are start attempts left
 case {StartRes,AttemptsLeft} of 
 
  %% ------------------------------------ NODE START SUCCESS ------------------------------------ %%
  {{ok,Node},_} ->
  
   % If the node was successfully started and linked to its manager, notify the manager by sending it:
   %   - The started node() complete name
   %   - The AppInfo map
   %   - The PID of the 'node_starter' process
   %
   gen_server:cast(MgrPid,{node_start_success,Node,AppInfo,self()});
   
  %% -------------- NODE START FAILURE (start error with no further attempts left) -------------- %%
  {{error,Reason},0} ->
  
   % If an error occured in starting the node and there are no attempts
   % left, inform the manager that its node could not be started
   gen_server:cast(MgrPid,{node_start_failure,Reason,self()});
   
  %% ---------------- NODE START ATTEMPT FAILED (start error with attempts left) ---------------- %% 
  {{error,Reason},AttemptsLeft} ->
  
   % If an error occured in starting the node and there are attempts left, report the error and,
   % if its Reason =/= 'timeout', wait for a predefined added delay before the next attempt
   case Reason of
   
    % Node host timeout (retry with no delay)
    timeout ->
	 io:format("[~s-~w]: <WARNING> Timeout in connecting with the ~s node's host, retrying...~n",[MgrType,Node_id,NodeType]);
	 
	% A previous instance of the node (NodeName@NodeHost) is still running (retry with delay)
    {already_running,_CtrNode} ->
	 io:format("[~s-~w]: <ERROR> A previous instance of the ~s node is still running, retrying in ~w seconds...~n",[MgrType,Node_id,NodeType,?Nodes_start_attempts_delay_sec]),
	 timer:sleep(?Nodes_start_attempts_delay_sec * 1000);
	 
	% No 'ssh' program was found in the remote host (retry with delay)
    no_rsh ->
	 io:format("[~s-~w]: <ERROR> No 'ssh' program was found in the ~s node's host, retrying in ~w seconds...~n",[MgrType,Node_id,NodeType,?Nodes_start_attempts_delay_sec]),
	 timer:sleep(?Nodes_start_attempts_delay_sec * 1000)
   end,
   
   % Decrement the number of start attempts left and perform the next one
   node_starter(NodeHost,NodeName,NodeArgs,AppInfo,Node_id,NodeType,MgrPid,MgrType,ProgName,AttemptsLeft-1)
 end.
 

%%====================================================================================================================================
%%                                                         SPAWN FUNCTION                                                        
%%====================================================================================================================================

%% Called by a controller or device manager for attempting to start and link with its node
spawn_link(NodeHost,NodeName,NodeArgs,AppInfo,Node_id,NodeType,MgrPid,MgrType) 

 when is_list(NodeHost), is_list(NodeName), is_list(NodeArgs), is_map(AppInfo),
      is_integer(Node_id), Node_id > 0, is_list(NodeType), is_pid(MgrPid), is_list(MgrType) ->

 % Spawn the 'node_starter' process with the specified arguments and by:
 %
 %  - Retrieving the name of the script that started this ERTS (slave:start/5 compatibility) 
 %  - Initializing the number of start attempts to its predefined maximum
 %
 {ok,proc_lib:spawn_link(?MODULE,node_starter,[NodeHost,NodeName,NodeArgs,AppInfo,Node_id,NodeType,MgrPid,MgrType,progname(),?Node_start_max_attempts])};
 
% Invalid arguments
spawn_link(_NodeHost,_NodeName,_NodeArgs,_AppInfo,_Node_id,_NodeType,_MgrPid,_MgrType) ->
 {error,badarg}. 
 
 
%% Returns the name of the script that started this ERTS (spawn_link() helper function)
%%
%% NOTE: This function is used for the slave:start/5 compatibility, and was adapted from:
%%       https://github.com/erlang/otp/blob/master/lib/stdlib/src/slave.erl (line 310)
%%
progname() ->
 
 % Retrieve the 'progname' argument of the 'init' module
 case init:get_argument(progname) of

  % If it was successfully retrieved, return it
  {ok,[[Progname]]} ->
   Progname;
		
  % Otherwise, return a predefined string
  _NoProgName ->
   "no_prog_name"
 end.