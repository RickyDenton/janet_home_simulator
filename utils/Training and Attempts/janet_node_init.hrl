%%====================================================================================================================================%%
%%                                          JANET NODES INITIALIZATION RECORD AND CONSTANTS                                           %%
%%====================================================================================================================================%%

%% NOTE: This header file, as well as the division in the 'ctr_manager' and 'dev_manager' modules of the managed nodes starting
%%       operations in two separate handle_continue() callbacks, is required to enable the managers to receive messages (in
%%       particular, exit signals from their 'sup_loc' supervisors) while attempting to start their managed nodes, a task that
%%       cannot be delegated to a client 'node_starter' process due to the 'slave' library not offering the possibility of starting
%%       and linking a node to a process different than the caller

%% This represents a JANET node initialization record		
-record(nodeinit,    
        {
		 node_host,     % The hostname where to deploy the node
		 node_name,     % The node name 
		 node_args,     % The node VM arguments
		 app_info,      % A map containing information required for starting the target JANET application on the managed node 
		 mgr_pid,       % The PID of the node's manager
		 attempts_left  % The number of attempts remaining for starting the node and linking it to its manager
		}).	
		
% Maximum number of attempts for starting a JANET
% node before its manager gives up and stops		
-define(Node_start_max_attempts,3).                     % Default: 3

% Added delay in seconds between a failed node start
% attempt with error Reason =/= 'timeout' and the next
-define(Nodes_start_attempts_delay_sec,3).              % Default: 3

