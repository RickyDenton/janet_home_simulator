%% This module represents the JANET Simulator remote resources monitor %%

-module(sim_resmonitor).
-behaviour(gen_server).

-export([start_link/0,init/1,terminate/2,handle_call/3,handle_cast/2,handle_continue/2,handle_info/2]).  % gen_server Behaviour Callback Functions

%% This record represents the state of a 'sim_resmonitor' gen_server
-record(resmonstate,    
        {
		 node_hosts_states,     % A list of information on the hosts JANET nodes can be deployed in (see the 'nodehoststate' record below)
		 rem_restserver_state,  % The state of the remote REST server                               (see the 'remreststate' record below)
		 resmon_pid             % The PID of the remote resources monitor process
		}).

%% This record represents the state of a host JANET nodes can be deployed in
-record(nodehoststate,    
        {
		 hostname,              % The host name   (a list, same as in the 'allowed_nodes_list' environment variable)
		 status,                % The host status ('connecting','online')
		 node                   % The complete name of the stub node deployed on the host
		}).

%% This record represents the state of the remote REST server
-record(remreststate,    
        {
		 ipaddr,                % The IP address of the remote REST server (a list)
		 port,                  % The port of the remote REST server (int > 0)
		 conn_state,            % The state of the connection with the remote REST server ('connecting'|'online')
		 conn_pid,              % The PID of the Gun connection process maintaining a connection with the remote REST server
		 conn_ref               % The reference used for monitoring the Gun connection process
		}).
				
% gen_server timeout after which the resource monitor
% reports the remote resources that are not online
-define(Res_monitoring_timeout,6 * 1000).     % Default: 6 * 1000 (6 seconds)
		
% Delay between stub nodes spawning attempts
-define(Stubs_spawn_attempts_delay,3 * 1000). % Default: 3 * 1000 (3 seconds)		
		
%%====================================================================================================================================
%%                                                  GEN_SERVER CALLBACK FUNCTIONS                                                        
%%====================================================================================================================================

%% ============================================================ INIT ============================================================ %%
init(_) ->

 % Trap exit signals so to allow cleanup operations when terminating (terminate(Reason,SrvState) callback function)
 process_flag(trap_exit,true),
 
 % Return a stub initial server state, where its proper initialization will continue
 % in the "handle_continue(Continue,State)" callback function for parallelization purposes  
 {ok,none,{continue,init}}.
 
 
%% ======================================================= HANDLE_CONTINUE ======================================================= %%

%% Initializes the remote resources monitor (called right after the 'init' callback function)
handle_continue(init,_SrvState) ->
 
 %% --------------------------------------------------- NODES HOSTS MONITORING --------------------------------------------------- %%
 
 % Retrieve the 'allowed_nodes_host' environment variable
 {ok,AllowedHosts} = application:get_env(janet_simulator,allowed_nodes_hosts),
 
 % Retrieve the local host name environment variable
 {ok,LocalHostName} = application:get_env(janet_simulator,localhost_name),
 
 % Retrieve the PID of the current process
 ResMonPID = self(),
 
 % For each host JANET nodes can be deployed in, excluding if present the local host,
 % spawn a 'sim_stubspawn' process for spawning a stub node on such host  
 [proc_lib:spawn_link(fun() -> sim_stubspawn(AllowedHost,ResMonPID) end) || AllowedHost <- AllowedHosts, AllowedHost =/= LocalHostName],
 
 % Build the list of node host states, excluding if present the local host
 NodeHostsStatesNoLocal = [#nodehoststate{hostname = AllowedHost, status = connecting, node = none} || AllowedHost <- AllowedHosts, AllowedHost =/= LocalHostName],
 
 % Whether the local host belongs to the list of hosts nodes can be deployed in
 NodeHostsStates =
 case lists:member(LocalHostName,AllowedHosts) of
 
  % If it does, create its 'nodehoststate' entry and append it in the list of node hosts states
  true ->
   NodeHostsStatesNoLocal ++ [#nodehoststate{hostname = LocalHostName, status = online, node = node()}];
   
  % Otherwise, do not modify the list of node hosts states
  false ->
   NodeHostsStatesNoLocal
 end,
 
 %% ----------------------------------------------- REMOTE REST SERVER MONITORING ----------------------------------------------- %%
 
 % Retrieve the 'remote_rest_server_addr' and 'remote_rest_server_port' environment variables
 {ok,RemoteRESTServerAddr} = application:get_env(remote_rest_server_addr),
 {ok,RemoteRESTServerPort} = application:get_env(remote_rest_server_port),
 
 % Ensure that all dependencies of the Gun HTTP client have been started
 {ok,_GunDepsStarted} = application:ensure_all_started(gun),
 
 % Spawn the Gun connection process for monitoring the connection with the
 % remote REST server, obtaining its PID and a monitor reference towards it
 {ConnPid,ConnRef} = gun_start_monitor(RemoteRESTServerAddr,RemoteRESTServerPort),
 
 % Return the 'sim_resmonitor' initial state and set the resources monitoring timeout
 {noreply,#resmonstate{node_hosts_states = NodeHostsStates, rem_restserver_state = #remreststate{ipaddr = RemoteRESTServerAddr,
          port = RemoteRESTServerPort, conn_state = connecting, conn_pid = ConnPid, conn_ref = ConnRef}, resmon_pid = ResMonPID},?Res_monitoring_timeout}.
 
 
%% ========================================================= HANDLE_CALL ========================================================= %% 

%% PRINT REMOTE RESOURCES SUMMARY
%% ------------------------------
%% SENDER:    The simulation controller (the user)
%% WHEN:      -
%% PURPOSE:   Obtain a summary of the remote resources states
%% CONTENTS:  -
%% MATCHES:   (always) (when the requests comes from the JANET Simulator node)
%% ACTIONS:   Parse the 'sim_resmonitor' state so to obtain a summary
%%            of the remote resources that are not online
%% ANSWER:    Return the summary of the remote resources that are not online
%% NEW STATE: -
%% TIMEOUT:   Reinitialize the resources monitoring timeout
%%
handle_call(print_rem_res_summary,ReqPid,SrvState) when node(ReqPid) =:= node() ->
 
 % Parse the 'sim_resmonitor' state, obtaining a summary of the remote resources that are not online
 RemResSummary = print_remresmonitor_summary(SrvState),
 
 % Return the summary to the caller and
 % reinitialize the resources monitoring timeout
 {reply,RemResSummary,SrvState,?Res_monitoring_timeout};


%% Unexpected call
handle_call(Request,From,SrvState) ->
 
 % Report that an unexpected call was received by this gen_server
 io:format("[sim_resmonitor]: <WARNING> Unexpected call (Request = ~p, From = ~p, SrvState = ~p)~n",[Request,From,SrvState]),

 % Reply with a stub message, keep the SrvState and
 % reinitialize the resources monitoring timeout
 {reply,unsupported,SrvState,?Res_monitoring_timeout}.

 
%% ========================================================= HANDLE_CAST ========================================================= %% 

%% STUB_NODE_ONLINE
%% ----------------
%% SENDER:    One of the 'sim_stubspawn' processes spawned by the 'sim_resmonitor'
%% WHEN:      When it successfully spawns a stub node on its assigned node host
%% PURPOSE:   Inform the 'sim_resmonitor' that the node host
%%            is online and of the stub node complete name
%% CONTENTS:  1) The name of the host the stub node was spawned in (a list)
%%            2) The complete name of the stub node that was spawned (node())
%%            3) The 'sim_stubspawn' PID ("security purposes")
%% MATCHES:   always
%% ACTIONS:   Create a monitor towards the stub node
%% NEW STATE: Update the node host status to 'online' and set its 'node' variable
%% TIMEOUT:   Reinitialize the resources monitoring timeout
%%
handle_cast({stub_node_online,NodeHost,StubNode,SimStubSpawnPID},SrvState=#resmonstate{node_hosts_states = NodeHostsStates}) when node(SimStubSpawnPID) =:= node() ->

 % Create a monitor towards the stub node
 %
 % NOTE: differently from the monitor BIF this functions does not return a reference:
 %       a {nodedown,Node} message is automatically returned to the calling process
 erlang:monitor_node(StubNode,true),

 % Retrieve the state associated with the host the stub node was spawned in
 {value,NodeHostState,NewNodeHostStates} = lists:keytake(NodeHost,2,NodeHostsStates),

 % Update the node host status to 'online' and set its 'node' variable
 NewNodeHostState = NodeHostState#nodehoststate{status = online, node = StubNode},
 
 % Append the updated node host state in the list of node hosts
 % states and reinitialize the resources monitoring timeout
 {noreply,SrvState#resmonstate{node_hosts_states = NewNodeHostStates ++ [NewNodeHostState]},?Res_monitoring_timeout};


%% Unexpected cast
handle_cast(Request,SrvState) ->
 
 % Report that an unexpected cast was received by this gen_server
 io:format("[sim_resmonitor]: <WARNING> Unexpected cast (Request = ~p, SrvState = ~w)~n",[Request,SrvState]),

 % Keep the SrvState and reinitialize
 % the resources monitoring timeout
 {noreply,SrvState,?Res_monitoring_timeout}. 
 
 
%% ========================================================= HANDLE_INFO ========================================================= %%  

%% RESOURCES MONITORING TIMEOUT
%% ----------------------------
%% SENDER:    The gen_server behaviour engine
%% WHEN:      When the configured 'gen_server' timeout expires
%% PURPOSE:   Inform the 'sim_resmonitor' of the timeout expiration
%% CONTENTS:  -
%% MATCHES:   (always)
%% ACTIONS:   Parse the 'sim_resmonitor' state and report the remote resources that are not online
%% NEW STATE: -
%% TIMEOUT:   Reinitialize the resources monitoring timeout
%%
handle_info(timeout,SrvState) ->
 
 % Parse the 'sim_resmonitor' state, obtaining a summary of the remote resources that are not online
 case print_remresmonitor_summary(SrvState) of
 
  % If the summary is empty, do nothing
  [] ->
   ok;
   
  % Otherwise if it is not empty, print it
  RemResSummary ->
   io:format("[sim_resmonitor]: ~s~n",[RemResSummary])
 end,
 
 % Keep the server state and reinitialize the resources monitoring timeout
 {noreply,SrvState,?Res_monitoring_timeout}; 


%% --------------------------------------------------- NODES HOSTS MONITORING --------------------------------------------------- %%

%% NODE HOST DOWN
%% --------------
%% SENDER:    The Erlang Run-Time System (ERTS)
%% WHEN:      When a monitored stub node terminates
%% PURPOSE:   Inform the 'sim_resmonitor' of the node termination
%% CONTENTS:  The complete name of the stub node that terminated
%% MATCHES:   (always)
%% ACTIONS:   1) Report that connection with the node host was lost
%%            2) Respawn a 'sim_stubspawn' process for
%%               attempting to respawn a stub node on such host 
%% NEW STATE: Update the node host status to 'connecting' and clear its 'node' variable
%% TIMEOUT:   Reinitialize the resources monitoring timeout
%%
handle_info({nodedown,StubNodeDown},SrvState=#resmonstate{node_hosts_states = NodeHostsStates, resmon_pid = ResMonPID}) ->
 
 % Retrieve the state associated with the host whose stub node terminated
 {value,NodeHostState,NewNodeHostStates} = lists:keytake(StubNodeDown,4,NodeHostsStates),

 % Retrieve the host name
 HostName = NodeHostState#nodehoststate.hostname,

 % Update the node host status to 'connecting' and clear its 'node' variable
 NewNodeHostState = NodeHostState#nodehoststate{status = connecting, node = none},
 
 % Log that connection with the node host was lost
 io:format("[sim_resmonitor]: <WARNING> Lost connection with node host \"~s\"~n",[HostName]),
 
 % Respawn a 'sim_stubspawn' process for attempting to respawn a stub node on such host
 proc_lib:spawn_link(fun() -> sim_stubspawn(HostName,ResMonPID) end),
 
 % Append the updated node host state in the list of node hosts
 % states and reinitialize the resources monitoring timeout
 {noreply,SrvState#resmonstate{node_hosts_states = NewNodeHostStates ++ [NewNodeHostState]},?Res_monitoring_timeout}; 
 

%% ----------------------------------------------- REMOTE REST SERVER MONITORING ----------------------------------------------- %%

%% GUN UP
%% ------
%% SENDER:    The Gun connection process associated with the remote resource monitor
%% WHEN:      When it has successfully established a connection with the remote REST server
%% PURPOSE:   Inform the resource monitor that the remote REST server is online
%% CONTENTS:  1) The PID of the Gun connection process
%%            2) The protocol used in the connection (HTTP/1.1, unused variable)
%% MATCHES:   When the remote resource monitor is connecting with the remote REST
%%            server (and the request comes from the JANET Simulator node)
%% ACTIONS:   - 
%% NEW STATE: Update the remote REST server 'conn_state' variable
%% TIMEOUT:   Reinitialize the resources monitoring timeout
%%
handle_info({gun_up,ConnPid,_Protocol},SrvState=#resmonstate{rem_restserver_state = #remreststate{conn_state = connecting, conn_pid = ConnPid}}) ->
 
 % Update the remote REST server 'conn_state' variable
 % and reinitialize the resources monitoring timeout
 {noreply,SrvState#resmonstate{rem_restserver_state = SrvState#resmonstate.rem_restserver_state#remreststate{conn_state = online}},?Res_monitoring_timeout};


%% GUN DOWN
%% --------
%% SENDER:    The Gun connection process associated with the remote resource monitor
%% WHEN:      When it has disconnected from the remote REST server
%% PURPOSE:   Inform the resource monitor that connection with
%%            the remote REST server was lost (possibly, temporarily)
%% CONTENTS:  1) The PID of the Gun connection process
%%            2) The protocol used in the connection (HTTP/1.1, unused variable)
%%            3) The disconnection reason (unused variable)
%%            4) The list of streams that were killed before completion (unused
%%               variable, their associated responses are considered lost)
%% MATCHES:   When the resource monitor is connected with the remote REST
%%            server (and the request comes from the JANET Controller node)
%% ACTIONS:   -
%% NEW STATE: Update the remote REST server 'conn_state' variable to 'connecting'
%% TIMEOUT:   Reinitialize the resources monitoring timeout
%%
%% NOTE:      No explicit reconnection attempt is necessary since the Gun connection process
%%            will periodically attempt to reconnect with the remote REST server, sending
%%            a 'gun_up' message when it succeeds
%%
handle_info({gun_down,ConnPid,_Protocol,_Reason,_KilledStreams},SrvState=#resmonstate{rem_restserver_state = #remreststate{conn_state = online, conn_pid = ConnPid}}) ->
 
 % Update the remote REST server 'conn_state' variable
 % and reinitialize the resources monitoring timeout
 {noreply,SrvState#resmonstate{rem_restserver_state = SrvState#resmonstate.rem_restserver_state#remreststate{conn_state = connecting}},?Res_monitoring_timeout};


%% GUN ERROR
%% ---------
%% SENDER:    The Gun connection process associated with the remote resource monitor
%% WHEN:      When an error occurs at a connection-wide level
%% PURPOSE:   Inform the remote resource monitor of the error
%% CONTENTS:  1) The PID of the Gun connection process
%%            2) The error reason
%% MATCHES:   (always)
%% ACTIONS:   -
%% NEW STATE: - (Currently the message is just removed from
%%              the remote resource monitor's receiving queue)
%% TIMEOUT:   Reinitialize the resources monitoring timeout
%%
handle_info({gun_error,ConnPid,_Reason},SrvState=#resmonstate{rem_restserver_state = #remreststate{conn_state = online, conn_pid = ConnPid}}) ->

 % Ignore the error and reinitialize the resources monitoring timeout
 {noreply,SrvState,?Res_monitoring_timeout};


%% GUN CONNECTION PROCESS DOWN
%% ---------------------------
%% SENDER:    The Erlang Run-Time System (ERTS)
%% WHEN:      When the monitored Gun connection process terminates
%% PURPOSE:   Inform of the Gun connection process termination (and consequently,
%%            that the connection with the remote REST server was lost)
%% CONTENTS:  1) The monitor reference the notification refers to
%%            2) The PID of the process that was monitored (the Gun connection process)
%%            3) The reason for the monitored process's termination (unused variable)
%% MATCHES:   (always) (when the monitor reference and the PID of the
%%            monitored process match the ones in the server's state)
%% ACTIONS:   Respawn the Gun connection process for attempting to
%%            re-establish a connection with the remote REST server
%% NEW STATE: Update the remote REST server 'conn_state' to 'connecting' and
%%            set the 'conn_pid' and 'conn_ref' variables to their new values
%% TIMEOUT:   Reinitialize the resources monitoring timeout
%% 
handle_info({'DOWN',ConnRef,process,ConnPid,_Reason},SrvState=#resmonstate{rem_restserver_state = #remreststate{ipaddr = RemoteRESTServerAddr,
                                                                            port = RemoteRESTServerPort, conn_pid = ConnPid,conn_ref=ConnRef}}) ->
 % Respawn the Gun connection process for attempting to re-connect with the
 % remote REST server, obtaining its PID and a monitor reference towards it
 {NewConnPid,NewConnRef} = gun_start_monitor(RemoteRESTServerAddr,RemoteRESTServerPort),

 % Update the remote REST server 'conn_state' to 'connecting', set the 'conn_pid' and
 % 'conn_ref' variables to their new values and reinitialize the resources monitoring timeout
 {noreply,SrvState#resmonstate{rem_restserver_state = SrvState#resmonstate.rem_restserver_state#remreststate
                               {conn_state = connecting, conn_pid = NewConnPid, conn_ref = NewConnRef}},?Res_monitoring_timeout}.


%% ========================================================== TERMINATE ========================================================== %% 

terminate(_,#resmonstate{node_hosts_states = NodeHostsStates, rem_restserver_state = RemRESTServerState}) ->

 % Terminate all stub nodes spawned in the node hosts via the init:stop() function
 [ rpc:cast(StubNode,init,stop,[]) || {nodehoststate,_HostName,_Status,StubNode} <- NodeHostsStates, StubNode =/= node() ],

 % Retrieve the Gun Connection Pid
 ConnPid = RemRESTServerState#remreststate.conn_pid,

 if
 
  % If the Gun connection process is alive, terminate it
  is_pid(ConnPid) ->
   ok = gun:close(ConnPid);
 
  true ->
   ok
 end,   
    
 % Stop the Gun application
 % and terminate
 ok = application:stop(gun).


%%====================================================================================================================================
%%                                                    PRIVATE HELPER FUNCTIONS
%%==================================================================================================================================== 


%% DESCRIPTION:  Body function of the homonymous process spawned by the 'sim_resmonitor'
%%               for spawning a stub Erlang node on a host JANET nodes can be deployed in
%%
%% ARGUMENTS:    - HostName:  The host name
%%               - ResMonPID: The PID of its parent 'sim_restmonitor' process
%%
%% RETURNS:      - ok -> The spawning of the stub Erlang node on the host was successful, and the
%%                       'sim_resmonitor' parent was informed that the node host is online via a cast()
%%
sim_stubspawn(HostName,ResMonPID) ->

 % Prepare the Host, Name and Args parameters of the stub node to be spawned on the host
 NodeHost = HostName,
 NodeName = "janet-stub",
 NodeArgs = "-setcookie janet_simulator_cookie -connect_all false -pa ~janet_simulator/ebin/",
 
 % Indefinetely attempt to spawn the stub node on the host
 spawn_stub_node(NodeHost,NodeName,NodeArgs,ResMonPID).
 
 
%% Indefinetely attempts to spawn a stub node on a host JANET nodes can
%% be deployed in (sim_stubspawn(HostName,ResMonPID) helper function)
spawn_stub_node(NodeHost,NodeName,NodeArgs,ResMonPID) ->

 % Attempt to spawn the stub node on the remote host
 case slave:start(NodeHost,NodeName,NodeArgs) of
 
  {ok,StubNode} ->
  
   % If the spawning was successful, inform the 'sim_resmonitor' that the host 
   % is online by forwarding it the complete node name, and then terminate
   gen_server:cast(ResMonPID,{stub_node_online,NodeHost,StubNode,self()});
   
  {error,_Reason} ->
  
   % If the spawning was unsuccessful, perform
   % a new attempt after a predefined delay
   timer:sleep(?Stubs_spawn_attempts_delay),
   spawn_stub_node(NodeHost,NodeName,NodeArgs,ResMonPID)
 end.
 
 
%% DESCRIPTION:  Spawns the Gun connection process for monitoring the connection with the
%%               remote REST server and returns its PID and a monitor reference towards it
%%
%% ARGUMENTS:    - RemoteRESTServerAddr: The IP address of the remote REST server (a list)
%%               - RemoteRESTServerPort: The port of the remote REST server (int > 0)
%%
%% RETURNS:      - {ConnPid,ConnRef} -> The PID of the Gun connection process and a monitor reference towards it
%%
gun_start_monitor(RemoteRESTServerAddr,RemoteRESTServerPort) when is_list(RemoteRESTServerAddr),
                                                                  is_integer(RemoteRESTServerPort), RemoteRESTServerPort > 0 ->

 % Spawn the Gun connection process by passing the remote REST server as the connection target
 %
 % NOTE: The 'retry' map parameter defines the maximum number of connection attempts towards
 %       the remote REST server before the Gun connection process gives up and terminates
 %       (leading to a GUN CONNECTION PROCESS DOWN message to be received in the handle_info()
 %       callback), with every connection attempt being performed every 5 seconds
 %
 {ok,ConnPid} = gun:open(RemoteRESTServerAddr,RemoteRESTServerPort,#{retry => 10000}), 
 
 % Create a monitor towards the Gun connection process
 ConnRef = monitor(process,ConnPid),
 
 % Return the PID of the Gun connection process and the monitor reference towards it
 {ConnPid,ConnRef}. 
 
 
%% [TODO]
print_remresmonitor_summary(_SrvState) ->
 "Puppa".
 

%%====================================================================================================================================
%%                                                         START FUNCTION                                                        
%%====================================================================================================================================

%% Called by the Janet Simulator top-level supervisor (sup_jsim) at boot time
start_link() ->
 gen_server:start_link({local,?MODULE},?MODULE,[],[]). % The spawned process is also registered locally under the 'sim_resmonitor' name