%% This module represents the JANET Simulator remote hosts monitor %%

-module(sim_hostsmonitor).
-behaviour(gen_statem).

-export([start_link/0,callback_mode/0,init/1,handle_event/4]). % gen_statem Behaviour Callback Functions

%% ---------------------------------  Constants --------------------------------- %%

% Delay before the startup report of the remote hosts monitor
-define(Startup_report_delay,6 * 1000).       % Default: 10 * 1000 (10 seconds)

% Period between protracted reports of the remote
% hosts monitor (MUST be > Startup_report_delay)
-define(Protracted_report_period,60 * 1000).  % Default: 60 * 1000 (60 seconds)

% Host watchdogs ping period
-define(Wdg_ping_period,15 * 1000).           % Default: 15 * 1000 (15 seconds)
		
%% ------------------- State Machine State and Data Definitions ------------------- %%

%% State: {startup,protracted}   (Whether the startup report was printed or not)
%% Data:  [HostMonitor#hostmon]

%% This record represents a host monitor
-record(hostmon,    
        {
		 name,             % The name of the monitored host
		 state,            % The state of the monitored host ('offline'|'online')
		 type,             % The type of the monitored host ('nodehost'|'restsrv')
		 wdg_pid,          % The PID of the 'host_watchdog' process monitoring such host
		 wdg_ref           % A reference used for monitoring the 'host_watchdog' process monitoring such host
		}).
		
%%====================================================================================================================================
%%                                                  GEN_STATEM CALLBACK FUNCTIONS                                                        
%%====================================================================================================================================

%% ======================================================== CALLBACK_MODE ======================================================== %%
callback_mode() ->

 % Defines the event-handling mode of the 'gen_statem' engine
 handle_event_function.


%% ============================================================ INIT ============================================================ %%
init(_) ->
 
 % Retrieve the 'remote_rest_server_addr' environment variable
 {ok,RemRESTSrvAddr} = application:get_env(remote_rest_server_addr),
 
 % Retrieve the hostnames of remote node hosts
 RemNodesHosts = get_remote_node_hosts(RemRESTSrvAddr),
 
 % Retrieve the PID of the 'sim_hostsmonitor' process
 SimMonPid = self(),
 
 %% ------------------------- Remote Node Hosts monitors initialization ------------------------- %%
  
 % Spawn a 'host_watchdog' process for each remote node host
 RemNodesHostsWdgs = [ {RemNodeHost,proc_lib:spawn(fun() -> host_monitor(SimMonPid,RemNodeHost) end)} || RemNodeHost <- RemNodesHosts],
 
 % Initialize the list of host monitors towards the nodes hosts
 RemNodesHostsMons = [ #hostmon{name = RemNodeHost, state = offline, type = nodehost, wdg_pid = WdgPID,
                                 wdg_ref = monitor(process,WdgPID)} || {RemNodeHost,WdgPID} <- RemNodesHostsWdgs],
 
 %% ----------------------- Remote REST Server host monitor initialization ----------------------- %%
 
 % Spawn a 'host_watchdog' process for the remote REST server
 RemRESTSrvWdg = proc_lib:spawn(fun() -> host_monitor(SimMonPid,RemRESTSrvAddr) end),
 
 % Initialize the host monitor towards the remote REST server
 RemRESTSrvMon = #hostmon{name = RemRESTSrvAddr, state = offline, type = restsrv, wdg_pid = RemRESTSrvWdg, wdg_ref = monitor(process,RemRESTSrvWdg)},
 
 %% --------------------------------- Remote Hosts Monitor Start --------------------------------- %%

 % Define the 'startup_report' and 'protracted_report' timers used by the remote hosts monitor
 StatemTimers = [ 
                 {{timeout,startup_report},?Startup_report_delay,none},
                 {{timeout,protracted_report},?Startup_report_delay + ?Protracted_report_period,none}
                ],

 % Return the initialization tuple to the 'gen_statem' engine:
 {
  ok,                                    % Indicates to the engine that the 'dev_statem' can start
  startup,                               % The initial State of the 'dev_statem'
  RemNodesHostsMons ++ [RemRESTSrvMon],  % The initial Data of the 'dev_statem'  
  StatemTimers                           % The list of timers to be initialized
 }.


%% ======================================================== HANDLE_EVENT ======================================================== %% 

%% ----------------------------------------------- GENERIC TIMERS (NAMED TIMERS) ----------------------------------------------- %%

%% STARTUP REPORT TIMER
%% --------------------
%% PURPOSE:   Report the list of offline remote hosts
%% ACTIONS:   Print the list of offline remote hosts and, if there are none,
%%            explicitly state that all JANET remote hosts are online
%% NEW STATE: Update the state to 'protracted'
%% NEW DATA:  -
%%
handle_event({timeout,startup_report},_,startup,HostsMons) ->

 % Report the list of offline remote hosts
 case report_offline_hosts(HostsMons) of
 
  % If all remote hosts are online (and so nothing
  % was reported), explicitly report the fact
  all_online ->
   io:format("~n[sim_hostsmonitor]: All JANET remote hosts appear to be online~n");
   
  % Otherwise if at least one remote
  % host was reported as offline, do nothing
  ok ->
   ok
 end,
 
 % Update the state to 'protracted' and keep the data
 {next_state,protracted,HostsMons};
 

%% PROTRACTED REPORT TIMER
%% -----------------------
%% PURPOSE:   Report the list of offline remote hosts
%% ACTIONS:   Print the list of offline remote hosts, if any
%% NEW STATE: -
%% NEW DATA:  -
%%
handle_event({timeout,protracted_report},_,protracted,HostsMons) ->

 % Report the list of offline remote hosts, if any
 report_offline_hosts(HostsMons),
 
 % Keep the state and data and reinitialize the protracted report timer
 {keep_state_and_data,[{{timeout,protracted_report},?Protracted_report_period,none}]};
 
 
%% -------------------------------------------------- EXTERNAL CALLS CALLBACKS -------------------------------------------------- %% 

%% GET_REMOTE_HOSTS_STATES
%% -----------------------
%% SENDER:    The simulation controller (the user)
%% WHEN:      -
%% PURPOSE:   Retrieve the remote hosts states
%% CONTENTS:  -
%% MATCHES:   (always) (when the request comes from the JANET Simulator node)
%% ACTIONS:   Return the caller a tuple containing the list of remote nodes
%%            that are offline and whether the remote REST server is offline 
%% NEW STATE: -
%% NEW DATA:  -
%%  
handle_event({call,ReqPID},get_remote_hosts_states,_State,HostsMons) ->

 % Retrieve a tuple containing the list of remote host nodes that
 % are offline and whether the remote REST server is offline
 RemHostsOffline = get_offline_hosts(HostsMons),
 
 % Keep the state and data, return the list of offline hosts
 % to the caller and reinitialize the recurrent report timer
 {keep_state_and_data,[{reply,ReqPID,RemHostsOffline},{{timeout,protracted_report},?Protracted_report_period,none}]};
 
 
%% -------------------------------------------------- EXTERNAL CASTS CALLBACKS -------------------------------------------------- %% 

%% HOST_STATE_UPDATE
%% -----------------
%% SENDER:    One of the 'host_watchdog' processes spawned by the 'sim_hostsmonitor'
%% WHEN:      When its 'ping' towards the remote host completes, either successfully or not
%% PURPOSE:   Inform the 'sim_hostmonitor' of the remote host state
%% CONTENTS:  1) The name of the monitored host
%%            2) Its current state ('offline'|'online')
%% MATCHES:   (always) (when the request comes from the JANET Simulator node)
%% ACTIONS:   If the remote host state changed and the gen_statem is in the 'protracted' state,
%%            report the state change according to host type ('nodehost' or 'restsrv')
%% NEW STATE: -
%% NEW DATA:  Update the 'state' variable in the associated host monitor
%%  
handle_event(cast,{host_state_update,HostName,HostState,WdgPID},State,HostsMons) when node(WdgPID) =:= node() ->

 % Retrieve the monitor associated with the HostName
 {value,HostMon,NewHostsMons} = lists:keytake(HostName,2,HostsMons), 

 % If the remote host state changed and the gen_statem is in the 'protracted'
 % state, report the state change according to host type ('nodehost' or 'restsrv')
 report_host_state_change(State,HostName,HostMon#hostmon.state,HostState,HostMon#hostmon.type),
 
 % Update the 'state' variable in the host monitor
 NewHostMon = HostMon#hostmon{state = HostState},
 
 % Keep the state and update the list of host monitors
 {keep_state,NewHostsMons ++ [NewHostMon]};
 
 
%% -------------------------------------------------- EXTERNAL INFO CALLBACKS -------------------------------------------------- %% 

%% HOST WATCHDOG DOWN
%% ------------------

%% SENDER:    The Erlang Run-Time System (ERTS)
%% WHEN:      When one of the monitored 'host_watchdog' processes terminates
%% PURPOSE:   Inform the 'sim_hostsmonitor' of the process termination
%% CONTENTS:  1) The monitor reference the notification refers to
%%            2) The PID of the process that terminated
%%            3) The reason for the monitored process termination
%% MATCHES:   (always)
%% ACTIONS:   Ignore the error and attempt to respawn the
%%            'host_watchdog' process associated with such host
%% NEW STATE: -
%% NEW DATA:  Update the 'wdg_pid' and 'wdg_ref' variables in the associated host monitor
%%  
handle_event(info,{'DOWN',MonRef,process,_DeadWdgPID,_Reason},_State,HostsMons) ->

 % Retrieve the host monitor associated with the watchdog process that terminated
 {value,HostMon,NewHostsMons} = lists:keytake(MonRef,6,HostsMons), 
 
 % Retrieve the PID of the 'sim_hostsmonitor' process
 SimMonPid = self(),

 % Respawn the 'host_watchdog' process for such remote host
 NewWdgPID = proc_lib:spawn(fun() -> host_monitor(SimMonPid,HostMon#hostmon.name) end),

 % Create a monitor towards the new 'host_watchdog' process
 NewWdgRef = monitor(process,NewWdgPID),

 % Update the 'wdg_pid' and 'wdg_ref' variables in the host monitor
 NewHostMon = HostMon#hostmon{wdg_pid = NewWdgPID, wdg_ref = NewWdgRef},
 
 % Keep the state and update the list of host monitors
 {keep_state,NewHostsMons ++ [NewHostMon]}.


%%====================================================================================================================================
%%                                                    PRIVATE HELPER FUNCTIONS
%%==================================================================================================================================== 

%% Returns the list of remote node hostnames (init() helper function)
get_remote_node_hosts(RemRESTSrvAddr) ->
 
 % Retrieve the 'nodes_hosts' environment variable
 {ok,NodesHosts} = application:get_env(nodes_hosts),
 
 % Return the list of remote node hostnames by
 % subtracting from the list of nodes hosts:
 %  - The hostname of the remote REST server
 %  - The 'localhost' hostname
 %  - The JANET simulator node host name
 %
 NodesHosts -- [RemRESTSrvAddr,"localhost",net_adm:localhost()].


%% Reports a host state change according to its type and the current state of the remote hosts monitor
%% (handle_event({cast,WdgPid},{host_state_update,HostName,HostState},State,HostsMons) helper function

% Remote hosts monitor in the 'startup' state -> do not report
report_host_state_change(startup,_Hostname,_OldState,_NewState,_Type) ->
 ok;

% No host state change -> do not report
report_host_state_change(protracted,_Hostname,SameState,SameState,_Type) ->
 ok;
 
% Remote REST server 'online' -> 'offline'
report_host_state_change(protracted,RemRESTSrvAddr,online,offline,restsrv) ->
 io:format("[sim_hostsmonitor]: <WARNING> The remote REST server \"~s\" appears to be offline~n",[RemRESTSrvAddr]);

% Remote REST server 'offline' -> 'online'
report_host_state_change(protracted,RemRESTSrvAddr,offline,online,restsrv) ->
 io:format("[sim_hostsmonitor]: The remote REST server \"~s\" is now online~n",[RemRESTSrvAddr]);

% Remote node host 'online' -> 'offline'
report_host_state_change(protracted,RemNodeHost,online,offline,nodehost) ->
 io:format("[sim_hostsmonitor]: <WARNING> Remote node host \"~s\" appears to be offline~n",[RemNodeHost]);

% Remote node host 'offline' -> 'online'
report_host_state_change(protracted,RemNodeHost,offline,online,nodehost) ->
 io:format("[sim_hostsmonitor]: Remote node host \"~s\" is now online~n",[RemNodeHost]).
 

%% Reports the list of offline remote hosts
%% ('startup_report_timer','protracted_report_timer' helper function
report_offline_hosts(HostsMons) ->

 % Retrieve a tuple containing the list of offline remote
 % node hosts and the state of the remote REST server
 {OfflineRemNodesHosts,RemRESTSrvState} = get_offline_hosts(HostsMons),
 
 % Print the offline hosts
 print_offline_hosts(OfflineRemNodesHosts,RemRESTSrvState).
 
%% Returns a tuple containing the list of offline remote node hosts and the state of the remote
%% REST server (report_offline_hosts(HostMons),'get_remote_hosts_states' helper function)
get_offline_hosts(HostsMons) ->
 
 % Extract from the list of host monitors the
 % one associated with the remote REST server
 {value,RemRESTSrvMon,NodesHostsMons} = lists:keytake(restsrv,4,HostsMons), 
 
 % Represent the remote REST server state as an empty
 % list if its online or its HostName if its offline
 RemRESTSrvState = case RemRESTSrvMon#hostmon.state of
                    online ->
		  	         [];
				    offline ->
				     RemRESTSrvMon#hostmon.name
				   end,
                          
 % Derive the list of offline remote node hosts
 OfflineRemNodesHosts = [ NodeHost || {hostmon,NodeHost,State,_Type,_WdgPid,_WdgRef} <- NodesHostsMons, State =:= offline ],
 
 % Return the list of offline remote node hosts and the remote REST server state in a tuple
 {OfflineRemNodesHosts,RemRESTSrvState}.


%% Prints the list of offline hosts according to their
%% type (report_offline_hosts(HostMons) helper function)
print_offline_hosts([],[]) ->

 % If no remote host is offline, return the 'all_online'
 % atom (used by the 'startup_report' timer)
 all_online;

print_offline_hosts([],RemRESTSrvAddr) ->
 
 % If only the remote REST server is offline
 io:format("~n[sim_hostsmonitor]: <WARNING> The remote REST server \"~s\" appears to be offline~n",[RemRESTSrvAddr]);
 
print_offline_hosts(RemNodesHost,[]) when length(RemNodesHost) == 1 ->
 
 % If only a single remote node host is offline
 io:format("~n[sim_hostsmonitor]: <WARNING> The remote node host \"~s\" appears to be offline~n",[RemNodesHost]);
 
print_offline_hosts(RemNodesHosts,[]) ->
 
 % If only remote nodes hosts are offline
 io:format("~n[sim_hostsmonitor]: <WARNING> The following remote node hosts appear to be offline: ~0p~n",[RemNodesHosts]);
 
print_offline_hosts(RemNodesHost,RemRESTSrvAddr) when length(RemNodesHost) == 1 ->
 
 % If the remote REST server and a single remote node host are offline
 io:format("~n[sim_hostsmonitor]: <WARNING>~n"),
 io:format("|-- The remote REST server \"~s\" appears to be offline~n",[RemRESTSrvAddr]),
 io:format("|-- The remote node host \"~s\" appears to be offline~n",[RemNodesHost]);
 
print_offline_hosts(RemNodesHosts,RemRESTSrvAddr) ->
 
 % If the remote REST server and multiple remote node host are offline
 io:format("~n[sim_hostsmonitor]: <WARNING>~n"),
 io:format("|-- The remote REST server \"~s\" appears to be offline~n",[RemRESTSrvAddr]),
 io:format("|-- The following remote nodes hosts appear to be offline: ~0p~n",[RemNodesHosts]).
 
 
%% ----------------------------------------- host_monitor process ----------------------------------------- %% 

%% DESCRIPTION:  Body function of the omonymous process used for monitoring the reachability of a
%%               remote host, which is inferred by periodically pinging it via the underlying OS
%%
%% ARGUMENTS:    - SimMonPid: The PID of the 'sim_hostsmonitor' process where to report the remote host state
%%               - HostName:  The remote hostname which to monitor the state
%%
%% RETURNS:      - (can only exit by receiving an exit signal from its 'sim_hostsmonitor' parent)
%%
host_monitor(SimMonPid,HostName) ->

 % Build the OS ping command towards the HostName
 %
 % NOTES: - The "-c 1" option restricts the ping to one attempt
 %        - The >/dev/null reidirection is used
 %          for discarding the ping verbose output
 %        - "; echo $?" allows to extract the result of the ping execution:
 %             + "0" -> ping succeeded
 %             + "_" -> ping failed
 %
 PingCmd = io_lib:format("ping -c 1 ~s >/dev/null ; echo $?",[HostName]),
 
 % Start pinging the remote host
 host_monitor_ping(SimMonPid,HostName,PingCmd).

%% Continuously pings a remote host and returns its state to the
%% 'sim_hostmonitor' process (host_monitor(SimMonPid,HostName) helper function)
host_monitor_ping(SimMonPid,HostName,PingCmd) ->

 % Attempt to ping the remote host, restricting
 % the result returned by the OS to 1 character
 case os:cmd(PingCmd,#{max_size => 1}) of
  "0" ->
  
    % If the ping towards the remote host was successful, inform the
	% 'sim_hostsmonitor' process that the host is online via a cast()
    gen_statem:cast(SimMonPid,{host_state_update,HostName,online,self()});
	
  _ ->
	
	% Otherwise inform the 'sim_hostsmonitor'
	% process that the host is offline via a cast()
	gen_statem:cast(SimMonPid,{host_state_update,HostName,offline,self()})
 end,
 
 % Sleep for the predefined period between ping attempts
 timer:sleep(?Wdg_ping_period),
 
 % Recursively call the function
 host_monitor_ping(SimMonPid,HostName,PingCmd).
 
 
%%====================================================================================================================================
%%                                                         START FUNCTION                                                        
%%====================================================================================================================================

%% Called by the Janet Simulator top-level supervisor (sup_jsim) at boot time
start_link() ->
 gen_statem:start_link({local,?MODULE},?MODULE,[],[]).  % The spawned process is also registered locally under the 'sim_hostsmonitor' name