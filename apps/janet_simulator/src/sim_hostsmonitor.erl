%% This module represents the JANET Simulator remote hosts monitor %%

-module(sim_hostsmonitor).
-behaviour(gen_statem).

-export([start_link/0,callback_mode/0,init/1,handle_event/4]). % gen_statem Behaviour Callback Functions

%% ---------------------------------  Constants --------------------------------- %%

% Delay before the startup report of the remote hosts monitor
-define(Startup_report_delay,6 * 1000).       % Default: 6 * 1000 (6 seconds)

% Period between protracted reports of the remote
% hosts monitor (MUST be > Startup_report_delay)
-define(Protracted_report_period,90 * 1000).  % Default: 90 * 1000 (90 seconds)

% Host watchdogs ping period
-define(Wdg_ping_period,10 * 1000).           % Default: 10 * 1000 (15 seconds)
		
%% ------------------- State Machine State and Data Definitions ------------------- %%

%% State: {'startup'|'protracted'|'idle'}
%% 
%%          - startup    -> Before the startup report on the connectivity states of the remote
%%                          hosts used by the JANET Simulator application (?Start_report_delay ms)
%%          - protracted -> After the startup report if at least one remote host
%%                          host is used in the JANET Simulator current configuration
%%          - idle       -> After the startup report if all hosts used in the JANET
%%                          Simulator current configuration reside in fact in the local host
%%
%% Data:  [HostMonitor#hostmon]  (A list of hosts monitor records, defined below)

%% This record represents a host monitor
-record(hostmon,    
        {
		 name,      % The name of the monitored host
		 state,     % The state of the monitored host ('offline'|'online')
		 type,      % The type of monitored host ('nodeshost'|'restsrv')
		 wdg_pid,   % The PID of the 'host_watchdog' process monitoring such host
		 wdg_ref    % A reference used for monitoring the 'host_watchdog' process
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
 
 % Retrieve the PID of the remote hosts monitor
 SimMonPid = self(),

 % Retrieve the 'distributed_mode' environment variable
 {ok,DistributedMode} = application:get_env(distributed_mode),
 
 % Define a fun() for filtering host mapping to the localhost
 LocalHostFilter = fun(HostName) -> utils:is_remote_host(HostName) end,

 %% ----------------- Remote REST Server and Node Hosts Retrieval and Filtering ----------------- %%

 % Retrieve the 'remote_rest_server_addr' environment variable
 {ok,RESTSrvAddr} = application:get_env(remote_rest_server_addr),

 % Filter the remote REST server address so to obtain
 % an empty list if in fact it maps to the localhost
 RemRESTSrvAddr = lists:filter(LocalHostFilter,[RESTSrvAddr]),

 % Retrieve the list of hostnames of remote nodes hosts
 RemNodesHosts = 
 case DistributedMode of
  false ->
  
   % If distributed mode is disabled, there are no remote nodes hosts
   [];
   
  true ->
  
   % If distributed mode is enabled, retrieve the 'nodes_hosts' environment variable
   {ok,NodesHosts} = application:get_env(nodes_hosts), 

   % If present, delete the remote REST server address from the nodes hosts list
   NodesHostsNoRESTSrv = NodesHosts -- [RESTSrvAddr],

   % Filter from the nodes hosts list all hostnames mapping in fact to the localhost
   lists:filter(LocalHostFilter,NodesHostsNoRESTSrv)
 end,

 %% ------------------------- Remote Nodes Hosts Monitors Initialization ------------------------- %%
  
 % Initialize the list of host monitors associated with remote nodes hosts, if any
 RemNodesHostsMons =
 case RemNodesHosts of
  
  % If there are no remote nodes
  % hosts, no host monitor is required
  [] ->
   [];
   
  % Otherwise if there is at least a remote nodes host
  _ ->
  
   % Spawn a 'host_watchdog' process for each remote nodes host
   RemNodesHostsWdgs = [ {RemNodesHost,proc_lib:spawn(fun() -> host_watchdog(SimMonPid,RemNodesHost) end)} || RemNodesHost <- RemNodesHosts],
 
   % Initialize and return the list of host monitors associated with remote nodes hosts
   [#hostmon{name = RemNodesHost, state = offline, type = nodeshost, wdg_pid = WdgPID, wdg_ref = monitor(process,WdgPID)} || {RemNodesHost,WdgPID} <- RemNodesHostsWdgs]
 end,
 
 %% ----------------------- Remote REST Server Host Monitor Initialization ----------------------- %%
 
 % Initialize the host monitor associated with the remote REST server, if any
 RemRESTSrvMonList =
 case RemRESTSrvAddr of
 
  % If the remote REST server resides in fact
  % in the localhost, no host monitor is required
  [] ->
   [];
   
  % Otherwise if the remote REST server
  % maps indeed to a remote host
  _ ->
   
   % Spawn a 'host_watchdog' process for the remote REST server
   RemRESTSrvWdg = proc_lib:spawn(fun() -> host_watchdog(SimMonPid,RemRESTSrvAddr) end),
 
   % Initialize the remote REST server host monitor and enclose it in a list
   [#hostmon{name = RemRESTSrvAddr, state = offline, type = restsrv, wdg_pid = RemRESTSrvWdg, wdg_ref = monitor(process,RemRESTSrvWdg)}]
 end,
 
 %% --------------------------------- Remote Hosts Monitor Start --------------------------------- %%

 % Define the 'startup_report' and 'protracted_report' timers used by the remote hosts monitor
 StatemTimers = [ 
                 {{timeout,startup_report},?Startup_report_delay,none},
                 {{timeout,protracted_report},?Startup_report_delay + ?Protracted_report_period,none}
                ],
 
 % Return the initialization tuple to the 'gen_statem' engine:
 {
  ok,                                      % Indicates to the engine that the 'dev_statem' can start
  startup,                                 % The initial State of the 'dev_statem'
  RemNodesHostsMons ++ RemRESTSrvMonList,  % The initial Data of the 'dev_statem' (the list of host monitors)
  StatemTimers                             % The list of timers to be initialized
 }.


%% ======================================================== HANDLE_EVENT ======================================================== %% 

%% ----------------------------------------------- GENERIC TIMERS (NAMED TIMERS) ----------------------------------------------- %%

%% STARTUP REPORT TIMER
%% --------------------
%% PURPOSE:   Report a summary of the connectivity states of the remote hosts
%%            used in the JANET Simulator application after a startup delay
%% ACTIONS:   A) If the list of host monitors is non empty, print the list of offline
%%               remote hosts according to their type ('nodeshost'|'restsrv') or, if
%%               there are none, inform that all JANET remote nodes appear to be online
%%            B) If the list of host monitors is empty (meaning that both the only nodes
%%               host and the remote REST server reside in fact in the localhost, inform
%%               the user that no further updates will be reported by the remote hosts monitor 
%% NEW STATE: A) Update the state to 'protracted'
%%            B) Update the state to 'idle', delete the 'protracted_report'
%%               timer and hibernate the remote hosts monitor 
%% NEW DATA:  -
%%
%% NOTES: 1) In case A) the sim_hostmonitor is not shutdown to allow it to reply to
%%           the "GET_REMOTE_HOSTS_STATES" call issued from the simulation controller
%%        2) In any case, this timer will fire only once 
%%

%% B) No host monitor is present -> Update the remote hosts monitor state to 'idle' and hibernate it
handle_event({timeout,startup_report},_,startup,[]) ->

 % Print a message informing the user that all hosts used by the JANET Simulator reside
 % in the localhost, and so that no further host states updated will be reported
 io:format("~n[sim_hostsmonitor]: In its current configuration all hosts used by the JANET Simulator reside in the localhost, no further hosts states updates will be reported~n"),
 
 % Update the state to 'idle', delete the
 % 'protracted_report' timer, and hibernate the gen_statem
 {next_state,idle,[],[{{timeout,protracted_report},cancel},hibernate]};

%% A) Host monitors present -> Print a report on the remote hosts connectivity states
handle_event({timeout,startup_report},_,startup,HostMons) ->

 % Report the list of offline remote hosts
 % according to their type ('nodeshost'|'restsrv') 
 case report_offline_hosts(HostMons) of
 
  % If all remote hosts are online (and so nothing was printed),
  % explicitly report that all JANET remote nodes appear to be online
  all_online ->
   io:format("~n[sim_hostsmonitor]: All remote hosts used by the JANET Simulator appear to be online~n");
   
  % Otherwise, if at least one remote
  % host was reported as offline, do nothing
  ok ->
   ok
 end,
 
 % Update the state to 'protracted' and keep the data
 {next_state,protracted,HostMons};
 

%% PROTRACTED REPORT TIMER
%% -----------------------
%% PURPOSE:   Periodically report the list of remote hosts used in
%%            the JANET Simulator application that appear to be offline
%% ACTIONS:   Report the list of offline remote hosts according
%%            to their type ('nodeshost'|'restsrv'), if any
%% NEW STATE: -
%% NEW DATA:  -
%%
handle_event({timeout,protracted_report},_,protracted,HostMons) ->

 % Report the list of offline remote hosts according
 % to their type ('nodeshost'|'restsrv'), if any
 report_offline_hosts(HostMons),
 
 % Keep the state and data and reinitialize the 'protracted_report' timer
 {keep_state_and_data,[{{timeout,protracted_report},?Protracted_report_period,none}]};
 
 
%% -------------------------------------------------- EXTERNAL CALLS CALLBACKS -------------------------------------------------- %% 

%% GET_REMOTE_HOSTS_STATES
%% -----------------------
%% SENDER:    The simulation controller (the user)
%% WHEN:      -
%% PURPOSE:   Retrieve a summary of the connectivity states of the
%%            remote hosts used in the JANET Simulator application
%% CONTENTS:  -
%% MATCHES:   (always) (when the request comes from the JANET Simulator node)
%% ACTIONS:   A) If the list of host monitors is non empty, return the caller
%%               caller a tuple containing the list of remote node hosts that
%%               are offline and whether the remote REST server is offline 
%%            B) If the list of host monitors is empty (meaning that both the only
%%               nodes host and the remote REST server reside in fact in the localhost)
%%               return a 'nohosts' atom to the caller and go back into hibernation 
%% NEW STATE: -
%% NEW DATA:  -
%%

%% B) No host monitor is present
handle_event({call,{ReqPID,_Tag}},get_remhosts_states,_State,[]) when node() =:= node(ReqPID) ->

 % Keep the state and data, return to the 'nohosts' atom to the caller and go back into hibernation
 {keep_state_and_data,[{reply,{ReqPID,_Tag},nohosts},hibernate]};
  
%% A) At least one host monitor is present
handle_event({call,{ReqPID,_Tag}},get_remhosts_states,_State,HostMons) when node() =:= node(ReqPID) ->

 % Keep the state and data, return to the caller the list of remote node hosts that are offline
 % and whether the remote REST server is offline, and reinitialize the 'protracted_report' timer
 {keep_state_and_data,[{reply,{ReqPID,_Tag},get_offline_hosts(HostMons)},{{timeout,protracted_report},?Protracted_report_period,none}]};
 
 
%% -------------------------------------------------- EXTERNAL CASTS CALLBACKS -------------------------------------------------- %% 

%% HOST_STATE_UPDATE
%% -----------------
%% SENDER:    One of the 'host_watchdog' processes spawned by the remote hosts monitor
%% WHEN:      When its 'ping' towards its associated remote host completes (whether successfully or not)
%% PURPOSE:   Inform the remote hosts monitor of the current connectivity state of the remote host
%% CONTENTS:  1) The monitored HostName
%%            2) Its sampled connectivity state ('offline'|'online')
%% MATCHES:   (always) (when the request comes from the JANET Simulator node)
%% ACTIONS:   If the remote host connectivity state differs from its previous sampling
%%            and the gen_statem is in the 'protracted' state, report such connectivity
%%            state change according to the host type ('nodeshost' or 'restsrv')
%% NEW STATE: -
%% NEW DATA:  Update accordingly the 'state' variable in the host monitor associated with HostName
%%  
handle_event(cast,{host_state_update,HostName,HostState,WdgPID},State,HostMons) when node(WdgPID) =:= node() ->

 % Retrieve the monitor associated with the HostName
 {value,HostMon,NewHostMons} = lists:keytake(HostName,2,HostMons),

 % If the remote host connectivity state differs from its previous sampling
 % and the gen_statem is in the 'protracted' state, report such connectivity
 % state change according to the host type ('nodeshost' or 'restsrv')
 Reported = report_host_state_change(State,HostName,HostMon#hostmon.state,HostState,HostMon#hostmon.type),
 
 % Update the 'state' variable in the host monitor
 NewHostMon = HostMon#hostmon{state = HostState},

 % Depending on whether the connectivity state change was reported
 case Reported of
 
  % If it was (and so the gen_statem is for sure in the 'protracted' state), keep
  % keep the state, update the list of host monitors and reinitialize the protracted
  % report timer for limiting the frequency of remote hosts state updates
  ok ->
   {keep_state,NewHostMons ++ [NewHostMon],[{{timeout,protracted_report},?Protracted_report_period,none}]};
    
  % Otherwise if the connectivity state change was not reported,
  % just keep the state and update the list of host monitors
  no_report ->
   {keep_state,NewHostMons ++ [NewHostMon]}
 end;
 
 
%% -------------------------------------------------- EXTERNAL INFO CALLBACKS -------------------------------------------------- %% 

%% HOST WATCHDOG DOWN
%% ------------------
%% SENDER:    The Erlang Run-Time System (ERTS)
%% WHEN:      When one of the monitored 'host_watchdog' processes
%%            spawned by the remote hosts monitor terminates
%% PURPOSE:   Inform the remote hosts monitor of the
%%            'host_watchdog' process termination
%% CONTENTS:  1) The monitor reference the notification refers to
%%            2) The PID of the process that has terminated
%%            3) The monitored process termination (or exit) reason
%% MATCHES:   (always)
%% ACTIONS:   Ignore the error and attempt to respawn the
%%            'host_watchdog' process associated with such HostName
%% NEW STATE: -
%% NEW DATA:  Update the 'wdg_pid' and 'wdg_ref' variables in the associated host monitor
%%  
handle_event(info,{'DOWN',MonRef,process,_DeadWdgPID,_Reason},_State,HostMons) ->

 % Retrieve the host monitor associated with the 'host_watchdog' process that terminated
 {value,HostMon,NewHostMons} = lists:keytake(MonRef,6,HostMons), 
 
 % Retrieve the PID of the remote hosts monitor process
 SimMonPid = self(),

 % Respawn the 'host_watchdog' process associated with such HostName
 NewWdgPID = proc_lib:spawn(fun() -> host_watchdog(SimMonPid,HostMon#hostmon.name) end),

 % Create a monitor towards the new 'host_watchdog' process
 NewWdgRef = monitor(process,NewWdgPID),

 % Update the 'wdg_pid' and 'wdg_ref' variables in the host monitor
 NewHostMon = HostMon#hostmon{wdg_pid = NewWdgPID, wdg_ref = NewWdgRef},
 
 % Keep the state and update the list of host monitors
 {keep_state,NewHostMons ++ [NewHostMon]}.


%%====================================================================================================================================
%%                                                    PRIVATE HELPER FUNCTIONS
%%==================================================================================================================================== 

%% If the old and new connectivity states associated with a remote host differ and the
%% remote hosts monitor is in the 'protracted' state, report the HostName connectivity state
%% change according to its type ('nodeshost' or 'restsrv') (HOST_STATE_UPDATE helper function)
 
% Remote hosts monitor in the 'startup' state -> do not report
report_host_state_change(startup,_Hostname,_OldState,_NewState,_Type) ->
 no_report;

% No host connectivity change -> do not report
report_host_state_change(_State,_Hostname,SameState,SameState,_Type) ->
 no_report;
 
% Remote REST server 'online' -> 'offline'
report_host_state_change(_State,RemRESTSrvAddr,online,offline,restsrv) ->
 io:format("[sim_hostsmonitor]: <WARNING> The remote REST server \"~s\" appears to be offline~n",[RemRESTSrvAddr]);

% Remote REST server 'offline' -> 'online'
report_host_state_change(_State,RemRESTSrvAddr,offline,online,restsrv) ->
 io:format("[sim_hostsmonitor]: The remote REST server \"~s\" is now online~n",[RemRESTSrvAddr]);

% Remote nodes host 'online' -> 'offline'
report_host_state_change(_State,RemNodesHost,online,offline,nodeshost) ->
 io:format("[sim_hostsmonitor]: <WARNING> Remote nodes host \"~s\" appears to be offline~n",[RemNodesHost]);

% Remote nodes host 'offline' -> 'online'
report_host_state_change(_State,RemNodesHost,offline,online,nodeshost) ->
 io:format("[sim_hostsmonitor]: Remote nodes host \"~s\" is now online~n",[RemNodesHost]).
 

%% Reports the list of offline remote hosts according to their type ('nodeshost'|'restsrv') 
%% (STARTUP REPORT TIMER, PROTRACTED REPORT TIMER helper function)
report_offline_hosts(HostMons) ->

 % Retrieve a tuple containing the list of offline remote nodes
 % hosts and the connectivity state of the remote REST server
 {OfflineNodesHosts,RemRESTSrvState} = get_offline_hosts(HostMons),
 
 % Print the list of offline remote hosts according to their type
 print_offline_hosts(OfflineNodesHosts,RemRESTSrvState).


%% Returns a tuple containing the list of offline remote nodes hosts and the connectivity state of
%% the remote REST server (report_offline_hosts(HostMons), GET_REMOTE_HOSTS_STATES helper function)
get_offline_hosts(HostMons) ->
 
 % Derive the list of nodes hosts monitors and the REST server connectivity
 % state by attempting to extract its host monitor from the hosts monitors list
 {NodesHostsMons,RemRESTSrvState} = 
 case lists:keytake(restsrv,4,HostMons) of
  false ->
 
   % If the remote REST server was not found in the host monitors list, meaning
   % that resides in fact in the localhost, return the list of nodes hosts
   % monitors and an empty list as a stub 'online' state for the REST server 
   {HostMons,[]};
   
  {value,RemRESTSrvMon,NodesHostsMons_} ->
  
   % If the remote REST server was found in the host monitors list, represent its
   % connectivity state as an empty list if its online or its HostName if its offline
   RemRESTSrvState_ = case RemRESTSrvMon#hostmon.state of
                      online ->
		    	       [];
				      offline ->
				       RemRESTSrvMon#hostmon.name
				     end,
                     
   % Return the required tuple
   {NodesHostsMons_,RemRESTSrvState_}
  end,
			
 % Derive the list of offline remote nodes hosts
 OfflineRemNodesHosts = [ NodesHost || {hostmon,NodesHost,State,_Type,_WdgPid,_WdgRef} <- NodesHostsMons, State =:= offline ],

 % Return the list of offline remote nodes hosts and the
 % connectivity state of the remote REST server in a tuple
 {OfflineRemNodesHosts,RemRESTSrvState}.


%% Print the list of offline remote hosts according to their type
%% ('nodeshost'|'restsrv') (report_offline_hosts(HostMons) helper function)

% No remote host is offline -> return the 'all_online'
% atom (used by the STARTUP REPORT TIMER)
print_offline_hosts([],[]) ->
 all_online;

% Only the remote REST server is offline
print_offline_hosts([],RemRESTSrvAddr) ->
 io:format("~n[sim_hostsmonitor]: <WARNING> The remote REST server \"~s\" appears to be offline~n",[RemRESTSrvAddr]);
 
% Only a single remote nodes host is offline 
print_offline_hosts(RemNodesHost,[]) when length(RemNodesHost) == 1 ->
 io:format("~n[sim_hostsmonitor]: <WARNING> The remote nodes host \"~s\" appears to be offline~n",[RemNodesHost]);
 
% Only (multiple) remote nodes hosts are offline
print_offline_hosts(RemNodesHosts,[]) ->
 io:format("~n[sim_hostsmonitor]: <WARNING> The following remote nodes hosts appear to be offline: ~0p~n",[RemNodesHosts]);
 
% The remote REST server and a single remote nodes host are offline
print_offline_hosts(RemNodesHost,RemRESTSrvAddr) when length(RemNodesHost) == 1 ->
 io:format("~n[sim_hostsmonitor]: <WARNING>~n"),
 io:format("|-- The remote REST server \"~s\" appears to be offline~n",[RemRESTSrvAddr]),
 io:format("|-- The remote nodes host \"~s\" appears to be offline~n",[RemNodesHost]);

% The remote REST server and multiple remote nodes hosts are offline
print_offline_hosts(RemNodesHosts,RemRESTSrvAddr) ->
 io:format("~n[sim_hostsmonitor]: <WARNING>~n"),
 io:format("|-- The remote REST server \"~s\" appears to be offline~n",[RemRESTSrvAddr]),
 io:format("|-- The following remote nodes hosts appear to be offline: ~0p~n",[RemNodesHosts]).
 

%% ------------------------------------ HOST_WATCHDOG PROCESS ------------------------------------ %%
 
%% DESCRIPTION: Body function of the omonymous process used for periodically monitoring the
%%              connectivity state of a remote host used in the JANET Simulator application
%%              and report it to the remote hosts monitor parent process 
%%
%% ARGUMENTS:    - SimMonPid: The PID of the remote hosts monitor parent process
%%                            where to report host connectivity state updates
%%               - HostName:  The host name whose connectivity state is to be monitored
%%
%% RETURNS:      - (can only terminate by receiving an exit signal from its parent process)
%%
%% NOTE:        The monitoring of the remote host connectivity state is
%%              implemented by periodically pinging it through the underlying OS       
%%
host_watchdog(SimMonPid,HostName) ->

 % Retrieve the family of the underlying operating system ('unix'|'win32')
 {OSFamily,_OSName} = os:type(),
 
 % Build the ping command depending on the OS family
 %
 % NOTES:  1) The "-c 2" (or "-n 2" in Windows systems)
 %            option limits the ping to two attempts
 %         2) The ">/dev/null" (or "> $null" in Windows systems)
 %            reidirection suppresses the command output
 %         3) "echo $?" (or "echo $LASTEXITCODE" in Windows systems) returns
 %            the exit code of the last command (0 -> success, !0 -> failure)  
 PingCmd = 
 case OSFamily of
  unix ->
   io_lib:format("ping -c 2 ~s >/dev/null ; echo $?",[HostName]);
  win32 ->
   io_lib:format("powershell.exe \"ping -n 2 ~s > $null; echo $LASTEXITCODE\"",[HostName])
 end,
 
 % Periodically ping the remote host, reporting each
 % result to the remote hosts monitor parent process 
 host_watchdog_ping(SimMonPid,HostName,PingCmd).


%% Periodically pings a remote host, reporting each result to the remote hosts
%% monitor parent process (host_watchdog(SimMonPid,HostName) helper function)
host_watchdog_ping(SimMonPid,HostName,PingCmd) ->

 % Execute the ping command, restricting the
 % result returned by the OS to a single character  
 case os:cmd(PingCmd,#{max_size => 1}) of
  
  % If a "0" was returned the ping towards the remote host was successful,
  % and so inform the remote hosts monitor that the host is online via a cast()
  "0" ->
   gen_statem:cast(SimMonPid,{host_state_update,HostName,online,self()});
	
  % If another value was returned the ping towards the remote host failed,
  % and so inform the remote hosts monitor that the host is offline via a cast()
  _ ->
   gen_statem:cast(SimMonPid,{host_state_update,HostName,offline,self()})
 end,
 
 % Sleep for the predefined period between ping executions
 timer:sleep(?Wdg_ping_period),
 
 % Recursively call the function
 host_watchdog_ping(SimMonPid,HostName,PingCmd).
 
 
%%====================================================================================================================================
%%                                                         START FUNCTION                                                        
%%====================================================================================================================================

%% Called by the Janet Simulator top-level supervisor (sup_jsim) at boot time
start_link() ->
 gen_statem:start_link({local,?MODULE},?MODULE,[],[]).  % The spawned process is also registered locally under the 'sim_hostsmonitor' name