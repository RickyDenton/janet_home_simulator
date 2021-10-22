%% This module represents the handler of a paired device in the JANET Controller application %%

-module(ctr_devhandler).
-behaviour(gen_server).

-export([start_link/2,init/1,terminate/2,handle_call/3,handle_cast/2,handle_info/2]).  % gen_server Behaviour Callback Functions

-include("ctr_mnesia_tables_definitions.hrl").  % Janet Controller Mnesia Tables Records Definitions

%% This record represents the state of a 'ctr_devhandler' gen_server (which is constant)
-record(devhandlerstate,    
        {
		 dev_id,       % The ID of the handled device node
		 dev_srv_pid,  % The PID of the device node's 'dev_server' process
		 dev_srv_mon   % A reference used for monitoring the device node's 'dev_server' process
		}).

% Maximum time between two device configuration updates for a new configuration updated
% to be converted in its entirety to the map that will be sent to the remote server
-define(Max_time_before_complete_update,85). % Default: 85 seconds  

%%====================================================================================================================================
%%                                                  GEN_SERVER CALLBACK FUNCTIONS                                                        
%%====================================================================================================================================

%% ============================================================ INIT ============================================================ %%
init({Dev_id,DevSrvPid}) ->

 % Trap exit signals so to allow cleanup operations when terminating (terminate(Reason,SrvState) callback function)
 process_flag(trap_exit,true),
 
 % Attempt to register the handler's PID in the 'ctr_device' table
 case register_handler(Dev_id) of
 
  {aborted,device_not_exists} ->
   
    % If the device was not found in the 'ctr_device' table, meaning that it was deleted
	% between its pairing and the spawning of its handler, abort the initialization 
    {stop,ignore};
	
  {atomic,ok} ->
 
    % If the device's handler was successfully registered, create a monito
	% towards the device node's 'dev_server' process identified by "DevSrvPid"
    MonRef = monitor(process,DevSrvPid),
	
	% Inform the remote REST server via the 'ctr_httpclient' that
	% the device has paired with the controller at the current time
	%
	% [SERVER COMPATIBILITY]: "online" -> 1
	%
	gen_server:cast(ctr_httpclient,{dev_conn_update,Dev_id,1,self(),erlang:system_time(second)}),

    % Return the devhandler server (constant) state
    {ok,#devhandlerstate{dev_id = Dev_id, dev_srv_pid = DevSrvPid, dev_srv_mon = MonRef}}
 end.


%% ========================================================= HANDLE_CALL ========================================================= %%

%% DEV_CONFIG_CHANGE
%% -----------------
%% SENDER:    The controller's REST handler 'ctr_resthandler'
%% WHEN:      -
%% PURPOSE:   Change the state machine configuration in the handled device
%% CONTENTS:  The requested new configuration of the device's state machine (whose validity is not checked for here)
%% MATCHES:   (always) (when the requests comes from the JANET Controller node)
%% ACTIONS:   Forward the configuration change command to the device's 'dev_server'
%%            via a synchronous call and return its response to the caller 
%% ANSWER:    The reply of the device's 'dev_server' (which corresponds to the reply of the device's 'dev_statem')
%% NEW STATE: -
%%
handle_call({dev_config_change,NewCfg},{ReqPid,_},SrvState) when node(ReqPid) =:= node() ->
 
 % Forward the configuration change command to the device's 'dev_server', waiting for its response up to a predefined timeout
 CfgChangeRes = try gen_server:call(SrvState#devhandlerstate.dev_srv_pid,{dev_config_change,NewCfg},4500)
 catch
  exit:{timeout,_} ->
  
   % dev_server timeout
   {error,dev_timeout}
 end,
 
 % Depending on the result of the device configuration change
 case CfgChangeRes of
  {error, Reason} ->
   
   % In case of error ('dev_timeout' included), report it to the caller
   {reply,{error,Reason},SrvState};
   
  {ok,{UpdatedCfg,Timestamp}} ->
  
    % If the device returned the updated device configuration and timestamp, parse it so to obtain its associated
	% associated configuration map with its timestamp to be sent to the remote server via the 'ctr_resthandler'
    [{UpdatedCfgMap,Timestamp}] = parse_new_devconfigs([{UpdatedCfg,Timestamp}],SrvState#devhandlerstate.dev_id),
  
    % Return the device configuration map and timestamp to the 'ctr_resthandler' 
	{reply,{ok,{UpdatedCfgMap,Timestamp}},SrvState}
 end;


%% Unexpected call
handle_call(Request,From,SrvState=#devhandlerstate{dev_id = Dev_id}) ->
  
 % Report that an unexpected call was received by this gen_server
 io:format("[ctr_devhandler-~w]: <WARNING> Unexpected call (Request = ~p, From = ~p, SrvState = ~p)~n",[Dev_id,Request,From,SrvState]),

 % Reply with a stub message and keep the SrvState
 {reply,unsupported,SrvState}.


%% ========================================================= HANDLE_CAST ========================================================= %% 

%% DEV_CONFIG_UPDATE
%% -----------------
%% SENDER:    The device's 'dev_server' process
%% WHEN:      When the device's state machine configuration changes
%% PURPOSE:   Inform the device handler of the updated state machine configuration
%% CONTENTS:  1) The PID of the device's 'dev_server' ("security purposes")
%%            2) The updated configuration of the device's state machine
%%            3) The timestamp of the updated configuration
%% MATCHES:   (always) (the request comes from the device's 'dev_server' process)
%% ACTIONS:   Parse the updated device configuration and send it
%%            to the remote REST server via the 'ctr_httpclient'
%% NEW STATE: -
%%
handle_cast({dev_config_update,DevSrvPid,CfgUpdatesList},SrvState) when DevSrvPid =:= SrvState#devhandlerstate.dev_srv_pid ->

 % Parse the list of device configuration updates, obtaining
 % the list of device configuration maps with their timestamps
 CfgMapsStamps = parse_new_devconfigs(CfgUpdatesList,SrvState#devhandlerstate.dev_id),
 
 % Build the list of device configuration updates to be sent to the remote server
 % by dropping from the list of device configuration maps with their timestamps the
 % elements with an empty configuration map and by append to each one the device ID
 DevCfgUpdates = [ {SrvState#devhandlerstate.dev_id,UpdatedCfgMap,Timestamp} || {UpdatedCfgMap,Timestamp} <- CfgMapsStamps, UpdatedCfgMap =/= #{}],

 % Depending on whether the resulting list of device configuration updates is empty
 case DevCfgUpdates of
 
  % If it is, do nothing
  [] ->
   ok;
   
  % Otherwise if it contains at least one element, send all device configuration
  % updates to the remote REST server via the controller HTTP client
  _ ->
   gen_server:cast(ctr_httpclient,{dev_config_update,DevCfgUpdates,self()})
 end,
  
 % Keep the server state
 {noreply,SrvState};


%% Unexpected cast
handle_cast(Request,SrvState=#devhandlerstate{dev_id = Dev_id}) ->

 % Report that an unexpected cast was received by this gen_server
 io:format("[ctr_devhandler-~w]: <WARNING> Unexpected cast (Request = ~p, SrvState = ~w)~n",[Dev_id,Request,SrvState]),

 % Keep the SrvState
 {noreply,SrvState}. 
 

%% ========================================================= HANDLE_INFO ========================================================= %%  

%% DEVICE NODE DOWN
%% ----------------
%% SENDER:    The Erlang Run-Time System (ERTS)
%% WHEN:      When the monitored 'dev_server' process on the device node terminates
%% PURPOSE:   Inform of the 'dev_server' process termination
%% CONTENTS:  1) The monitor reference the notification refers to
%%            2) The PID of the process that was monitored (the 'dev_server' process)
%%            3) The reason for the monitored process's termination
%% MATCHES:   (always) (when the monitor reference and the PID of the monitored process match the ones in the server's state)
%% ACTIONS:   If Reason =:= 'noproc' log the event (it should not happen), and then stop 
%%            the 'ctr_devhandler' server, unpairing the device node from the controller
%% ANSWER:    -
%% NEW STATE: Stop the server (reason = 'normal' because no errors should be propagated to the 'sup_devhandlers' supervisor)
%%
handle_info({'DOWN',MonRef,process,DevSrvPid,Reason},SrvState) when MonRef =:= SrvState#devhandlerstate.dev_srv_mon, DevSrvPid =:= SrvState#devhandlerstate.dev_srv_pid ->

 % If Reason =:= 'noproc', which is associated to the fact that the 'dev_server' passed a non-existing "DevSrvPid"
 % while pairing with the 'ctr_pairserver' or it died before the monitor could be established, log the error
 if
  Reason =:= noproc ->
   io:format("[ctr_devhandler-~w]: <WARNING> The 'dev_server' process of paired device does not exist~n",[SrvState#devhandlerstate.dev_id]);
  true ->
   ok
 end,
 
 % Stop the 'ctr_devhandler' server (reason = 'normal' because no errors should be propagated to the 'sup_devhandlers' supervisor)
 {stop,normal,SrvState}.


%% ========================================================== TERMINATE ========================================================== %% 

%% Called when the 'dev_handler' server is asked to shutdown by its 'sup_devhandlers' supervisor or if it crashes
terminate(_,SrvState) ->
  
 % Attempt to deregister the handler's PID from the 'ctr_device' table 
 deregister_handler(SrvState#devhandlerstate.dev_id),
 
 % If still active, remove the monitor towards the device's 'dev_server'
 % process, also flushing possible notifications from the message queue  
 demonitor(SrvState#devhandlerstate.dev_srv_mon,[flush]),
 
 % Inform the remote REST server via the 'ctr_httpclient' that
 % the device has unpaired from the controller at the current time
 %
 % [SERVER COMPATIBILITY]: "offline" -> 0
 %
 gen_server:cast(ctr_httpclient,{dev_conn_update,SrvState#devhandlerstate.dev_id,0,self(),erlang:system_time(second)}),
 
 % Terminate
 ok. 


%%====================================================================================================================================
%%                                                    PRIVATE HELPER FUNCTIONS
%%====================================================================================================================================  
 
%% Attempts to register the handler's PID in the 'ctr_device' table (init({Dev_id,DevSrvPid}) helper function)
register_handler(Dev_id) ->
 F = fun() ->
 
      % Retrieve the "Dev_id" entry from the 'ctr_device' table
	  case mnesia:wread({ctr_device,Dev_id}) of      % wread = write lock
	   [CtrDevRecord] ->
	   
	    % Register the device's handler by setting the 'handler_pid' field
	    mnesia:write(CtrDevRecord#ctr_device{handler_pid = self()});
		
	   [] ->
	   
	    % If the entry does not exist, it means that the device was deleted between
		% its pairing and the spawning of its handler, and so abort the transaction
	    mnesia:abort(device_not_exists)
      end
	 end,
	  
 mnesia:transaction(F).
 
%% Attempts to deregister the handler's PID from the 'ctr_device' table (terminate(_,SrvState) helper function)
deregister_handler(Dev_id) ->
 F = fun() ->
 
      % Retrieve the "Dev_id" entry from the 'ctr_device' table
	  case mnesia:wread({ctr_device,Dev_id}) of      % wread = write lock
	   [CtrDevRecord] ->
	   
	    % Deregister the device's handler by clearing the 'handler_pid' field
	    mnesia:write(CtrDevRecord#ctr_device{handler_pid = '-'});
		
	   [] ->
	   
	    % If the entry does not exist, it means that the
		% device was already deleted, and so just return
	    ok
      end
	 end,
	  
 mnesia:transaction(F).


%% DESCRIPTION:  Converts a list of new device configurations with their timestamps into the list of device configuration maps with their
%%               timestamps to be sent to the remote server (either synchronously, i.e. in a device command response, or asynchronously,
%%               i.e. via the controller HTTP client), also pushing the most recent update in the Controller Mnesia database
%%
%% ARGUMENTS:    - NewCfgList: The list of new device configurations with their associated timestamp [{Cfg,Timestamp}]
%%               - Dev_id:     The ID of the device the configuration updates refer to
%%
%% RETURNS:      - CfgMapsStamps: The list of device configuration maps with their timestamps to be sent to the remote server
%%    
parse_new_devconfigs([{NewConfig,Timestamp}|NextConfig],Dev_id) ->
 
 % Retrieve the device record from the Mnesia database
 case ctr_db:get_record(ctr_device,Dev_id) of 
  {error,not_found} ->
  
   % If the device was not found it means that it was deleted before its 'ctr_devhandler' could
   % be terminated and in this case create a "stub" device record that will be used for converting
   % the list of updated device configurations into the list of maps to be sent to the remote server
   io:format("[ctr_devhandler-~w]: <WARNING> Creating a stub #ctr_device record for creating the list of configuration updates maps (the device no longer exists in the database)~n",[Dev_id]),
   parse_new_devconfigs_record([{NewConfig,Timestamp}|NextConfig],Dev_id,#ctr_device{dev_id = Dev_id, config = NewConfig, lastupdate = Timestamp - 1000000},[]);
    
  {ok,CtrDevRecord} ->
  
   % If the device was found, start parsing the new device configurations
   parse_new_devconfigs_record([{NewConfig,Timestamp}|NextConfig],Dev_id,CtrDevRecord,[])
 end.

% Base case
parse_new_devconfigs_record([],Dev_id,CtrDevRecord,CfgMapsStamps) ->

 % Once all new device configurations have been parsed,
 % push the last, or more recent, one into the Mnesia database
 %
 % NOTE: If the device was deleted before its 'ctr_devhandler' could
 %       be terminated this operation will fail (error that is ignored)
 %      
 ctr_db:update_dev_config(Dev_id,CtrDevRecord#ctr_device.config,CtrDevRecord#ctr_device.lastupdate),

 % Return the list of updated device configuration maps with their timestamps
 CfgMapsStamps;
 
% Recursive case
parse_new_devconfigs_record([{NewConfig,Timestamp}|NextConfig],Dev_id,CtrDevRecord,CfgMapsStamps) ->

 % Convert the updated device configuration into the map that will be sent to the remote server
 NewCfgMap =
 if
  
  % If the controller never received a configuration update from the device, or the last one it received was
  % more than a predefined maximum time before this one, conver the new configuration in its entirety into a map
  CtrDevRecord#ctr_device.config =:= '-' orelse Timestamp - CtrDevRecord#ctr_device.lastupdate > ?Max_time_before_complete_update ->
   utils:devconfig_to_map_all(NewConfig);
	
  % Otherwise build a map containing only the traits
  % that changed from the previous configuration update	
  true -> 
   utils:devconfig_to_map_diff(CtrDevRecord#ctr_device.config,NewConfig)
 end,
 
 % Append the map and timestamp into the list of updated configuration maps and parse the next
 % updated device configuration, passing directly the updated device record for optimization purposes
 parse_new_devconfigs_record(NextConfig,Dev_id,CtrDevRecord#ctr_device{config = NewConfig,lastupdate = Timestamp},CfgMapsStamps ++ [{NewCfgMap,Timestamp}]).


%%====================================================================================================================================
%%                                                         START FUNCTION                                                        
%%====================================================================================================================================

%% Called by the device handlers supervisor 'sup_devhandlers' on behalf
%% of the 'ctr_pairserver' whenever a device pairs with the controller
start_link(Dev_id,DevSrvPid) ->
 gen_server:start_link(?MODULE,{Dev_id,DevSrvPid},[]).