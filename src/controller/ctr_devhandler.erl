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

    % Return the devhandler server (constant) state
    {ok,#devhandlerstate{dev_id = Dev_id, dev_srv_pid = DevSrvPid, dev_srv_mon = MonRef}}
 end.


%% ========================================================= HANDLE_CALL ========================================================= %%

%% DEV_CONFIG_CHANGE
%% -----------------
%% SENDER:    The controller's 'ctr_restserver' [TODO]: Double-check
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
 CfgChangeRes = try gen_server:call(SrvState#devhandlerstate.dev_srv_pid,{dev_config_change,NewCfg},4800)
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
  
   % If the device returned the updated device configuration and timestamp, attempt to push it in the Mnesia 'ctr_device' table
   %% [NOTE]: This probably is not necessary, but it never knows
   case ctr_db:update_dev_config(SrvState#devhandlerstate.dev_id,UpdatedCfg,Timestamp) of
	 
    {error,Reason} ->
       
	 % If there was an error in updating the device configuration and timestamp, return it
     {reply,{mnesia_error,Reason},SrvState};
		
	ok ->
	  
	 % Otherwise return the user the updated device's configuration and timestamp
	 {reply,{ok,{UpdatedCfg,Timestamp}},SrvState}
  
   end
 end;


%% DEBUGGING PURPOSES [TODO]: REMOVE
handle_call(_,{ReqPid,_},SrvState) ->
 io:format("[ctr_devhandler-~w]: <WARNING> Generic response issued to ReqPid = ~w~n",[SrvState#devhandlerstate.dev_id,ReqPid]),
 {reply,gen_response,SrvState}.


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
%% ACTIONS:   Push the updated device configuration and timestamp to the remote MongoDB database [TODO]: CHECK
%% NEW STATE: -
%%
handle_cast({dev_config_update,DevSrvPid,{UpdatedCfg,Timestamp}},SrvState) when DevSrvPid =:= SrvState#devhandlerstate.dev_srv_pid ->

 % Push the updated device configuration and timestamp in the 'ctr_device' table
 PushToMnesia = ctr_db:update_dev_config(SrvState#devhandlerstate.dev_id,UpdatedCfg,Timestamp),
 
 % Log the result of the operation
 %% [TODO]: Debugging purposes, remove when ready
 io:format("[ctr_devhandler-~w]: Received status update (Config = ~p, Mnesia update result = ~w)~n",[SrvState#devhandlerstate.dev_id,UpdatedCfg,PushToMnesia]),
 
 
 %% [TODO]: Push only the changed values to the Java EE Rest Server?
 
 % Keep the server state
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
 
 % Remove the monitor towards the device's 'dev_server' process, if it is still active
 demonitor(SrvState#devhandlerstate.dev_srv_mon),
 
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

 
%%====================================================================================================================================
%%                                                         START FUNCTION                                                        
%%====================================================================================================================================

%% Called by the device handlers supervisor 'sup_devhandlers' on behalf
%% of the 'ctr_pairserver' whenever a device pairs with the controller
start_link(Dev_id,DevSrvPid) ->
 gen_server:start_link(?MODULE,{Dev_id,DevSrvPid},[]).