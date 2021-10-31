%% This module represents the HTTP client of the JANET Controller application %%

-module(ctr_httpclient).
-behaviour(gen_server).

-export([start_link/0,init/1,handle_continue/2,handle_call/3,handle_cast/2,handle_info/2,terminate/2]).  % gen_server Behaviour Callback Functions

%% This record represents the state of a 'ctr_httpclient' gen_server
-record(httpcstate,    
        {
		 loc_id,            % The controller's location ID
		 loc_user,          % The user the location belongs to [REMOTE SERVER COMPATIBILITY]
		 conn_state,        % Whether the Controller has established a connection with the remote REST server ('connecting'|'online')
		 conn_pid,          % The PID of the Gun connection process maintaining a connection with the remote REST server
		 conn_ref,          % The reference used for monitoring the Gun connection process
		 rest_path,         % The remote REST server path where to send device state and connectivity updates
		 streams_refs,      % The list of references associated with HTTP requests pending a response
		 devconn_backlog,   % The backlog of postponed device connection updates to be sent to the remote REST server (a list)
         devcfg_backlog     % The backlog of postponed device configuration updates to be sent to the remote REST server (a list)
		}).

% Maximum size of the backlogs used for postponing device connection
% and configuration updates to be sent to the remote REST server
-define(Max_backlog_size,300).      % Default: 300

% Initial delay before attempting to connect to the remote REST server so to
% collect the initial device connection and configuration updates in their
% respective backlogs, allowing them to be sent in single HTTP requests
-define(Init_conn_delay,2 * 1000).  % Default: 2* 1000 

%%====================================================================================================================================
%%                                                  GEN_SERVER CALLBACK FUNCTIONS                                                        
%%====================================================================================================================================

%% ============================================================ INIT ============================================================ %%
init(_) ->
 
 % Trap exit signals so to allow cleanup operations when terminating (terminate(Reason,SrvState) callback function)
 process_flag(trap_exit,true),
 
 % Return a stub server initial state, where further initializations will continue in
 % the "handle_continue(init,State)" callback function for parallelization purposes  
 {ok,#httpcstate{conn_state = connecting, _ = none},{continue,init}}.
 
 
%% ======================================================= HANDLE_CONTINUE ======================================================= %%

%% Initializes the controller HTTP client (called right after the 'init' callback function)
handle_continue(init,_SrvState) -> 
 
 % Retrieve the controller's location ID for logging purposes
 {ok,Loc_id} = application:get_env(janet_controller,loc_id),
 
  % Retrieve the user the location belongs to [REMOTE SERVER COMPATIBILITY]
 {ok,Loc_user} = application:get_env(janet_controller,loc_user),

 % Retrieve the remote REST server path
 {ok,RemoteRESTServerPath} = application:get_env(janet_controller,remote_rest_server_path),

 % Ensure that all dependencies of the Gun HTTP client have been started
 {ok,_GunDepsStarted} = application:ensure_all_started(gun),
 
 % Await for a predefined time before attempting to connect with the remote REST
 % server so to collect the initial device connection and configuration updates
 % in their respective backlogs, allowing them to be sent in single HTTP requests
 timer:sleep(?Init_conn_delay),
 
 % Spawn the Gun connection process for attempting to connect with the
 % remote REST server, obtaining its PID and a monitor reference towards it
 {ConnPid,ConnRef} = gun_spawn(),
 
 % Return the server initial state 
 {noreply,#httpcstate{loc_id = Loc_id, loc_user = Loc_user, conn_state = connecting, conn_pid = ConnPid, conn_ref = ConnRef, rest_path = RemoteRESTServerPath, streams_refs = [], devconn_backlog = [], devcfg_backlog = []}}.


%% ===================================================== HANDLE_CALL (STUB) ===================================================== %% 

%% This represents a STUB of the handle_call() callback function, whose
%% definition is formally required by the 'gen_server' OTP behaviour
handle_call(Request,From,SrvState=#httpcstate{loc_id=Loc_id}) ->
 
 % Report that this gen_server should not receive call requests
 io:format("[ctr_httpclient-~w]: <WARNING> Unexpected call (Request = ~p, From = ~p, SrvState = ~p)~n",[Loc_id,Request,From,SrvState]),

 % Reply with a stub message and keep the SrvState
 {reply,unsupported,SrvState}.


%% ========================================================= HANDLE_CAST ========================================================= %% 

%% DEV_CONN_UPDATE
%% ---------------
%% SENDER:    One of the controller's device handlers ('ctr_devhandler')
%% WHEN:      When it has started (meaning that its associated device has paired with the controller)
%%            or is stopping (meaning that its associated device has unpaired from the controller)
%% PURPOSE:   Inform the remote REST server of the device connection update
%% CONTENTS:  1) The "Dev_id" of the associated device
%%            2) An atom indicating whether the device has paired ('online') or unpaired ('offline') from the controller
%%            3) The PID of the device handler process ("security purposes")
%%            4) The timestamp when the device paired or unpaired from the controller
%% MATCHES:   - (when the request comes from the JANET Controller node)
%% ACTIONS:   A) If the controller is connected with the remote REST
%%               server, directly forward it the device connection update
%%            B) If the controller is NOT connected with the remote REST server,
%%               postpone the update by appending it in the 'devconn_backlog'
%% NEW STATE: A) Append the "DevConnStreamRef" associated with the HTTP request in the the 'streams_refs' list
%%            B) Update the 'devconn_backlog' state variable
%%

%% Controller ONLINE -> Push the device connection update to the remote REST server 
handle_cast({dev_conn_update,Dev_id,DevConnState,DevHandlerPid,Timestamp},SrvState=#httpcstate{loc_id=Loc_id,loc_user=Loc_user,conn_state=online,conn_pid=ConnPid,rest_path=RESTPath,streams_refs=StreamsRefs})
                                                                                               when is_integer(Dev_id), Dev_id > 0, node(DevHandlerPid) =:= node()  ->
 
 % Attempt to send the device connection update, obtaining the stream
 % reference associated with the HTTP request enclosed in a list
 DevConnStreamRef = send_devconn_updates([{Dev_id,DevConnState,Timestamp}],ConnPid,Loc_user,Loc_id,RESTPath),
 
 % Append the "DevConnStreamRef" into the 'streams_refs" state variable
 {noreply,SrvState#httpcstate{streams_refs = StreamsRefs ++ DevConnStreamRef}};
 

%% Controller CONNECTING -> Append the device connection update in the 'devconn_backlog' 
handle_cast({dev_conn_update,Dev_id,DevConnState,DevHandlerPid,Timestamp},SrvState=#httpcstate{loc_id=Loc_id,conn_state=connecting,devconn_backlog=DevConnBacklog})
                                                                             when is_integer(Dev_id), Dev_id > 0, node(DevHandlerPid) =:= node()  ->
 
 % If the controller is NOT currently connected with the remote REST
 % server, append the device connection update in the 'devconn_backlog'
 NewConnBacklog = append_to_backlog(DevConnBacklog,[{Dev_id,DevConnState,Timestamp}],Loc_id,"connection"),
	
 % Update the 'devconn_backlog' state variable
 {noreply,SrvState#httpcstate{devconn_backlog = NewConnBacklog}};
	
	
%% DEV_CONFIG_UPDATE
%% -----------------
%% SENDER:    One of the controller's device handlers ('ctr_devhandler')
%% WHEN:      When it has received a list of configuration updates of
%%            its associated device to be sent to the remote REST server
%% PURPOSE:   Send the list of device configuration updates to the remote REST server
%% CONTENTS:  1) The list of device configuration updates to be sent to the remote REST server
%%            2) The PID of the device handler process ("security purposes")
%% MATCHES:   - (when the request comes from the JANET Controller node)
%% ACTIONS:   A) If the controller is connected with the remote REST
%%               server, directly forward it the device configuration update
%%            B) If the controller is NOT connected with the remote REST server,
%%               postpone the update by appending it in the 'devcfg_backlog'
%% NEW STATE: A) Append the "DevCfgStreamRef" associated with the HTTP request in the the 'streams_refs' list
%%            B) Update the 'devcfg_backlog' state variable
%%

%% Controller ONLINE -> Push the device configuration update to the remote REST server 
handle_cast({dev_config_update,DevCfgUpdates,DevHandlerPid},SrvState=#httpcstate{loc_id=Loc_id,loc_user=Loc_user,conn_state=online,conn_pid=ConnPid,rest_path=RESTPath,streams_refs=StreamsRefs})
                                                                                  when is_list(DevCfgUpdates), node(DevHandlerPid) =:= node() ->
 
 % Attempt to send the device configuration update, obtaining the
 % stream reference associated with the HTTP request enclosed in a list
 DevCfgStreamRef = send_devcfg_updates(DevCfgUpdates,ConnPid,Loc_user,Loc_id,RESTPath),
 
 % Append the "DevCfgStreamRef" into the 'streams_refs" state variable
 {noreply,SrvState#httpcstate{streams_refs = StreamsRefs ++ DevCfgStreamRef}};
 

%% Controller CONNECTING -> Append the device configuration update in the 'devcfg_backlog' 
handle_cast({dev_config_update,DevCfgUpdates,DevHandlerPid},SrvState=#httpcstate{loc_id=Loc_id,conn_state=connecting,devcfg_backlog=DevCfgBacklog})
                                                                                  when node(DevHandlerPid) =:= node()  ->
 % If the controller is NOT currently connected with the remote REST
 % server, append the device configuration update in the 'devcfg_backlog'
 NewDevCfgBacklog = append_to_backlog(DevCfgBacklog,DevCfgUpdates,Loc_id,"configuration"),
	
 % Update the 'devcfg_backlog' state variable
 {noreply,SrvState#httpcstate{devcfg_backlog = NewDevCfgBacklog}};	
	

%% Unexpected Cast
handle_cast(Request,SrvState=#httpcstate{loc_id=Loc_id}) ->
 
 % Report that an unexpected cast was received by this gen_server
 io:format("[ctr_httpclient-~w]: <WARNING> Unexpected cast (Request = ~p, SrvState = ~w)~n",[Loc_id,Request,SrvState]),

 % Keep the SrvState
 {noreply,SrvState}.


%% ========================================================= HANDLE_INFO ========================================================= %%  

%% GUN UP
%% ------
%% SENDER:    The Gun connection process associated with the controller HTTP client
%% WHEN:      When it has successfully established a connection with the remote REST server
%% PURPOSE:   Inform the HTTP client that the controller is now connected with the remote REST server
%% CONTENTS:  1) The PID of the Gun connection process
%%            2) The protocol used in the connection (HTTP/1.1, unused variable)
%% MATCHES:   When the HTTP client is connecting with the remote REST
%%            server (and the request comes from the JANET Controller node)
%% ACTIONS:   1) Inform the 'ctr_manager' in the JANET Simulator passing by the 'ctr_simserver'
%%               that the controller is now connected with the remote REST server
%%            2) If there are postponed device or configuration updates in their respective
%%               backlogs ('devconn_backlog','devcfg_backlog'), push them to the remote REST server
%% NEW STATE: Update the 'conn_state' to 'online', clear both backlogs, and append the StreamRefs
%%            associated with sending thebacklogs, if any, to the 'streams_refs' state variable
%%
handle_info({gun_up,ConnPid,_Protocol},SrvState=#httpcstate{loc_id=Loc_id,loc_user=Loc_user,conn_state='connecting',conn_pid=ConnPid,rest_path=RESTPath,devconn_backlog=ConnBacklog,devcfg_backlog=CfgBacklog,streams_refs=StreamsRefs}) ->
 
 % Log that the controller is now connected to the remote REST server
 %% [TODO]: Remove when ready?
 io:format("[ctr_httpclient-~w]: Uplink established~n",[Loc_id]),
 
 % Inform the 'ctr_manager' in the JANET Simulator passing by the 'ctr_simserver'
 % that the controller is now connected with the remote REST server
 gen_server:cast(ctr_simserver,{ctr_conn_update,online,self()}),
 
 % Send the backlogs of device connection and configuration updates to the remote REST server,
 % obtaining the stream references associated with their HTTP requests enclosed in lists
 %
 % NOTE: If a backlog is empty, an empty list is returned
 %
 ConnBacklogRef = send_devconn_updates(ConnBacklog,ConnPid,Loc_user,Loc_id,RESTPath),
 CfgBacklogRef = send_devcfg_updates(CfgBacklog,ConnPid,Loc_user,Loc_id,RESTPath),

 % Update the connection state to 'online', clear both backlogs, and
 % append their stream references to the 'streams_refs' state variable
 {noreply,SrvState#httpcstate{conn_state = 'online', devconn_backlog = [], devcfg_backlog = [], streams_refs = StreamsRefs ++ ConnBacklogRef ++ CfgBacklogRef}};


%% GUN DOWN
%% --------
%% SENDER:    The Gun connection process associated with the controller HTTP client
%% WHEN:      When it has disconnected from the remote REST server
%% PURPOSE:   Inform the HTTP client that the controller is
%%            no longer connected with the remote REST server
%% CONTENTS:  1) The PID of the Gun connection process
%%            2) The protocol used in the connection (HTTP/1.1, unused variable)
%%            3) The disconnection reason
%%            4) The list of streams that were killed before completion (unused
%%               variable, their associated responses are considered lost)
%% MATCHES:   When the HTTP client is connected with the remote REST server
%%            (and the request comes from the JANET Controller node)
%% ACTIONS:   Inform the 'ctr_manager' in the JANET Simulator passing by the 'ctr_simserver'
%%            that the controller is no longer connected with the remote REST server
%% NEW STATE: Update the 'conn_state' to 'connecting' and clear the 'streams_refs' variable
%%
%% NOTE:      No explicit reconnection attempt is necessary since the Gun connection process
%%            will periodically attempt to reconnect with the remote REST server, sending
%%            a 'gun_up' message when it succeeds
%%
handle_info({gun_down,ConnPid,_Protocol,Reason,_KilledStreams},SrvState=#httpcstate{loc_id=Loc_id,conn_state=online,conn_pid=ConnPid}) ->
 
 % Report that the controller is no longer connected with the remote REST server
 %% [TODO]: Remove when ready?
 io:format("[ctr_httpclient-~w]: Uplink down (reason = ~p)~n",[Loc_id,Reason]),
 
 % Inform the 'ctr_manager' in the JANET Simulator passing by the 'ctr_simserver'
 % that the controller is no longer connected with the remote REST server
 gen_server:cast(ctr_simserver,{ctr_conn_update,connecting,self()}),
 
 %% NOTE: Since the controller was previously connected with the remote REST
 %%       server the backlogs 'devconn_backlog' and 'devcfg_backlog' are surely
 %%       empty at this point, and so do not require to be explicitly updated
 
 % Update the 'conn_state' to 'connecting' and clear the 'streams_refs' variable
 {noreply,SrvState#httpcstate{conn_state ='connecting', streams_refs = []}};
 

%% GUN RESPONSE
%% ------------
%% SENDER:    The Gun connection process associated with the controller HTTP client
%% WHEN:      When it receives a response from the remote REST
%%            server associated with a previous HTTP request
%% PURPOSE:   Validate the response returned by the remote REST server
%% CONTENTS:  1) The PID of the Gun connection process
%%            2) The request reference the response is associated to
%%            3) Whether there are additional data to be received in the response (unused variable)
%%            4) The HTTP response status
%%            5) The HTTP response headers (unused variable)
%% MATCHES:   (always)
%% ACTIONS:   If the HTTP response status is not 200 (OK), log the error
%% NEW STATE: Remove the response reference from the list of stream
%%            references associated with HTTP requests pending a response
%%
handle_info({gun_response,ConnPid,StreamRef,_Fin,Status,_Headers},SrvState=#httpcstate{loc_id=Loc_id,conn_pid=ConnPid,streams_refs=StreamsRefs}) ->

 % Ensure the HTTP response status to be 200 (OK)
 case Status of
 
  % If it is, do nothing
  200 ->
   ok;
   
  % If it is not, log the error
  ErrorCode ->
   io:format("[ctr_httpclient-~w]: <WARNING> Server returned status \"~p\" for request with StreamRef = ~p~n",[Loc_id,ErrorCode,StreamRef])
 end,

 % Remove the response reference from the 'streams_refs' state variable
 {noreply,SrvState#httpcstate{streams_refs = lists:delete(StreamRef,StreamsRefs)}};


%% GUN ERROR
%% ---------
%% SENDER:    The Gun connection process associated with the controller HTTP client
%% WHEN:      When an error occurs at a connection-wide or stream-specific level
%% PURPOSE:   Inform the HTTP client of the error
%% CONTENTS:  1) The PID of the Gun connection process
%%            2) (if stream-specific error) The reference associated with the stream in which the error occured
%%            3) The error reason
%% MATCHES:   (always)
%% ACTIONS:   Report the error
%% NEW STATE: If stream-specific error, remove such stream from the list
%%            of streams associated with HTTP requests pending a response
%%

% Stream-specific error
handle_info({gun_error,ConnPid,StreamRef,Reason},SrvState=#httpcstate{loc_id=Loc_id,conn_pid=ConnPid,streams_refs=StreamsRefs}) ->

 % Log the error
 io:format("[ctr_httpclient-~w]: <ERROR> Gun stream-specific error (reason = ~p) ~n",[Loc_id,Reason]),

 % Remove the stream in which the error occured from the list of streams associated with HTTP requests pending a response
 {noreply,SrvState#httpcstate{streams_refs = lists:delete(StreamRef,StreamsRefs)}};

% Connection-wide error
handle_info({gun_error,ConnPid,Reason},SrvState=#httpcstate{loc_id=Loc_id,conn_pid=ConnPid}) ->

 % Log the error
 io:format("[ctr_httpclient-~w]: <ERROR> Gun connection-wide error (reason = ~p) ~n",[Loc_id,Reason]),

 % Keep the server state
 {noreply,SrvState};


%% GUN CONNECTION PROCESS DOWN
%% ---------------------------
%% SENDER:    The Erlang Run-Time System (ERTS)
%% WHEN:      When the monitored Gun connection process terminates
%% PURPOSE:   Inform of the Gun connection process termination (and consequently,
%%            that the controller is not connected with the remote REST server)
%% CONTENTS:  1) The monitor reference the notification refers to
%%            2) The PID of the process that was monitored (the Gun connection process)
%%            3) The reason for the monitored process's termination
%% MATCHES:   (always) (when the monitor reference and the PID of the
%%            monitored process match the ones in the server's state)
%% ACTIONS:   1) If the controller was previously connected with the remote REST
%%               server, inform the 'ctr_manager' in the JANET Simulator passing
%%               by the 'ctr_simserver' that the controller is no longer connected
%%            2) Respawn the Gun connection process for attempting to
%%               re-establish a connection with the remote REST server
%% NEW STATE: Set the 'conn_state' to 'connecting', update the 'conn_pid' and
%%            'conn_ref' to their new values, and reset the 'streams_refs' variable
%% 
handle_info({'DOWN',ConnRef,process,ConnPid,Reason},SrvState=#httpcstate{loc_id=Loc_id,conn_state=ConnState,conn_pid=ConnPid,conn_ref=ConnRef}) ->

 % Report that the Gun connection process has terminated
 io:format("[ctr_httpclient-~w]: The Gun connection process has terminated (reason = ~p)~n",[Loc_id,Reason]),
 
 % Depending on whether the controller was previously connected with the remote REST server
 case ConnState of
 
  % If it was, inform the 'ctr_manager' in the JANET Simulator passing
  % by the 'ctr_simserver' that the controller is no longer connected
  online ->
   gen_server:cast(ctr_simserver,{ctr_conn_update,connecting,self()});
   
  % If it was not connected in the first place, do nothing
  connecting ->
   ok
 end,
 
 % Respawn the Gun connection process for attempting to re-connect with the
 % remote REST server, obtaining its PID and a monitor reference towards it
 {NewConnPid,NewConnRef} = gun_spawn(),

 % Set the 'conn_state' to 'connecting', update the 'conn_pid' and
 % 'conn_ref' to their new values, and reset the 'streams_refs' variable
 %
 % NOTE: The device connection and configuration updates backlogs are preserved
 %
 {noreply,SrvState#httpcstate{conn_state = 'connecting', conn_pid = NewConnPid, conn_ref = NewConnRef, streams_refs = []}}.


%% ========================================================== TERMINATE ========================================================== %% 

%% Server termination when the Gun connection process is alive
terminate(_,_SrvState=#httpcstate{conn_pid=ConnPid}) when is_pid(ConnPid)->

 %% [TODO]: Send controller stop message here?
 
 % Stop the Gun connection process
 ok = gun:close(ConnPid),
 
 % Stop the Gun application
 ok = application:stop(gun);
 
%% Server termination when the Gun connection process is NOT alive
terminate(_,_SrvState) ->
 
 % Ensure the Gun application to be stopped
 application:stop(gun),
 
 % Terminate
 ok.


%%====================================================================================================================================
%%                                                    PRIVATE HELPER FUNCTIONS
%%==================================================================================================================================== 

%% DESCRIPTION:  Spawns the Gun connection process for attempting to connect with the
%%               remote REST server and returns its PID and a monitor reference towards it
%%
%% ARGUMENTS:    none
%%
%% RETURNS:      - {ConnPid,ConnRef} -> The PID of the Gun connection process and a monitor reference towards it
%%
gun_spawn() ->

 % Retrieve the 'remote_rest_server_addr' and 'remote_rest_server_port' environment variables
 {ok,RemoteRESTServerAddr} = application:get_env(remote_rest_server_addr),
 {ok,RemoteRESTServerPort} = application:get_env(remote_rest_server_port),

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


%% DESCRIPTION:  Appends a list of new elements into a backlog used by the HTTP client, dropping its
%%               first (or oldest) elements if the predefined maximum backlog size has been reached
%%
%% ARGUMENTS:    - Backlog:     The backlog which to append the new elements to (a list)
%%               - NewElems:    The ne elements to be appended to the backlog (a list)
%%               - Loc_id:      The controller's location ID (logging purposes)
%%               - BacklogName: The backlog name (a list, logging purposes) 
%%
%% RETURNS:      - UpdatedBacklog -> The updated backlog (a list)
%%
append_to_backlog(Backlog,NewElems,Loc_id,BacklogName) when is_list(Backlog), is_list(NewElems), is_list(BacklogName) ->

 % Retrieve the length of the list of new elements to be appended in the backlog
 NewElemsLength = length(NewElems),
 
 % Perform a set of checks, obtaining the updated backlog to be returned and the following two flags:
 %
 % NewElemsExceedSize -> Whether the list of new elements is longer than the predefined maximum backlog size
 % OldElemsDropped    -> Whether existing elements were dropped from the backlog
 %
 {UpdatedBacklog,NewElemsExceedSize,OldElemsDropped} =
 if
  NewElemsLength > ?Max_backlog_size ->
  
   % If the list of new elements is longer than the predefined
   % maximum backlog size, set the "NewElemsExceedSize" flag
   NewElemsExceedSize_ = true,
   
   % In this instance any existing elements in the backlog are
   % dropped, and so if there are any se the "OldElemsDropped" flag
   if
    length(Backlog) > 0 ->
	 OldElemsDropped_ = true;
	true ->
	 OldElemsDropped_ = false
   end,
   
   % Set the updated backlog as the last "?Max_backlog_size" new elements
   UpdatedBacklog_ = lists:nthtail(NewElemsLength-?Max_backlog_size,NewElems),
   
   % Return the required tuple
   {UpdatedBacklog_,NewElemsExceedSize_,OldElemsDropped_};
   
  true ->
  
   % Otherwise if the list of new elements is not longer than the
   % predefined maximum backlog size, clear the "NewElemsExceedSize" flag
   NewElemsExceedSize_ = false,
   
   % Determine the length of the list that would be obtained by
   % concatenating the new elements with the current backlog contents 
   CumulativeLength = NewElemsLength + length(Backlog),
   
   % Depending on whether such cumulative length
   % is greater than the maximum backlog size
   if
    CumulativeLength > ?Max_backlog_size ->
	
	 % If it is, set the "OldElemsDropped" flag
	 OldElemsDropped_ = true,
	 
	 % Set the updated backlog as its last (CumulativeLength - ?Max_backlog_size)
	 % current elements concatenated with the new elements
	 UpdatedBacklog_ = lists:nthtail(CumulativeLength-?Max_backlog_size,Backlog) ++ NewElems,
	  
	 % Return the required tuple
     {UpdatedBacklog_,NewElemsExceedSize_,OldElemsDropped_};
   
    true -> 
	  
	 % If instead the cumulative length is smaller than the
	 % maximum backlog size, clear the "OldElemsDropped" flag
     OldElemsDropped_ = false,
	 
	 % Set the updated backlog as the concatenation of
	 % its current contents with the list of new elements
     UpdatedBacklog_ = Backlog ++ NewElems,
	 
	 % Return the required tuple
     {UpdatedBacklog_,NewElemsExceedSize_,OldElemsDropped_}
   end
 end,
 
 % If any of the two previous flags is set, print a warning message 
 case {NewElemsExceedSize,OldElemsDropped} of
 
  % If both are set
  {true,true} ->
   io:format("[ctr_httpclient-~w]: <WARNING> Size of new device ~s updates exceeds maximum backlog size (~w > ~w), older updates as well as the current backlog contents are being dropped~n",[Loc_id,BacklogName,NewElemsLength,?Max_backlog_size]);
 
  % If olny the "NewElemsExceedSize" flag is set
  {true,false} ->
   io:format("[ctr_httpclient-~w]: <WARNING> Size of new device ~s updates exceeds maximum backlog size (~w > ~w), older updates are being dropped~n",[Loc_id,BacklogName,NewElemsLength,?Max_backlog_size]);
 
  % If olny the "OldElemsDropped" flag is set
  {false,true} ->
   io:format("[ctr_httpclient-~w]: <WARNING> The maximum size of the device ~s updates backlog has been reached (~w), older updates are being dropped~n",[Loc_id,BacklogName,?Max_backlog_size]);
 
  % If no flags are set, do nothing
  {false,false} ->
   ok
 end,
 
 % Return the updated backlog
 UpdatedBacklog.
 

%% DESCRIPTION:  Sends a list of device connection updates to the remote REST server,
%%               returning the stream reference associated with the HTTP request
%%
%% ARGUMENTS:    - DevConnUpdates: The list of device connection updates [{Dev_id,DevConnState,Timestamp}]
%%               - ConnPid:        The PID of the Gun connection process
%%               - Loc_user:       The user the location belongs to [REMOTE SERVER COMPATIBILITY]
%%               - Loc_id:         The controller's location ID (logging purposes)
%%               - RESTPath:       The remote REST server path where to send the device connectivity updates
%%
%% RETURNS:      - [DevConnStreamRef] -> The stream reference associated with the HTTP request of sending
%%                                       the list of device connection updates enclosed in a list
%%               - []                 -> If an empty DevConnUpdates list was passed
%% 
send_devconn_updates([],_ConnPid,_Loc_user,_Loc_id,_RESTPath) ->

 % If there are no device connection updates
 % to be sent, simply return an empty list
 [];
 
send_devconn_updates(DevConnUpdates,ConnPid,Loc_user,Loc_id,RESTPath) when is_list(DevConnUpdates), is_pid(ConnPid),
                                                                           is_list(Loc_user), is_integer(Loc_id), Loc_id > 0 ->

 % Attempt to encode the device connection updates in JSON format
 case devconns_to_json(DevConnUpdates,Loc_user) of
  jsone_encode_error ->
   
   % If the encoding failed, report the error and drop the device connection updates
   io:format("[ctr_httpclient-~w]: <ERROR> Device connection updates could not be encoded in JSON (DevConnUpdates = ~p, Loc_user = ~p), dropping the updates~n",[Loc_id,DevConnUpdates,Loc_user]),
   
   % Return an empty list
   [];
   
  ReqBody ->
  
   % Otherwise if the encoding was successful, asynchronosuly send the remote REST server the
   % device connection updates, obtaining the stream reference associated with the HTTP request
   DevConnStreamRef = gun:post(
                               ConnPid,                                     % PID of the Gun connection process
                               RESTPath,                                    % Resource path in the remote REST server
		     			       [{<<"content-type">>, "application/json"}],  % Request "Content-Type" header
                               ReqBody                                      % Request body
			  	  	          ), 
 
   % Return the stream reference enclosed in a list
   [DevConnStreamRef]
 end;

send_devconn_updates(DevConnUpdates,ConnPid,Loc_user,Loc_id,RESTPath) ->

 % If invalid arguments were passed, report the error
 io:format("[ctr_httpclient-~w]: <FATAL> Invalid arguments passed to 'send_devconn_updates' (DevConnUpdates = ~p, ConnPid = ~p, Loc_user = ~p, Loc_id = ~p, RESTPath = ~p), dropping the updates~n",[Loc_id,DevConnUpdates,ConnPid,Loc_user,Loc_id,RESTPath]),
 
 % Return an empty list
 [].
 
%% Encodes a list of device connection updates in JSON format
%% (send_devconn_updates(DevConnUpdates,ConnPid,Loc_user,Loc_id) helper function)
devconns_to_json(DevConnUpdates,Loc_user) ->
 
 % Convert the list of device connection updates into a list of map
 % connection updates as required by the remote REST server interface
 %
 % [REMOTE SERVER COMPATIBILITY]: The "user" is inserted in each device connection
 %                                update instead of being a message-wide attribute  
 %
 DevConnsReqs = [ #{user => list_to_binary(Loc_user), dev_id => Dev_id, actions => #{connectivity => DevConnState}, timestamp => utils:timestamp_to_binary(Timestamp)} || {Dev_id,DevConnState,Timestamp} <- DevConnUpdates],

 % Attempt to encode the list of device connection updates in JSON
 try jsone:encode(DevConnsReqs)
 catch
	
  % If the encoding was unsuccessful, return an error
  error:badarg ->
   jsone_encode_error
 end.


%% DESCRIPTION:  Sends a list of device configuration updates to the remote REST server,
%%               returning the stream reference associated with the HTTP request
%%
%% ARGUMENTS:    - DevCfgUpdates:  The list of device configuration updates [{Dev_id,UpdatedCfgMap,Timestamp}]
%%               - ConnPid:        The PID of the Gun connection process
%%               - Loc_user:       The user the location belongs to [REMOTE SERVER COMPATIBILITY]
%%               - Loc_id:         The controller's location ID (logging purposes)
%%               - RESTPath:       The remote REST server path where to send the device configuration updates
%%
%% RETURNS:      - [DevCfgStreamRef] -> The stream reference associated with the HTTP request of sending
%%                                      the list of device configuration updates enclosed in a list
%%               - []                -> If an empty DevCfgUpdates list was passed
%% 
send_devcfg_updates([],_ConnPid,_Loc_user,_Loc_id,_RESTPath) ->

 % If there are no device configuration updates
 % to be sent, simply return an empty list
 [];
 
send_devcfg_updates(DevCfgUpdates,ConnPid,Loc_user,Loc_id,RESTPath) when is_list(DevCfgUpdates), is_pid(ConnPid), is_list(Loc_user),
                                                                         is_integer(Loc_id), Loc_id > 0, is_list(RESTPath) ->

 % Attempt to encode the device configuration updates in JSON format
 case devcfgs_to_json(DevCfgUpdates,Loc_user) of
  jsone_encode_error ->
   
   % If the encoding failed, report the error and drop the device configuration updates
   io:format("[ctr_httpclient-~w]: <ERROR> Device configuration updates could not be encoded in JSON (DevCfgUpdates = ~p, Loc_user = ~p), dropping the updates~n",[Loc_id,DevCfgUpdates,Loc_user]),
   
   % Return an empty list
   [];
   
  ReqBody ->
  
   % Otherwise if the encoding was successful, asynchronosuly send the remote REST server the
   % device configuration updates, obtaining the stream reference associated with the HTTP request
   DevCfgStreamRef = gun:patch(
                               ConnPid,                                     % PID of the Gun connection process
                               RESTPath,                                    % Resource path in the remote REST server
		     			       [{<<"content-type">>, "application/json"}],  % Request "Content-Type" header
                               ReqBody                                      % Request body
			  	  	          ), 
 
   % Return the stream reference enclosed in a list
   [DevCfgStreamRef]
 end;

send_devcfg_updates(DevCfgUpdates,ConnPid,Loc_user,Loc_id,RESTPath) ->

 % If invalid arguments were passed, report the error
 io:format("[ctr_httpclient-~w]: <FATAL> Invalid arguments passed to 'send_devcfg_updates' (DevCfgUpdates = ~p, ConnPid = ~p, Loc_user = ~p, Loc_id = ~p, RESTPath = ~p), dropping the updates~n",[Loc_id,DevCfgUpdates,ConnPid,Loc_user,Loc_id,RESTPath]),
 
 % Return an empty list
 [].
 
%% Encodes a list of device connection updates in JSON format
%% (send_devcfg_updates(DevCfgUpdates,ConnPid,Loc_user,Loc_id) helper function)
devcfgs_to_json(DevCfgUpdates,Loc_user) ->
 
 % Convert the list of device configuration updates into a list of map
 % configuration updates as required by the remote REST server interface
 %
 % [REMOTE SERVER COMPATIBILITY]: The "user" is inserted in each device connection
 %                                update instead of being a message-wide attribute  
 %
 DevCfgsReqs = [ #{user => list_to_binary(Loc_user), dev_id => Dev_id, actions => UpdatedCfgMap, timestamp => utils:timestamp_to_binary(Timestamp)} || {Dev_id,UpdatedCfgMap,Timestamp} <- DevCfgUpdates],

 % Attempt to encode the list of device configuration updates in JSON
 try jsone:encode(DevCfgsReqs)
 catch
	
  % If the encoding was unsuccessful, return an error
  error:badarg ->
   jsone_encode_error
 end.
 
 
%%====================================================================================================================================
%%                                                         START FUNCTION                                                        
%%==================================================================================================================================== 

%% Called by the Janet Controller top-level supervisor (sup_jctr) at boot time
start_link() ->
 gen_server:start_link({local,?MODULE},?MODULE,[],[]).  % The spawned process is also registered locally under the 'ctr_httpclient' name