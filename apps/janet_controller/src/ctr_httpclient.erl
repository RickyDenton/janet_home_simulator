%% This module represents the HTTP client of the JANET Controller application %%

-module(ctr_httpclient).
-behaviour(gen_server).

-export([start_link/0,init/1,handle_continue/2,handle_call/3,handle_cast/2,handle_info/2,terminate/2]).  % gen_server Behaviour Callback Functions

%% This record represents the state of a 'ctr_httpclient' gen_server
-record(httpcstate,    
        {
		 loc_id,            % The controller's location ID
		 conn_state,        % Whether the Controller has established a connection with the remote REST server ('connecting'|'online')
		 conn_pid,          % The PID of the Gun connection process maintaining a connection with the remote REST server
		 conn_ref,          % The reference used for monitoring the Gun connection process
		 streams_refs,      % The list of references associated with HTTP requests pending a response
		 devconn_backlog,   % The backlog of postponed device connection updates to be sent to the remote REST server (a list)
         devcfg_backlog     % The backlog of postponed device configuration updates to be sent to the remote REST server (a list)
		}).

% Maximum size of the backlogs used for postponing device connection
% and configuration updates to be sent to the remote REST serverz\
-define(Max_backlog_size,200).  

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
 
 % Ensure that all dependencies of the Gun HTTP client have been started
 {ok,_GunDepsStarted} = application:ensure_all_started(gun),
 
 % Spawn the Gun connection process for attempting to connect with the
 % remote REST server, obtaining its PID and a monitor reference towards it
 {ConnPid,ConnRef} = gun_spawn(),
 
 % Return the server initial state 
 {noreply,#httpcstate{loc_id = Loc_id, conn_state = connecting, conn_pid = ConnPid, conn_ref = ConnRef, streams_refs = [], devconn_backlog = [], devcfg_backlog = []}}.


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
%% SENDER:    A device handler ('ctr_devhandler')
%% WHEN:      When it has started (meaning that its associated device has paired with the controller)
%%            or is stopping (meaning that its associated device has unpaired from the controller)
%% PURPOSE:   Inform the remote REST server of the device connection update
%% CONTENTS:  1) The "Dev_id" of the associated device
%%            2) An atom indicating whether the device has paired ('online') or unpaired ('offline') from the controller
%% MATCHES:   - (when the request comes from the JANET Controller node)
%% ACTIONS:   A) If the controller is connected with the remote REST server, forward it the device connection update
%%            B) If the controller is NOT connected with the remote REST server,
%%               postpone the update by appending it in the 'devconn_backlog'
%% NEW STATE: A) Append the "StreamRef" associated with the HTTP request in the the 'streams_refs' list
%%            B) Update the 'devconn_backlog' state variable
%%

%% Controller ONLINE -> Push the device connection update to the remote REST server 
handle_cast({dev_conn_update,Dev_id,DevState,DevHandlerPid,Timestamp},SrvState=#httpcstate{conn_state=online,streams_refs=StreamsRefs})
                                                                      when is_integer(Dev_id), Dev_id > 0, node(DevHandlerPid) =:= node()  ->
 
 % If the controller is currently connected with the remote REST server, encode the device
 % connection update in JSON, which represents the body of the associated HTTP request
 ReqBody = devconn_to_json(Dev_id,DevState,Timestamp),
 
 % Asynchronosuly send the remote REST server the device connection update,
 % obtaining a stream reference that will be used later for validating its response
 StreamRef = gun:post(SrvState#httpcstate.conn_pid,               % PID of the Gun connection process
                      "/device",                                   % Resource path in the remote REST server
		   			  [{<<"content-type">>, "application/json"}],  % Request "Content-Type" header
                      ReqBody                                      % Request body
					 ), 
 
 % Append the new StreamRef into the 'streams_refs" state variable
 {noreply,SrvState#httpcstate{streams_refs = StreamsRefs ++ [StreamRef]}};
 
%% Controller CONNECTING -> Append the device connection update in the 'devconn_backlog' 
handle_cast({dev_conn_update,Dev_id,DevState,DevHandlerPid,Timestamp},SrvState=#httpcstate{loc_id=Loc_id,conn_state=connecting,devconn_backlog=DevStateBacklog})
                                                                      when is_integer(Dev_id), Dev_id > 0, node(DevHandlerPid) =:= node()  ->
 
 % If the controller is NOT currently connected with the remote REST
 % server, append the device connection update in the 'devconn_backlog'
 NewConnBacklog = append_to_backlog(DevStateBacklog,{Dev_id,DevState,Timestamp},Loc_id),
	
 % Update the 'devconn_backlog' state variable
 {noreply,SrvState#httpcstate{devconn_backlog = NewConnBacklog}};
	

%% Unexpected Cast
handle_cast(Request,SrvState=#httpcstate{loc_id=Loc_id}) ->
 
 % Report that this gen_server should not receive cast requests
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
handle_info({gun_up,ConnPid,_Protocol},SrvState=#httpcstate{loc_id=Loc_id,conn_state='connecting',conn_pid=ConnPid,devconn_backlog=ConnBacklog,devcfg_backlog=CfgBacklog,streams_refs=StreamsRefs}) ->
 
 % Log that the controller is now connected to the remote REST server
 %% [TODO]: Remove
 io:format("[ctr_httpclient-~w]: Gun up!~n",[Loc_id]),
 
 % Inform the 'ctr_manager' in the JANET Simulator passing by the 'ctr_simserver'
 % that the controller is now connected with the remote REST server
 gen_server:cast(ctr_simserver,{ctr_conn_update,online,self()}),
 
 % If any, push the device connection and configuration updates backlogs to the remote REST server,
 % obtaining the list of new stream references to be appended to the 'streams_refs' state variable
 NewStreamsRefs = push_backlogs(ConnBacklog,CfgBacklog),
 
 % Update the connection state to 'online', clear both backlogs, and
 % append the new stream references to the 'streams_refs' state variable
 {noreply,SrvState#httpcstate{conn_state = 'online', devconn_backlog = [], devcfg_backlog = [], streams_refs = StreamsRefs ++ NewStreamsRefs}};


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
 %% [TODO]: Remove
 io:format("[ctr_httpclient-~w]: Gun Down! (reason = ~p) ~n",[Loc_id,Reason]),
 
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
   %% [TODO]: Remove
   io:format("[ctr_httpclient-~w]: Server returned OK for request with StreamRef = ~p~n",[Loc_id,StreamRef]);

  % If it is not, log the error
  ErrorCode ->
   io:format("[ctr_httpclient-~w]: <WARNING> Server returned status \"~p\" for request with StreamRef = ~p~n",[Loc_id,ErrorCode,StreamRef])
 end,

 % Ensure the "StreamRefs" variable to not be cluttered
 %% [TODO]: Remove
 if
  length(StreamsRefs) > 20 ->
   io:format("[ctr_httpclient-~w]: <WARNING> More than 20 references pending in the 'streams_refs' list~n",[Loc_id]);
  true ->
   ok
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
 io:format("[ctr_httpclient-~w]: Gun stream-specific error (reason = ~p) ~n",[Loc_id,Reason]),

 % Remove the stream in which the error occured from the list of streams associated with HTTP requests pending a response
 {noreply,SrvState#httpcstate{streams_refs = lists:delete(StreamRef,StreamsRefs)}};

% Connection-wide error
handle_info({gun_error,ConnPid,Reason},SrvState=#httpcstate{loc_id=Loc_id,conn_pid=ConnPid}) ->

 % Log the error
 io:format("[ctr_httpclient-~w]: Gun connection-wide error (reason = ~p) ~n",[Loc_id,Reason]),

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
 %% [TODO]: Possibly remove, note that this happens periodically if the remote REST server is not reachable
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

 % Spawn the Gun connection process by passing the remote REST server as the connection targe
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


%% DESCRIPTION:  Appends a new element into a backlog used by the HTTP client, dropping its first
%%               (or oldest) element if the predefined maximum backlog size has been reached
%%
%% ARGUMENTS:    - Backlog: The backlog which to append the new element to (a list)
%%               - NewElem: The new element to be appended to the Backlog
%%               - Loc_id:  The controller's location ID (used for logging purposes
%%                          if the predefined maximum backlog size has been reached
%%
%% RETURNS:      - UpdatedBacklog -> The updated backlog (list)
%%
append_to_backlog(Backlog,NewElem,Loc_id) when is_list(Backlog), length(Backlog) =:= ?Max_backlog_size ->

 % If the predefined maximum backlog size
 % has been reached, print a warning message
 io:format("[ctr_httpclient-~w]: <WARNING> Maximum backlog size reached, the oldest update is being dropped~n",[Loc_id]),

 % Drop the first (or oldest) element in
 % the backlog and append the new element
 [_OldestElem|OtherElems] = Backlog,
 OtherElems ++ [NewElem];

append_to_backlog(Backlog,NewElem,_) when is_list(Backlog) ->

 % If the predefined maximum backlog size has not been
 % reached, simply append the new element to the backlog
 Backlog ++ [NewElem].


%% Encodes a single device connection update in JSON to be sent to the remote REST server
%%
%% NOTE: Temporary implementation
devconn_to_json(Dev_id,online,Timestamp) ->
 "[{\"dev_id\":" ++ integer_to_list(Dev_id) ++ ",\"connState\":\"online\",\"timestamp\":\"" ++ calendar:system_time_to_rfc3339(Timestamp) ++ "\"}]";
devconn_to_json(Dev_id,offline,Timestamp) ->
 "[{\"dev_id\":" ++ integer_to_list(Dev_id) ++ ",\"connState\":\"offline\",\"timestamp\":\"" ++ calendar:system_time_to_rfc3339(Timestamp) ++ "\"}]".







% Multi-state (list)
%
%% NOTE: This is currently not JSON, no commas between devstates
%
%devconn_to_json([],DevStates) ->
% "[" ++ DevStates ++ "]";
%devconn_to_json([{Dev_id,online,Timestamp}|NextDevState],DevStates) ->
% DevState = "{\"dev_id\":" ++ integer_to_list(Dev_id) ++ ",\"connState\":\"online\",\"timestamp\":\"" ++ calendar:system_time_to_rfc3339(Timestamp) ++ "\"}",
% devconn_to_json(NextDevState,DevStates ++ [DevState]);
%devconn_to_json([{Dev_id,offline,Timestamp}|NextDevState],DevStates) ->
% DevState = "{\"dev_id\":" ++ integer_to_list(Dev_id) ++ ",\"connState\":\"offline\",\"timestamp\":\"" ++ calendar:system_time_to_rfc3339(Timestamp) ++ "\"}",
% devconn_to_json(NextDevState,DevStates ++ [DevState]).

%% [TODO!]
push_backlogs(_ConnBacklog,_CfgBacklog) ->
 [].
  
% % Push the 'devstate' backlog
% DevStateBacklogStreamRef =
% if
% 
%  % There are Devstates to be pushed in the backlog
%  length(DevStateBacklog) > 0 ->
% 
%   % Encode the contents of the 'devstate' backlog in JSON
%   DevStateBacklogReqBody = devstate_to_json(DevStateBacklog,[]),
%   
%   % Send in a single HTTP request all device state updates to the remote server,
%   % obtaining a stream reference that will be used later for processing its response
%   gun:post(SrvState#httpcstate.conn_pid,               % Gun connection process PID
%            "/device",                                   % Remote server PATH%
%		    [{<<"content-type">>, "application/json"}],  % "Content-Type" header
%            DevStateBacklogReqBody                       % Request body
%		   );
%		   
%  % There are no DevStates to be pushed
%  true ->
%   []
% end,


%%====================================================================================================================================
%%                                                         START FUNCTION                                                        
%%==================================================================================================================================== 

%% Called by the Janet Controller top-level supervisor (sup_jctr) at boot time
start_link() ->
 gen_server:start_link({local,?MODULE},?MODULE,[],[]).  % The spawned process is also registered locally under the 'ctr_httpclient' name