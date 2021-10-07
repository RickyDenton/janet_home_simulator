%% This module represents the HTTP client of the JANET Controller application %%

-module(ctr_httpclient).
-behaviour(gen_server).

-export([start_link/0,init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2]).  % gen_server Behaviour Callback Functions

%% This record represents the state of a 'ctr_httpclient' gen_server
-record(httpcstate,    
        {
		 state,           % Whether the HTTP Client has established a connection with the remote server or not ('connecting'|'online')
		 conn_pid,        % The PID of the process used by Gun for connecting with the remote server
		 conn_ref,        % The reference used for monitoring the process used by Gun for connecting with the remote server
		 streams_refs,    % The list of Gun streams associated with HTTP requests pending a response
		 devup_backlog,   % The notifications backlog of devices that have paired with the controller to be sent to the remote server
         devdown_backlog  % The notifications backlog of devices that have unpaired with the controller to be sent to the remote server
		}).

% Maximum size of the backlogs used for postponing notifications to the remote server that
% devices have paired or unpaired from the controller ('devup_backlog', 'devdown_backlog')
-define(Max_backlog_size,100).  

%%====================================================================================================================================
%%                                                  GEN_SERVER CALLBACK FUNCTIONS                                                        
%%====================================================================================================================================

%% ============================================================ INIT ============================================================ %%
init(_) ->
 
 % Trap exit signals so to allow cleanup operations when terminating (terminate(Reason,SrvState) callback function)
 process_flag(trap_exit,true),
 
 % Retrieve the 'remote_rest_server_addr' and 'remote_rest_server_port' environment variables
 {ok,RemoteRESTServerAddr} = application:get_env(remote_rest_server_addr),
 {ok,RemoteRESTServerPort} = application:get_env(remote_rest_server_port),
 
 % Ensure that all dependencies of the Gun HTTP client have been started
 {ok,_GunDepsStarted} = application:ensure_all_started(gun),
 
 % Spawn the Gun process which will attempt to connect with the remote server
 {ok,ConnPid} = gun:open(RemoteRESTServerAddr,RemoteRESTServerPort),
 
 % Create a monitor towards the Gun connection process
 ConnRef = monitor(process,ConnPid),
 
 % Return the server initial state
 {ok,#httpcstate{state = 'connecting', conn_pid = ConnPid, conn_ref = ConnRef, streams_refs = [], devup_backlog = [], devdown_backlog = []}}.
 

%% ===================================================== HANDLE_CALL (STUB) ===================================================== %% 

%% This represents a STUB of the handle_call() callback function, whose
%% definition is formally required by the 'gen_server' OTP behaviour
handle_call(Request,From,SrvState) ->

 % Retrieve the controller's location ID for logging purposes
 {ok,Loc_id} = application:get_env(janet_controller,loc_id),
 
 % Report that this gen_server should not receive call requests
 io:format("[ctr_httpclient-~w]: <WARNING> Unexpected call (Request = ~p, From = ~p, SrvState = ~p)~n",[Loc_id,Request,From,SrvState]),

 % Reply with a stub message and keep the SrvState
 {reply,unsupported,SrvState}.


%% ===================================================== HANDLE_CAST (STUB) ===================================================== %% 

%% This represents a STUB of the handle_cast() callback function, whose
%% definition is formally required by the 'gen_server' OTP behaviour
handle_cast(Request,SrvState) ->
 
 % Retrieve the controller's location ID for logging purposes
 {ok,Loc_id} = application:get_env(janet_controller,loc_id),
 
 % Report that this gen_server should not receive cast requests
 io:format("[ctr_httpclient-~w]: <WARNING> Unexpected cast (Request = ~p, SrvState = ~w)~n",[Loc_id,Request,SrvState]),

 % Keep the SrvState
 {noreply,SrvState}.


%% ========================================================= HANDLE_INFO ========================================================= %%  

%% Gun connection up
handle_info({gun_up,ConnPid,_Protocol},SrvState=#httpcstate{conn_pid=ConnPid}) ->
  
 % Retrieve the controller's location ID for logging purposes
 {ok,Loc_id} = application:get_env(janet_controller,loc_id),
 
 % Report that the controller is now connected to the remote REST server
 io:format("[ctr_httpclient-~w]: Gun up!~n",[Loc_id]),
 
 % Inform the 'ctr_manager' in the JANET Simulator passing by the 'ctr_simserver'
 % that the controller is now connected with the remote REST server
 gen_server:cast(ctr_simserver,{ctr_state_update,online,self()}),
 
 %% [TODO]: Push backlogs
 
 % Update the server state
 {noreply,SrvState#httpcstate{state = 'online'}};



%% Gun connection down
handle_info({gun_down,ConnPid,_Protocol,Reason,_Killed},SrvState=#httpcstate{conn_pid=ConnPid}) ->
  
 % Retrieve the controller's location ID for logging purposes
 {ok,Loc_id} = application:get_env(janet_controller,loc_id),
 
 % Report that the controller is now connected to the remote REST server
 io:format("[ctr_httpclient-~w]: Gun Down! (reason = ~p) ~n",[Loc_id,Reason]),
 
 % Inform the 'ctr_manager' in the JANET Simulator passing by the 'ctr_simserver'
 % that the controller is no longer connected with the remote REST server
 gen_server:cast(ctr_simserver,{ctr_state_update,connecting,self()}),
 
 %% NOTES:
 %%  1) _Killed: list of streams that were not sent (but we don't care)
 %%  2) The Gun process (ConnPid) reattempts the connection automatically, it it dies -> Gun connection process down
 %%
 
 % Update the server state
 {noreply,SrvState#httpcstate{state = 'connecting'}};
 
 

%% Gun connection process down
handle_info({'DOWN',ConnRef,process,ConnPid,Reason},SrvState=#httpcstate{conn_pid = ConnPid,conn_ref=ConnRef}) ->

 % Retrieve the controller's location ID for logging purposes
 {ok,Loc_id} = application:get_env(janet_controller,loc_id),

 % Report the error depending on whether the HTTP client was connected with the remote REST server
 case SrvState#httpcstate.state of
 
  % If it was not connected
  connecting ->
   io:format("[ctr_httpclient-~w]: The remote REST server is not responding~n",[Loc_id]);
   
  % If it was
  online ->
   io:format("[ctr_httpclient-~w]: <WARNING> Gun connection process has died (reason = ~p)~n",[Loc_id,Reason])
 end,
 
 %% -------------------- Attempt to restablish the connection with the remote REST server -------------------- %% 
 
 % Retrieve the 'remote_rest_server_addr' and 'remote_rest_server_port' environment variables
 {ok,RemoteRESTServerAddr} = application:get_env(remote_rest_server_addr),
 {ok,RemoteRESTServerPort} = application:get_env(remote_rest_server_port),
 
 % Respawn the Gun connection process for re-attempting to connect with the remote REST server
 {ok,NewConnPid} = gun:open(RemoteRESTServerAddr,RemoteRESTServerPort),
 
 % Create a monitor towards the new Gun connection process
 NewConnRef = monitor(process,NewConnPid),
 
 % Return the updated server state
 %
 % NOTE: The 'devup_backlog' and 'devdown_backlog' state variables are preserved
 %
 {noreply,SrvState#httpcstate{state = 'connecting', conn_pid = NewConnPid, conn_ref = NewConnRef, streams_refs = []}}.


%% ========================================================== TERMINATE ========================================================== %% 

terminate(_,SrvState) ->

 % Stop the Gun connection process
 ok = gun:close(SrvState#httpcstate.conn_pid),
 
 % Stop the Gun application
 ok = application:stop(gun).
 
 

%%====================================================================================================================================
%%                                                    PRIVATE HELPER FUNCTIONS
%%==================================================================================================================================== 



 



%%====================================================================================================================================
%%                                                         START FUNCTION                                                        
%%==================================================================================================================================== 

%% Called by the Janet Controller top-level supervisor (sup_jctr) at boot time
start_link() ->
 gen_server:start_link({local,?MODULE},?MODULE,[],[]).  % The spawned process is also registered locally under the 'ctr_httpclient' name