%% This module represents the server in the JANET Controller application used for interfacing with the JANET simulator node %%

-module(ctr_simserver).
-behaviour(gen_server).

-export([start_link/0,init/1,handle_call/3,handle_cast/2,handle_continue/2]).  % gen_server Behaviour Callback Functions

%% This record represents the state of a 'ctr_simserver' gen_server
-record(simsrvstate,    
        {
		 ctr_state,  % The controller state ('booting'|'connecting'|'online'), which once booted mirrors the 'ctr_httpclient' 'conn_state'
		 mgr_pid     % The PID of the 'ctr_manager' process in the JANET Simulator associated with the controller
		}).

%%====================================================================================================================================
%%                                                  GEN_SERVER CALLBACK FUNCTIONS                                                        
%%====================================================================================================================================

%% ============================================================ INIT ============================================================ %%
init(_) ->
 
 % Return the server initial state, where further initialization operations will be performed in the "handle_continue(Continue,State)"
 % callback function for parallelization purposes (and for allowing the ctr_manager process to respond to the registration request)  
 {ok,#simsrvstate{ctr_state = booting, mgr_pid = none},{continue,init}}.
 
 
%% ======================================================= HANDLE_CONTINUE ======================================================= %%  

%% Registers the JANET controller node with its controller manager in the JANET simulator node by passing the PID of the ctr_simserver
handle_continue(init,_SrvState) ->

 % Retrieve the 'mgr_pid' environment variable
 {ok,MgrPid} = application:get_env(mgr_pid),
 
 % Attempt to register the JANET controller node with its controller
 % manager in the simulator node by passing the PID of the ctr_simserver
 %
 % NOTE: This registration request is performed synchronously since
 %       the controller node's execution cannot continue if it fails 
 ok = gen_server:call(MgrPid,{ctr_reg,self()},10000),
 
 % Return the server state
 {noreply,#simsrvstate{ctr_state = connecting, mgr_pid = MgrPid}}.
 

%% ========================================================= HANDLE_CALL ========================================================= %%

%% CTR_COMMAND
%% -----------
%% SENDER:    The controller node's manager in the JANET Simulator node
%% WHEN:      When the JANET Simulator is executing a function requiring synchronization in the Controller node
%% PURPOSE:   Execute a command on the controller node and return the result of the operation
%% CONTENTS:  The Module, Function and ArgsList to be evaluated via apply()
%% MATCHES:   (always) (when the requests comes from the controller manager in the JANET Simulator)
%% ACTIONS:   Execute the required command via apply()
%% ANSWER:    The result of the apply() function
%% NEW STATE: -
%%
handle_call({ctr_command,Module,Function,ArgsList},{MgrPid,_},SrvState=#simsrvstate{mgr_pid=MgrPid}) ->

 % Execute the required command and return its result
 {reply,apply(Module,Function,ArgsList),SrvState};


%% SIM_COMMAND
%% -----------
%% SENDER:    The controller's REST handler ('ctr_resthandler')
%% WHEN:      A remote command is received requiring a function to be executed in the JANET Simulator
%% PURPOSE:   Execute a command on the JANET Simulator via its 'ctr_manager' and return the result of the operation
%% CONTENTS:  The Module, Function and ArgsList to be evaluated by the 'ctr_manager' via apply()
%% MATCHES:   (always) (when the request comes from the JANET Controller node)
%% ACTIONS:   Forward the command to the 'ctr_manager' via a synhcronous call and return its response
%% ANSWER:    The answer of the 'ctr_manager', consisting in the result of the specified command
%% NEW STATE: -
%%
handle_call({sim_command,Module,Function,ArgsList},{CtrRESTHdlPID,_},SrvState=#simsrvstate{mgr_pid=MgrPid}) when node(CtrRESTHdlPID) =:= node() ->

 % Forward the command to associated 'ctr_manager' in the JANET Simulator, waiting for its response up to a predefined timeout
 Res = try gen_server:call(MgrPid,{sim_command,Module,Function,ArgsList},4800)
 catch
  exit:{timeout,_} ->
  
   % ctr_manager timeout
   {error,sim_timeout}
 end,
 
 % Return the 'ctr_manager' response (or the timeout expiration)
 {reply,Res,SrvState};


%% Unexpected call
handle_call(Request,From,SrvState) ->
  
 % Retrieve the controller's location ID for logging purposes
 {ok,Loc_id} = application:get_env(loc_id),
 
 % Report that an unexpected call was received by this gen_server
 io:format("[ctr_simserver-~w]: <WARNING> Unexpected call (Request = ~p, From = ~p, SrvState = ~p)~n",[Loc_id,Request,From,SrvState]),

 % Reply with a stub message and keep the SrvState
 {reply,unsupported,SrvState}.


%% ========================================================= HANDLE_CAST ========================================================= %% 

%% CTR_CONN_UPDATE
%% ---------------
%% SENDER:    The controller HTTP client ('ctr_httpclient')
%% WHEN:      When the connection towards the remote REST server changes ("GUN UP" or "GUN DOWN" messages)
%% PURPOSE:   Inform the 'ctr_manager' associated with the controller in the
%%            JANET Simulator of the updated controller connection state
%% CONTENTS:  1) The updated controller connection state ('connecting'|'online')
%%            2) The PID of the 'ctr_httpclient' ("security purposes")
%% MATCHES:   - (when the request comes from the JANET Controller)
%% ACTIONS:   Forward the updated controller connection state to the
%%            'ctr_manager' in the JANET Simulator via a cast()
%% NEW STATE: Update the 'conn_state' variable to the passed connection state
%%
handle_cast({ctr_conn_update,ConnState,CtrHTTPCliPID},SrvState=#simsrvstate{mgr_pid=MgrPid}) when (ConnState =:= 'online' orelse ConnState =:= 'connecting'),
                                                                                                    is_pid(CtrHTTPCliPID), node(CtrHTTPCliPID) =:= node() ->

 % Forward the updated controller connection state to its associated 'ctr_manager' in the JANET Simulator
 gen_server:cast(MgrPid,{ctr_conn_update,ConnState,self()}),
 
 % Update the 'ctr_state' variable to the passed connection state
 {noreply,SrvState#simsrvstate{ctr_state = ConnState}};


%% Unexpected cast
handle_cast(Request,SrvState) ->
 
 % Retrieve the controller's location ID for logging purposes
 {ok,Loc_id} = application:get_env(loc_id),
 
 % Report that an unexpected cast was received by this gen_server
 io:format("[ctr_simserver-~w]: <WARNING> Unexpected cast (Request = ~p, SrvState = ~w)~n",[Loc_id,Request,SrvState]),

 % Keep the SrvState
 {noreply,SrvState}. 
 

%%====================================================================================================================================
%%                                                         START FUNCTION                                                        
%%====================================================================================================================================

%% Called by its 'sup_jctr' supervisor during the JANET Controller boot
start_link() ->
 gen_server:start_link({local,?MODULE},?MODULE,[],[]).  % The spawned process is also registered locally under the 'ctr_simserver' name