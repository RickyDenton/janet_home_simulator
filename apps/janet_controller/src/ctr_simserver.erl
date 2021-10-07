%% This module represents the server in the JANET Controller application used for interfacing with the JANET simulator node %%

-module(ctr_simserver).
-behaviour(gen_server).

-export([start_link/0,init/1,handle_call/3,handle_cast/2,handle_continue/2]).  % gen_server Behaviour Callback Functions

%% Server state: 
%% {
%%  CtrState,   % Whether the controller is currently connected with the remote REST server ('connecting' | 'online')
%%  MgrPid      % The PID of the 'ctr_manager' associated with the Controller in the JANET Simulator
%% }
%%
%% [TODO]: Define via a proper record?

%%====================================================================================================================================
%%                                                  GEN_SERVER CALLBACK FUNCTIONS                                                        
%%====================================================================================================================================

%% ============================================================ INIT ============================================================ %%
init(_) ->
 
 % Return the server initial state, where further initialization operations will be performed in the "handle_continue(Continue,State)"
 % callback function for parallelization purposes (and for allowing the ctr_manager process to respond to the registration request)  
 {ok,{booting,none},{continue,init}}.
 
 
%% ======================================================= HANDLE_CONTINUE ======================================================= %%  

%% Registers the JANET controller node with its controller manager in the JANET simulator node by passing the PID of the ctr_simserver
handle_continue(init,{booting,none}) ->

 % Retrieve the 'mgr_pid' environment variable
 {ok,MgrPid} = application:get_env(mgr_pid),
 
 % Attempt to register the JANET controller node with its controller
 % manager in the simulator node by passing the PID of the ctr_simserver
 %
 % NOTE: This registration request is performed synchronously since
 %       the controller node's execution cannot continue if it fails 
 ok = gen_server:call(MgrPid,{ctr_reg,self()},10000),
 
 % Return the server state
 {noreply,{connecting,MgrPid}}.
 

%% ========================================================= HANDLE_CALL ========================================================= %%

%% CTR_COMMAND
%% -----------
%% SENDER:    The controller node's manager in the JANET Simulator node
%% WHEN:      (varies) [TODO]: Double-check
%% PURPOSE:   Execute a command on the controller node and return the result of the operation
%% CONTENTS:  The Module, Function and ArgsList to be evaluated via apply()
%% MATCHES:   When the 'ctr_simserver' is registered (and the request comes from the JANET Simulator)
%% ACTIONS:   Execute the required command via apply()
%% ANSWER:    The result of the apply() function
%% NEW STATE: -
%%
handle_call({ctr_command,Module,Function,ArgsList},{ReqPid,_},{CtrState,MgrPid}) when CtrState =/= booting andalso ReqPid =:= MgrPid ->

 % Execute the required command and return its result
 {reply,apply(Module,Function,ArgsList),{CtrState,MgrPid}};


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
handle_call({sim_command,Module,Function,ArgsList},{CommPid,_},{CtrState,MgrPid}) when node(CommPid) =:= node() ->

 % Forward the command to associated 'ctr_manager' in the JANET Simulator, waiting for its response up to a predefined timeout
 Res = try gen_server:call(MgrPid,{sim_command,Module,Function,ArgsList},4800)
 catch
  exit:{timeout,_} ->
  
   % ctr_manager timeout
   {error,sim_timeout}
 end,
 
 % Return the 'ctr_manager' response (or the timeout expiration)
 {reply,Res,{CtrState,MgrPid}}.


%% ========================================================= HANDLE_CAST ========================================================= %% 

%% CTR_STATE_UPDATE
%% ----------------
%% SENDER:    The controller HTTP client ('ctr_httpclient')
%% WHEN:      When the connection towards the remote REST server changes (Gun up or down)
%% PURPOSE:   Inform the 'ctr_server' of the updated controller connection state
%% CONTENTS:  The updated controller connection state ('connecting'|'online')
%% MATCHES:   - (when the request comes from the JANET Controller)
%% ACTIONS:   Forward the updated controller connection state to the 'ctr_server' via a cast()
%% NEW STATE: Update the server state to the passed connection state
%%
handle_cast({ctr_state_update,UpdatedCtrState,CtrHTTPCliPID},{_CurrCtrState,MgrPid}) when node(CtrHTTPCliPID) =:= node() ->

 % Forward the updated controller connection state to its 'ctr_manager' in the JANET Simulator
 gen_server:cast(MgrPid,{ctr_state_update,UpdatedCtrState,self()}),
 
 % Update the server state to the passed connection state
 {noreply,{UpdatedCtrState,MgrPid}}.


%%====================================================================================================================================
%%                                                         START FUNCTION                                                        
%%====================================================================================================================================

%% Called by its 'sup_jctr' supervisor during the JANET Controller boot
start_link() ->
 gen_server:start_link({local,?MODULE},?MODULE,[],[]).  % The spawned process is also registered locally under the 'ctr_simserver' name