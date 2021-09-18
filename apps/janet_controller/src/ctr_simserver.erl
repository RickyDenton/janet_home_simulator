%% This module represents the server in the JANET Controller application used for interfacing with the JANET simulator node %%

-module(ctr_simserver).
-behaviour(gen_server).

-export([start_link/0,init/1,handle_call/3,handle_cast/2,handle_continue/2]).  % gen_server Behaviour Callback Functions

%% [TODO]: Server state?

%%====================================================================================================================================
%%                                                  GEN_SERVER CALLBACK FUNCTIONS                                                        
%%====================================================================================================================================

%% ============================================================ INIT ============================================================ %%
init(_) ->

 %% [TODO]: Trap Exit signals?
 %% process_flag(trap_exit,true),
 
 % Return the server initial state, where further initialization operations will be performed in the "handle_continue(Continue,State)"
 % callback function for parallelization purposes (and for allowing the ctr_manager process to respond to the registration request)  
 {ok,{booting,none},{continue,init}}. %% [TODO]: Define the server state via a proper record
 
 
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
 
 % Return the server (constant) state
 {noreply,{online,MgrPid}}. %% [TODO]: Online or something else?
 

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
handle_call({ctr_command,Module,Function,ArgsList},{ReqPid,_},{State,MgrPid}) when State =/= booting andalso ReqPid =:= MgrPid ->

 % Execute the required command and return its result
 {reply,apply(Module,Function,ArgsList),{State,MgrPid}};


%% SIM_COMMAND
%% -----------
%% SENDER:    The controller's REST server [TODO]:
%% WHEN:      A remote command is received requiring a function to be executed in the JANET Simulator
%% PURPOSE:   Execute a command on the JANET Simulator via its 'ctr_manager' and return the result of the operation
%% CONTENTS:  The Module, Function and ArgsList to be evaluated by the 'ctr_manager' via apply()
%% MATCHES:   (always) (when the request comes from the JANET Controller node)
%% ACTIONS:   Forward the command to the 'ctr_manager' via a synhcronous call and return its response
%% ANSWER:    The answer of the 'ctr_manager', consisting in the result of the specified command
%% NEW STATE: -
%%
handle_call({sim_command,Module,Function,ArgsList},{CommPid,_},{State,MgrPid}) when node(CommPid) =:= node() ->

 % Forward the command to associated 'ctr_manager' in the JANET Simulator, waiting for its response up to a predefined timeout
 Res = try gen_server:call(MgrPid,{sim_command,Module,Function,ArgsList},4800)
 catch
  exit:{timeout,_} ->
  
   % ctr_manager timeout
   {error,sim_timeout}
 end,
 
 % Return the 'ctr_manager' response (or the timeout expiration)
 {reply,Res,{State,MgrPid}}.
 

%% ===================================================== HANDLE_CAST (STUB) ===================================================== %% 

%% This represents a STUB of the handle_cast() callback function, whose
%% definition is formally required by the 'gen_server' OTP behaviour
handle_cast(Request,SrvState) ->

 % Retrieve the controller's location ID
 {ok,Loc_id} = application:get_env(loc_id),
 
 % Report that this gen_server should not receive cast requests
 io:format("[ctr_simserver-~w]: <WARNING> Unexpected cast (Request = ~w, SrvState = ~w)~n",[Loc_id,Request,SrvState]),

 % Keep the SrvState
 {noreply,SrvState}.


%%====================================================================================================================================
%%                                                         START FUNCTION                                                        
%%====================================================================================================================================

%% Called by its 'sup_jctr' supervisor during the JANET Controller boot
start_link() ->
 gen_server:start_link({local,?MODULE},?MODULE,[],[]).  % The spawned process is also registered locally under the 'ctr_simserver' name