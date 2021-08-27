%% This module represents the server in the JANET Controller application used for interfacing with the JANET simulator node %%

-module(ctr_simserver).
-behaviour(gen_server).

-export([start_link/0,init/1,terminate/2,handle_call/3,handle_cast/2,handle_continue/2]).  % gen_server Behaviour Callback Functions

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

 % Retrieve the 'mgrpid' environment variable
 {ok,MgrPid} = application:get_env(mgrpid),
 
 % Attempt to register the JANET controller node with its controller
 % manager in the simulator node by passing the PID of the ctr_simserver
 %
 % NOTE: This registration request is performed synchronously since
 %       the controller node's execution cannot continue if it fails 
 gen_server:call(MgrPid,{ctr_reg,self()},10000),
 
 io:format("[dev_server]: Initialized~n"),
 {noreply,{online,MgrPid}}. %% [TODO]: Online or something else?
 
 
%% ========================================================== TERMINATE ========================================================== %% 

%% --------- STUB
% NOTE: This is currently not called on shutdown since the process is not trapping exit signals
terminate(normal,_) ->
 io:format("[ctr_simserver]: Terminated").


%% ========================================================= HANDLE_CALL ========================================================= %%

%% SENDER:    The controller node's manager
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
 
 
%% --------- STUB
handle_call(Num,_,{Sum,N}) when is_number(Num) ->
 New_Sum = Sum + Num,
 New_N = N+1,
 {reply,New_Sum/New_N,{New_Sum,New_N}}.


%% ========================================================= HANDLE_CAST ========================================================= %% 

%% --------- STUB
handle_cast(reset,State) -> % Resets the server State
 {noreply,State}.

%%====================================================================================================================================
%%                                                         START FUNCTION                                                        
%%====================================================================================================================================

%% Called by its 'sup_jctr' supervisor during the JANET Controller boot
start_link() ->
 gen_server:start_link({local,?MODULE},?MODULE,[],[]).