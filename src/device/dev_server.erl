%% This module represents the server in the JANET device application used for interfacing
%% interfacing both with the JANET Simulator and its location's JANET Controller node

-module(dev_server).
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
 % callback function for parallelization purposes (and for allowing the dev_manager process to respond to the registration request)  
 {ok,{booting,none},{continue,init}}. %% [TODO]: Define the server state via a proper record
 
 
%% ======================================================= HANDLE_CONTINUE ======================================================= %%  

%% Registers the JANET device node with its device manager in the JANET simulator node by passing the PIDs of
%% the dev_server and of the dev_statem for then attempting to register the devices with its location controller
%%
%% [TODO]: Rewrite description if necessary
%%
handle_continue(init,{booting,none}) ->

 % Retrieve the 'mgrpid' environment variable
 {ok,MgrPid} = application:get_env(mgrpid),
 
 % Retrieve the PID of the device's state
 % machine via the local naming registry
 case whereis(dev_statem) of
  undefined ->
  
   % The device state machine not being registered in the local naming registry is a
   % fatal error, and dev_server, along with the entire device node, must be shut down
   {stop,dev_statem_not_registered,booting};
   
  Dev_statem_pid ->
  
   % If the PID of the device state machine was successfully retrieved, attempt
   % to register the JANET device node with its device manager in the simulator
   % node by passing the PIDs of the 'dev_statem' and of the 'dev_server'
   %
   % NOTE: This registration request is performed synchronously since
   %       the controller node's execution cannot continue if it fails 
   gen_server:call(MgrPid,{dev_reg,self(),Dev_statem_pid},10000),
   
	 
   %% [TODO]: Continue from here
	 
   io:format("[dev_server]: Initialized~n"),
   {noreply,{connecting,MgrPid}}
 end.
 
 
%% ========================================================== TERMINATE ========================================================== %% 
 
%% --------- STUB
% NOTE: This is currently not called on shutdown since the process is not trapping exit signals
terminate(normal,_) ->
 io:format("[dev_server]: Terminated").


%% ========================================================= HANDLE_CALL ========================================================= %%

%% SENDER:    The device node's manager
%% WHEN:      (varies) [TODO]: Double-check
%% PURPOSE:   Execute a command on the device node and return the result of the operation
%% CONTENTS:  The Module, Function and ArgsList to be evaluated via apply()
%% MATCHES:   When the 'dev_server' is registered (and the request comes from the JANET Simulator)
%% ACTIONS:   Execute the required command via apply()
%% ANSWER:    The result of the apply() function
%% NEW STATE: -
%%
handle_call({dev_command,Module,Function,ArgsList},{ReqPid,_},{State,MgrPid}) when State =/= booting andalso ReqPid =:= MgrPid ->

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

%% Called by its 'sup_jdev' supervisor during the JANET Device boot
start_link() ->
 gen_server:start_link({local,?MODULE},?MODULE,[],[]).