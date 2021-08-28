%% This module represents the handler of a registered device in the JANET Controller application %%

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
 
 % Register the handler in the 'devregister' table
 {atomic,ok} = mnesia:transaction(fun() -> mnesia:write(#devregister{dev_id=Dev_id,handler_pid=self()}) end),
 
 % Old logging
 % io:format("[ctr_devhandler-~w]: Device registered~n",[Dev_id]),
 
 % Create a monitor towards the device node's 'dev_server' process identified by "DevSrvPid"
 MonRef = monitor(process,DevSrvPid),
 
 % Return the devhandler server (constant) state
 {ok,#devhandlerstate{dev_id = Dev_id, dev_srv_pid = DevSrvPid, dev_srv_mon = MonRef}}.


%% ========================================================= HANDLE_CALL ========================================================= %%

%% DEBUGGING PURPOSES [TODO]: REMOVE
handle_call(_,{ReqPid,_},SrvState) ->
 io:format("[ctr_devhandler-~w]: <WARNING> Generic response issued to ReqPid = ~w~n",[SrvState#devhandlerstate.dev_id,ReqPid]),
 {reply,gen_response,SrvState}.


%% ========================================================= HANDLE_CAST ========================================================= %% 

%% --------- STUB
handle_cast(reset,SrvState) ->
 {noreply,SrvState}.
 

%% ========================================================= HANDLE_INFO ========================================================= %%  

%% SENDER:    The Erlang Run-Time System (ERTS)
%% WHEN:      When the monitored 'dev_server' process on the device node terminates
%% PURPOSE:   Inform of the 'dev_server' process termination
%% CONTENTS:  1) The monitor reference the notification refers to
%%            2) The PID of the process that was monitored (the 'dev_server' process)
%%            3) The reason for the monitored process's termination
%% MATCHES:   (always) (when the monitor reference and the PID of the monitored process match the ones in the server's state)
%% ACTIONS:   If Reason =:= 'noproc' log the event (it should not happen), and then stop 
%%            the 'ctr_devhandler' server, deregistring the device node from the controller
%% ANSWER:    -
%% NEW STATE: Stop the server (reason = 'normal' because no errors should be propagated to the 'sup_devhandlers' supervisor)
%%
handle_info({'DOWN',MonRef,process,DevSrvPid,Reason},SrvState) when MonRef =:= SrvState#devhandlerstate.dev_srv_mon, DevSrvPid =:= SrvState#devhandlerstate.dev_srv_pid ->

 % If Reason =:= 'noproc', which is associated to the fact that the 'dev_server' passed a non-existing "DevSrvPid"
 % while registering in the 'ctr_regserver' or it died before the monitor could be established, log the error
 if
  Reason =:= noproc ->
   io:format("[ctr_devhandler-~w]: <WARNING> The 'dev_server' process of registered device does not exist~n",[SrvState#devhandlerstate.dev_id]);
  true ->
   ok
 end,
 
 % Stop the 'ctr_devhandler' server (reason = 'normal' because no errors should be propagated to the 'sup_devhandlers' supervisor)
 {stop,normal,SrvState}.


%% ========================================================== TERMINATE ========================================================== %% 

%% Called when the 'dev_handler' server is asked to shutdown by its 'sup_devhandlers' supervisor or if it crashes
terminate(_,SrvState) ->
  
 % Deregister the handled device from the 'devregister' table
 {atomic,ok} = mnesia:transaction(fun() -> mnesia:delete({devregister,SrvState#devhandlerstate.dev_id}) end),
 
 % Remove the monitor towards the device's 'dev_server' process, if it is still active
 demonitor(SrvState#devhandlerstate.dev_srv_mon),
 
 % Terminate
 ok. 

 
%%====================================================================================================================================
%%                                                         START FUNCTION                                                        
%%====================================================================================================================================

%% Called by the registered device handlers supervisor 'sup_devhandlers' on behalf
%% of the 'ctr_regserver' process whenever a device registers within the controller
start_link(Dev_id,DevSrvPid) ->
 gen_server:start_link(?MODULE,{Dev_id,DevSrvPid},[]).