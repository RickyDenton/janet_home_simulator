%% This module represents the location devices registration server in the JANET Controller application %%

-module(ctr_regserver).
-behaviour(gen_server).

-export([start_link/0,init/1,handle_call/3,handle_cast/2]).  % gen_server Behaviour Callback Functions

-include("ctr_mnesia_tables_definitions.hrl").  % Janet Controller Mnesia Tables Records Definitions

%% [SrvState Format]: {online} (constant, stateless server)

%%====================================================================================================================================
%%                                                  GEN_SERVER CALLBACK FUNCTIONS                                                        
%%====================================================================================================================================

%% ============================================================ INIT ============================================================ %%
init(_) ->

 %% [TODO]: Trap Exit signals?
 %% process_flag(trap_exit,true),
 
 % No initialization is required for the 'ctr_regserver',
 % and so just return its (constant) state
 {ok,online}.


%% ========================================================= HANDLE_CALL ========================================================= %%

%% SENDER:    A device's 'dev_server' (or more precisely its 'ctr_reg' gen_client) [TODO]: Check Name
%% WHEN:      After the 'dev_server' has registered with the JANET Simulator and is attempting to register with its JANET Controller
%% PURPOSE:   Registration request with its location JANET controller
%% CONTENTS:  The device's ID and the PID of its 'dev_server' process
%% MATCHES:   (always)
%% ACTIONS:   1) If the device is recognized to belong to the location and it is not already registered,
%%               register it by spawning its 'ctr_devhandler' server under the 'sup_devhandlers' supervisor
%%            2) If the device is recognized to belong to the location and it is already registered,
%%               just return the PID of its 'ctr_devhandler' server
%%            3) If the device does not belong to the location, return an error
%% ANSWER:    1- {ok,Devhandler_Pid}
%%            2- {ok,Devhandler_Pid}
%%            3- {error,device_not_recognized}
%% NEW STATE: -
%%
handle_call({reg_request,Dev_id,DevSrvPid},{_,_},SrvState) when is_number(Dev_id), Dev_id>0, is_pid(DevSrvPid) ->

 % Check if the device is already registed via the 'devregister' table
 case ctr_db:get_record(devregister,Dev_id) of
  {ok,DevHandlerRecord} ->
  
   % If the device is already registered, just return the PID of its 'ctr_devhandler' server
   {reply,{ok,DevHandlerRecord#devregister.handler_pid},SrvState};
   
  _ ->
  
   % If the device is not registered, ensure that it belongs to the controller's location via the 'devalloc' table
   {ok,DevAllocAll} = ctr_db:get_record(devalloc,all),
   case lists:member(Dev_id,DevAllocAll#devalloc.devlist) of
    false ->
	
     % If the device is not recognized as belonging to the location, return an error to the sender
     {reply,{error,device_not_recognized},SrvState};

    true ->
    
     % Otherwise register the device by spawning its 'ctr_devhandler' server under the 'sup_devhandlers' supervisor 	
     {ok,Devhandler_pid} = supervisor:start_child(sup_devhandlers,[Dev_id,DevSrvPid]),
	 {reply,{ok,Devhandler_pid},SrvState}
   end
 end.


%% ===================================================== HANDLE_CAST (STUB) ===================================================== %% 

%% This represents a STUB of the handle_cast() callback function, whose
%% definition is formally required by the 'gen_server' OTP behaviour
handle_cast(Request,SrvState) ->

 % Retrieve the controller's location ID
 {ok,Loc_id} = application:get_env(loc_id),
 
 % Report that this gen_server should not receive cast requests
 io:format("[ctr_regserver-~w]: <WARNING> Unexpected cast (Request = ~w, SrvState = ~w)~n",[Loc_id,Request,SrvState]),

 % Keep the SrvState
 {noreply,SrvState}.
 
 
%%====================================================================================================================================
%%                                                         START FUNCTION                                                        
%%====================================================================================================================================

%% Called by the Janet Controller top-level supervisor (sup_jctr) at boot time 
start_link() ->
 gen_server:start_link({local,?MODULE},?MODULE,[],[]).  % The spawned process is also registered locally under the 'ctr_regserver' name