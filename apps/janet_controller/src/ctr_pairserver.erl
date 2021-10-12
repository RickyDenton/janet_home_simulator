%% This module represents the location devices' pairing server in the JANET Controller application %%

-module(ctr_pairserver).
-behaviour(gen_server).

-export([start_link/0,init/1,handle_call/3,handle_cast/2]).  % gen_server Behaviour Callback Functions

-include("ctr_mnesia_tables_definitions.hrl").  % Janet Controller Mnesia Tables Records Definitions

%% SrvState: {online} (constant, stateless server)

%%====================================================================================================================================
%%                                                  GEN_SERVER CALLBACK FUNCTIONS                                                        
%%====================================================================================================================================

%% ============================================================ INIT ============================================================ %%
init(_) ->

 % No initialization is required for the 'ctr_pairserver',
 % and so just return its (constant) state
 {ok,online}.


%% ========================================================= HANDLE_CALL ========================================================= %%

%% PAIR_REQUEST
%% ------------
%% SENDER:    A device's 'dev_server' (or more precisely its 'ctr_pairer' process)
%% WHEN:      After the 'dev_server' has registered with the JANET
%%            Simulator and is attempting to pair with its JANET Controller
%% PURPOSE:   Pairing request with its location JANET controller
%% CONTENTS:  The device's ID and the PID of its 'dev_server' process
%% MATCHES:   (always)
%% ACTIONS:   1) If the device is recognized to belong to the location and it is not already paired,
%%               do so by spawning its 'ctr_devhandler' server under the 'sup_devhandlers' supervisor
%%            2) If the device is recognized to belong to the location and it is already paired,
%%               just return the PID of its 'ctr_devhandler' server
%%            3) If the device does not belong to the location, return an error
%% ANSWER:    1- {ok,Devhandler_Pid}
%%            2- {ok,Devhandler_Pid}
%%            3- {error,device_not_recognized}
%% NEW STATE: -
%%
handle_call({pair_request,Dev_id,DevSrvPid},{_,_},SrvState) when is_number(Dev_id), Dev_id>0, is_pid(DevSrvPid) ->

 % Retrieve the device's entry in the 'ctr_device' table
 case ctr_db:get_record(ctr_device,Dev_id) of 
 
  {error,not_found} ->
  
   % If the "dev_id" entry was not found, the device does not
   % belong to the location, and so return an error to the sender
   {reply,{error,device_not_recognized},SrvState};
 
  {ok,CtrDevRecord} ->
  
   % If the entry was found, check if the device
   % is already paired via the 'handler_pid' field
   case CtrDevRecord#ctr_device.handler_pid of 
	
    HandlerPID when is_pid(HandlerPID) ->
	  
	 % If the device is already paired, just return the PID of its handler
	 {reply,{ok,HandlerPID},SrvState};
	  
	'-' ->
	 
	 % Otherwise if the device is not paired, attempt to do so by spawning
	 % its 'ctr_devhandler' under the 'sup_devhandlers' supervisor
	 case supervisor:start_child(sup_devhandlers,[Dev_id,DevSrvPid]) of
	  
	  {ok,undefined} ->
	    
	   % If this tuple is returned by the 'sup_devhandlers' supervisor
	   % it means that the device was deleted from the controller between
	   % its pairing and the spawning of its handler, and so print a warning
	   % before responding to the sender (which probably doesn't exist anymore)
	   {ok,Loc_id} = application:get_env(loc_id),
	   io:format("[ctr_pairserver-~w]: <WARNING> Device was deleted between its pairing and the spawning of its handler (Dev_id = ~w)~n",[Loc_id,Dev_id]),
	   {reply,{error,device_deleted},SrvState};
	   
	  {ok,Devhandler_pid} ->

       % Otherwise if the device handler was successfully
	   % spawned, return its PID to the sender
	   {reply,{ok,Devhandler_pid},SrvState}
	 end
   end
 end;


%% Unexpected call
handle_call(Request,From,SrvState) ->
  
 % Retrieve the controller's location ID for logging purposes
 {ok,Loc_id} = application:get_env(loc_id),
 
 % Report that an unexpected call was received by this gen_server
 io:format("[ctr_pairserver-~w]: <WARNING> Unexpected call (Request = ~p, From = ~p, SrvState = ~p)~n",[Loc_id,Request,From,SrvState]),

 % Reply with a stub message and keep the SrvState
 {reply,unsupported,SrvState}.
 

%% ===================================================== HANDLE_CAST (STUB) ===================================================== %% 

%% This represents a STUB of the handle_cast() callback function, whose
%% definition is formally required by the 'gen_server' OTP behaviour
handle_cast(Request,SrvState) ->

 % Retrieve the controller's location ID for logging purposes
 {ok,Loc_id} = application:get_env(loc_id),
 
 % Report that this gen_server should not receive cast requests
 io:format("[ctr_pairserver-~w]: <WARNING> Unexpected cast (Request = ~w, SrvState = ~w)~n",[Loc_id,Request,SrvState]),

 % Keep the SrvState
 {noreply,SrvState}.
 
 
%%====================================================================================================================================
%%                                                         START FUNCTION                                                        
%%====================================================================================================================================

%% Called by the Janet Controller top-level supervisor (sup_jctr) at boot time 
start_link() ->
 gen_server:start_link({local,?MODULE},?MODULE,[],[]).  % The spawned process is also registered locally under the 'ctr_pairserver' name
