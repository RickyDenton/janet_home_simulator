%% This is the callback module of the JANET Controller (janet_controller) application %%

-module(jctr).
-behaviour(application).

-export([run/6,shutdown/0]).  % Application Start and Stop
-export([start/2,stop/1]). 	  % Application Behaviour Callback Functions
 
%%====================================================================================================================================
%%                                                   APPLICATION START AND STOP                                                        
%%====================================================================================================================================

%% DESCRIPTION:  Prepares the configuration parameters and starts the JANET Controller application
%%
%% ARGUMENTS:    - Loc_id:         The ID of the location the controller is deployed in
%%               - CtrSublocTable: The serialized controller's 'ctr_sublocation' table ([{subloc_id,devlist}])
%%               - CtrDeviceTable: The serialized controller's 'ctr_device' table      ([{dev_id,subloc_id,type,config,lastupdate,handler_pid}])
%%               - MgrPid:         The PID of the manager associated to this controller in the Janet Simulator node 
%%               - RestPort:       The port that will be used by the JANET Controller for binding its REST server on the host OS (>=30000)
%%               - RemoteHost:     The IP address of the host where JANET controller will forward state updates
%%
%% RETURNS:      - ok                      -> JANET Controller succesfully started
%%               - {error,already_running} -> The janet_controller application is already running on the node
%%               - {error,Reason}          -> Internal error in starting the application
%%               - {error,badarg}          -> Invalid arguments
%%
run(Loc_id,CtrSublocTable,CtrDeviceTable,MgrPid,RestPort,RemoteHost) when is_number(Loc_id), Loc_id>0, is_pid(MgrPid), is_number(RestPort), RestPort>=30000 ->
 
 % Check if the JANET Controller is already running
 case utils:is_running(janet_controller) of
  true ->
  
   % If it is, return an error
   {error,already_running};
  
  false ->
   
   % Otherwise, initialize the JANET Controller configuration parameters as for the arguments
   application:set_env(janet_controller,loc_id,Loc_id),
   application:set_env(janet_controller,mgr_pid,MgrPid),
   application:set_env(janet_controller,rest_port,RestPort),
   application:set_env(janet_controller,remotehost,RemoteHost),
   
   % Start Mnesia in disc-less and permanent mode and initialize
   % the tables used by the JANET Controller application
   ok = ctr_db:init_mnesia(CtrSublocTable,CtrDeviceTable),
   
   % Start the JANET Controller in permanent mode
   %% [TODO]: logger:set_primary_config(#{level => warning}),  (hides the == APPLICATION INFO === messages when supervisors stop components, uncomment before release)	
   application:start(janet_controller,permanent)
 end;
 
run(_,_,_,_,_,_) ->
 {error,badarg}.
 

%% DESCRIPTION:  Stops the JANET Controller node
%%
%% ARGUMENTS:    none 
%%
%% RETURNS:      - ok -> JANET Controller node succesfully stopped
%% 
shutdown() ->
 init:stop("shutdown").
 
%%====================================================================================================================================
%%                                             APPLICATION BEHAVIOUR CALLBACK FUNCTIONS                                                        
%%====================================================================================================================================

%% Starts the JANET Controller
start(normal,_Args) ->
 sup_jctr:start_link().
 
%% Called once the JANET Controller has been stopped
stop(_State) ->
 ok.