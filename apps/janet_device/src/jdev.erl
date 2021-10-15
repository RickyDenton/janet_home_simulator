%% This is the callback module of the JANET Device (janet_device) application %%

-module(jdev).
-behaviour(application).

-export([run/6,shutdown/0]).  % Application Start and Stop
-export([start/2,stop/1]). 	  % Application Behaviour Callback Functions
 
%%====================================================================================================================================
%%                                                   APPLICATION START AND STOP                                                        
%%====================================================================================================================================

%% DESCRIPTION:  Prepares the configuration parameters and starts the JANET Device application
%%
%% ARGUMENTS:    - Dev_id:      The device's ID
%%               - Loc_id:      The ID of the location the device is deployed in
%%               - MgrPid:      The PID of the manager associated to this device in the Janet Simulator node 
%%               - Type:        The device's type
%%               - Config:      The device's starting configuration
%%               - CtrHostName: The name of the host where the location controller node is deployed
%%
%% RETURNS:      - ok                      -> JANET Device succesfully started               
%%               - {error,already_running} -> The janet_device application is already running on the node
%%               - {error,invalid_devtype} -> The device type is invalid
%%               - {error,Reason}          -> Internal error in starting the application
%%               - {error,badarg}          -> Invalid arguments
%%
%% NOTE:         A device doesn't require to know the sublocation it is deployed in
%%
run(Dev_id,Loc_id,MgrPid,Type,Config,CtrHostName) when is_number(Dev_id), Dev_id>0, is_number(Loc_id),
                                                        Loc_id>0, is_pid(MgrPid), is_list(CtrHostName) ->
 
 % Check if the JANET Device is already running
 case utils:is_running(janet_device) of
  true ->
  
   % If it is, return an error
   {error,already_running};
   
  false ->
   
   % Otherwise, check the device's type to be valid
   %
   % NOTE: The configuration validity is not checked for here
   %
   case utils:is_valid_devtype(Type) of
    false ->
	
	 % If it is invalid, return an error
	 {error,invalid_devtype};
	 
    true ->
	
	 % Otherwise initialize the JANET Device configuration parameters as for the arguments
     application:set_env(janet_device,dev_id,Dev_id),
     application:set_env(janet_device,loc_id,Loc_id),
     application:set_env(janet_device,mgr_pid,MgrPid),
     application:set_env(janet_device,type,Type),
     application:set_env(janet_device,config,Config),
	 application:set_env(janet_device,ctr_hostname,CtrHostName),
	 
	 % Start the JANET Device in permanent mode	
     application:start(janet_device,permanent)
   end
 end;
 
run(_,_,_,_,_,_) ->
 {error,badarg}. 
 
 
%% DESCRIPTION:  Stops the JANET Device node
%%
%% ARGUMENTS:    none 
%%
%% RETURNS:      - ok -> JANET Device node succesfully stopped
%% 
shutdown() ->
 init:stop("shutdown").


%%====================================================================================================================================
%%                                             APPLICATION BEHAVIOUR CALLBACK FUNCTIONS                                                        
%%====================================================================================================================================

%% ============================================================ START ============================================================ %%

%% Called during the "application:start(janet_device)" call for starting the JANET Device application
start(normal,_Args) ->

 % Start the root supervision tree of the JANET Device application
 sup_jdev:start_link().
 

%% ============================================================ STOP ============================================================ %% 

%% Called during the "application:stop(janet_device)" call AFTER the application has been stopped
stop(_State) ->
 ok.