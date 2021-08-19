%% This is the callback module of the JANET Device (janet_device) application %%

-module(jdev).
-behaviour(application).
-export([run/5,shutdown/0]).  % Application Start and Stop
-export([start/2,stop/1]). 	  % Application Behaviour Callback Functions
 
%%====================================================================================================================================
%%                                                   APPLICATION START AND STOP                                                        
%%====================================================================================================================================

%% DESCRIPTION:  Prepares the configuration parameters and starts the JANET Device application
%%
%% ARGUMENTS:    - Dev_id: The device's ID
%%               - Loc_id: The ID of the location the device is deployed in
%%               - MgrPid: The PID of the manager associated to this device in the Janet Simulator node 
%%               - Type:   The device's type
%%               - Config: The device's starting configuration
%%
%% RETURNS:      - ok                      -> JANET Device succesfully started               
%%               - {error,already_running} -> The janet_device application is already running on the node
%%               - {error,invalid_devtype} -> The device type is invalid
%%               - {error,Reason}          -> Internal error in starting the application
%%               - {error,badarg}          -> Invalid arguments
%%
%% NOTE:         A device doesn't require to know the sublocation it is deployed in
%%
run(Dev_id,Loc_id,MgrPid,Type,Config) when is_number(Dev_id), Dev_id>0, is_number(Loc_id), Loc_id>0, is_pid(MgrPid) ->
 
 % Check if the JANET Device is already running
 case utils:is_running(janet_device) of
  true ->
  
   % If it is, return an error
   {error,already_running};
   
  false ->
   
   % Otherwise, check the device's type to be valid (NOTE: The configuration Config is not checked here)
   case utils:is_valid_devtype(Type) of
    false ->
	
	 % If it is invalid, return an error
	 {error,invalid_devtype};
	 
    true ->
	
	 % Otherwise initialize the JANET Device configuration parameters as for the arguments
     application:set_env(janet_device,dev_id,Dev_id),
     application:set_env(janet_device,loc_id,Loc_id),
     application:set_env(janet_device,mgrpid,MgrPid),
     application:set_env(janet_device,type,Type),
     application:set_env(janet_device,config,Config),
	 
	 % Start the JANET Device
     %% [TODO]: logger:set_primary_config(#{level => warning}),  (hides the == APPLICATION INFO === messages when supervisors stop components, uncomment before release)	
     application:start(janet_device)
   end
 end;
 
run(_,_,_,_,_) ->
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

%% Starts the JANET Device
start(normal, _Args) ->
 sup_jdev:start_link().
 
%% Called once the JANET Device has been stopped
stop(_State) ->
 ok.