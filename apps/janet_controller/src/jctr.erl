%% This is the callback module of the JANET Controller (janet_controller) application %%

-module(jctr).
-behaviour(application).

-export([run/9,shutdown/0]).  % Application Start and Stop
-export([start/2,stop/1]). 	  % Application Behaviour Callback Functions
 
%%====================================================================================================================================
%%                                                   APPLICATION START AND STOP                                                        
%%====================================================================================================================================

%% DESCRIPTION:  Prepares the configuration parameters and starts the JANET Controller application
%%
%% ARGUMENTS:    - Loc_id:               The ID of the location the controller is deployed in
%%               - CtrSublocTable:       The serialized controller's 'ctr_sublocation' table ([{subloc_id,devlist}])
%%               - CtrDeviceTable:       The serialized controller's 'ctr_device' table      ([{dev_id,subloc_id,type,config,lastupdate,handler_pid}])
%%               - MgrPid:               The PID of the manager associated to this controller in the Janet Simulator node 
%%               - CtrRESTPort:          The OS port to be used by the controller's REST server (int >= 30000)
%%               - RemoteRESTServerAddr: The address of the remote server accepting REST requests from the controller (a list)
%%               - RemoteRESTServerPort: The port of the remote server accepting REST requests from the controller (int > 0)
%%               - RemoteRESTServerPath: The remote REST server path where to send device state and connectivity updates
%%               - Loc_user:             The user the location belongs to [REMOTE SERVER COMPATIBILITY]
%%
%% RETURNS:      - ok                      -> JANET Controller succesfully started
%%               - {error,already_running} -> The janet_controller application is already running on the node
%%               - {error,Reason}          -> Internal error in starting the application
%%               - {error,badarg}          -> Invalid arguments
%%
run(Loc_id,CtrSublocTable,CtrDeviceTable,MgrPid,CtrRESTPort,RemoteRESTServerAddr,RemoteRESTServerPort,RemoteRESTServerPath,Loc_user) when is_number(Loc_id), Loc_id > 0, is_pid(MgrPid), is_number(CtrRESTPort),
                                                                                                                                          CtrRESTPort >= 30000, is_list(RemoteRESTServerAddr), is_number(RemoteRESTServerPort),
																													                      RemoteRESTServerPort > 0, is_list(RemoteRESTServerPath), is_list(Loc_user) ->
 % Check if the JANET Controller is already running
 case utils:is_running(janet_controller) of
  true ->
  
   % If it is, return an error
   {error,already_running};
  
  false ->
   
   % Otherwise, initialize the JANET Controller configuration parameters as for the arguments
   application:set_env(janet_controller,loc_id,Loc_id),
   application:set_env(janet_controller,mgr_pid,MgrPid),
   application:set_env(janet_controller,ctr_rest_port,CtrRESTPort),
   application:set_env(janet_controller,remote_rest_server_addr,RemoteRESTServerAddr),
   application:set_env(janet_controller,remote_rest_server_port,RemoteRESTServerPort),
   application:set_env(janet_controller,remote_rest_server_path,RemoteRESTServerPath),
   application:set_env(janet_controller,loc_user,Loc_user),
   
   % Start Mnesia in disc-less and permanent mode and initialize
   % the tables used by the JANET Controller application
   ok = ctr_db:init_mnesia(CtrSublocTable,CtrDeviceTable),
   
   % Start the JANET Controller in permanent mode
   application:start(janet_controller,permanent)
 end;
 
run(_,_,_,_,_,_,_,_,_) ->
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

%% ============================================================ START ============================================================ %%

%% Called during the "application:start(janet_controller)" call for starting the JANET Controller application
start(normal,_Args) ->
 
 % Start the root supervision tree of the JANET Simulator application
 sup_jctr:start_link().


%% ============================================================ STOP ============================================================ %% 

%% Called during the "application:stop(janet_controller)" call AFTER the application has been stopped
stop(_State) ->
 ok.