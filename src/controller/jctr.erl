-module(jctr).
-behaviour(application).
-export([run/5,halt/0]).                   % Application Start and Stop
-export([start/2,stop/1]). 				   % Application Behaviour Callback Functions
 
%% ========== JANET Controller API ========== %%

%% --- Application start and stop --- %% 

% Start the Application TODO: Create hidden node!
run(Loc_id,DevAlloc,MgrPid,Port,RemoteHost) ->
 
 case is_running() of
  true ->
   {error, already_running};
  false ->
   
   % Initialize the JANET controller environment variables
   application:set_env(janet_controller,loc_id,Loc_id),
   application:set_env(janet_controller,devalloc,DevAlloc),
   application:set_env(janet_controller,mgrpid,MgrPid),
   application:set_env(janet_controller,rest_port,Port),
   application:set_env(janet_controller,remotehost,RemoteHost),
   
  
   % logger:set_primary_config(#{level => warning}),    % Uncomment before release (hides == APPLICATION INFO === messages when applications are stopped)
   % Start the JANET controller
   application:start(janet_controller)
 end.
 
% Stop the Application
halt() ->
 case is_running() of
  true ->
   application:stop(janet_controller),
   timer:sleep(5),   % For output ordering purposes (not necessary if the primary logger level is configured to "warning")
   io:format("JANET Controller stopped~n");
  false ->
   {error, not_running}
 end.
 
 
%% --- Mnesia Utility Functions --- %%  

 
%% --- Other Utility Functions --- %% 
 
% Check if an application is running (default: janet_controller)
is_running() ->
 is_running(janet_controller).
is_running(AppName) ->
 case [App || {App, _, _} <- application:which_applications(), App =:= AppName] of
  [AppName] ->
   true;
  [] ->
   false
 end.
 

%% ========== Application Behaviour Callback Functions ========== %% 

% Start the Janet Controller
start(normal, _Args) ->
 sup_jctr:start_link().
 
% Stop the Janet Controller
stop(_State) ->
 ok.
 