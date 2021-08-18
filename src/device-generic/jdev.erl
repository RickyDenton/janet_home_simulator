-module(jdev).
-behaviour(application).
-export([run/4,halt/0]).                   % Application Start and Stop
-export([start/2,stop/1]). 				   % Application Behaviour Callback Functions
 
%% ========== JANET Device API ========== %%

%% --- Application start and stop --- %% 

% Start the Application
run(Dev_id,Loc_id,Type,Config) ->
 
 case is_running() of
  true ->
   {error, already_running};
  false ->
   
   % Initialize the JANET device environment variables
   application:set_env(janet_device,dev_id,Dev_id),
   application:set_env(janet_device,loc_id,Loc_id),
   application:set_env(janet_device,type,Type),
   application:set_env(janet_device,config,Config),
   
  
   % logger:set_primary_config(#{level => warning}),    % Uncomment before release (hides == APPLICATION INFO === messages when applications are stopped)
   % Start the JANET device
   application:start(janet_device)
 end.
 
% Stop the Application
halt() ->
 case is_running() of
  true ->
   application:stop(janet_device),
   timer:sleep(5),   % For output ordering purposes (not necessary if the primary logger level is configured to "warning")
   io:format("JANET Device stopped~n");
  false ->
   {error, not_running}
 end.
 
 
%% --- Mnesia Utility Functions --- %%  

 
%% --- Other Utility Functions --- %% 
 
% Check if an application is running (default: janet_device)
is_running() ->
 is_running(janet_device).
is_running(AppName) ->
 case [App || {App, _, _} <- application:which_applications(), App =:= AppName] of
  [AppName] ->
   true;
  [] ->
   false
 end.
 

%% ========== Application Behaviour Callback Functions ========== %% 

% Start the Janet Device
start(normal, _Args) ->
 sup_jdev:start_link().
 
% Stop the Janet Device
stop(_State) ->
 ok.
 