-module(jsim).
-export([run/0,run/2,halt/0]).             % Application Start and Stop
-export([start_mnesia/0]).                 % Mnesia Utility Functions
-export([is_running/0,is_running/1]). 	   % Other Utility Functions
-export([start/2,stop/1]). 				   % Application Behaviour Callback Functions
-behaviour(application).
 
%% ========== JANET Simulator API ========== %%

%% --- Application start and stop --- %% 

% Start the Application
run() ->
 run(55555,"somehost.com:1240").

run(Port,RemoteHost) when is_number(Port), Port>=10000 ->

 case is_running() of
  true ->
   {error,already_running};
  false ->
  
   % Initialize the JANET simulator environment variables
   application:set_env(janet_simulator,rest_port,Port),
   application:set_env(janet_simulator,remotehost,RemoteHost),
  
   % logger:set_primary_config(#{level => warning}),    % Uncomment before release (hides == APPLICATION INFO === messages when applications are stopped)
   start_mnesia(),
   application:start(janet_simulator)
 end;
 
run(Port,_) when is_number(Port) ->
 io:format("Please use a port of value >= 10000 for the rest server for preventing port allocation conflicts on the host OS~n"); 
run(_,_) ->
 io:format("usage: run(Port,RemoteHost) (Port >= 10000)~n").
 
% Stop the Application
halt() ->
 case is_running() of
  true ->
   application:stop(janet_simulator),
   application:stop(mnesia),
   timer:sleep(5),   % For output ordering purposes (not necessary if the primary logger level is configured to "warning")
   io:format("JANET Simulator stopped~n");
  false ->
   {error,not_running}
 end.
 
 
%% --- Mnesia Utility Functions --- %%  

% Starts the Mnesia Database, waiting for its tables to be loaded from disc
start_mnesia() ->
 application:start(mnesia),
 mnesia:wait_for_tables([location,sublocation,device],5000).
 
 
%% --- Other Utility Functions --- %% 
 
% Check if an application is running (default: janet_simulator)
is_running() ->
 is_running(janet_simulator).
is_running(AppName) ->
 case [App || {App, _, _} <- application:which_applications(), App =:= AppName] of
  [AppName] ->
   true;
  [] ->
   false
 end.
 

%% ========== Application Behaviour Callback Functions ========== %% 

% Start the Janet Simulator
start(normal, _Args) ->
 sup_jsim:start_link().
 
% Stop the Janet Simulator
stop(_State) ->
 ok.
 