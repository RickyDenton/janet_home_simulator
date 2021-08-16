-module(jsim).
-export([start/0,stop/0]).                                      								    % Application Start and Stop
-export([start_mnesia/0,stop_mnesia/0,db_info/0,db_reset/0,db_addlocation/4,db_findlocation/1]).    % Mnesia Utility Functions
-export([is_running/0,is_running/1]). 						     									% Other Utility Functions
-export([start/2,stop/1]). 										 									% Application Behaviour Callback Functions
-behaviour(application).
 
%% ========== JANET Simulator API ========== %%

%% --- Application start and stop --- %% 

% Start the Application TODO: Create hidden node!
start() ->
 case is_running() of
  true ->
   {error, already_started};
  false ->
   % logger:set_primary_config(#{level => warning}),    % Uncomment before release (hides == APPLICATION INFO === messages when applications are stopped)
   start_mnesia(),
   application:start(janet_simulator)
 end.
 
% Stop the Application
stop() ->
 case is_running() of
  true ->
   application:stop(janet_simulator),
   stop_mnesia(),
   timer:sleep(5),   % For output ordering purposes (not necessary if the primary logger level is configured to "warning")
   io:format("JANET Simulator stopped~n");
  false ->
   {error, not_started}
 end.
 
 
%% --- Mnesia Utility Functions --- %%  

% Starts the Mnesia Database used by the application
start_mnesia() ->
 application:set_env(mnesia,dir,"db/mnesia.db"),
 application:start(mnesia),
 mnesia:wait_for_tables([location,sublocation,device],5000).
 
% Stops the Mnesia Database used by the application
stop_mnesia() ->
 application:stop(mnesia),
 application:unset_env(mnesia,dir,"db/mnesia.db").

% Adds a location to the database (manually, debugging purposes)
db_addlocation(Id,Port,Name,User) ->
 db_utils:addlocation(Id,Port,Name,User).

% Finds a location in the database (manually, debugging purposes) 
db_findlocation(Id) ->
 db_utils:findlocation(Id).

% Returns the database information
db_info() ->
 db_utils:info().
 
% Resets the database
db_reset() ->
 db_utils:reset().
 
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
 