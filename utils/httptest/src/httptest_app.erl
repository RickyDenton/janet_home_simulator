%% This is the callback module of the HTTP Test (httptest) application %%

-module(httptest_app).
-behaviour(application).

-export([start/2, stop/1]).

%%====================================================================================================================================
%%                                             APPLICATION BEHAVIOUR CALLBACK FUNCTIONS                                                        
%%====================================================================================================================================

%% Starts the HTTP Test Server
start(_StartType, _StartArgs) ->

 % Start all application dependencies
 {ok,_StartedCowboyApps} = application:ensure_all_started(cowboy),
 {ok,_StartedJSONEApps} = application:ensure_all_started(jsone),

 % Define and compile the routes to listen on
 Dispatch = cowboy_router:compile([{'_',[{'_',httptest,[]}]}]),
 
 % Start the Cowboy listener
 {ok, _} = cowboy:start_clear(httptest,
                              [{port,50505}],
                              #{
							    env => #{dispatch => Dispatch},
							    request_timeout => infinity   % Gun connection persistence
							   }
                             ),

 % Call the root supervisor 
 httptest_sup:start_link().


%% Stops the HTTP Test Server
stop(_State) ->

 % Stop the cowboy listnener
 ok = cowboy:stop_listener(httptest).

