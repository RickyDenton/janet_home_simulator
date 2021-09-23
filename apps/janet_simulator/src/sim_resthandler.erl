%% This module represents the REST handler in the JANET Simulator application %%
%%
%% NOTE: Even if not supposed to serve call or cast requests this module was implemented as a gen_server so to allow for
%%       an orderly startup and shutdown of the Cowboy REST server as part of the JANET Simulator supervision tree (also
%%       given the fact that the overhead of using a gen_server with respect to a barebone custom OTP-compliant process
%%       implemented via the "proc_lib" library is neglegible, see: https://erlang.org/doc/design_principles/spec_proc.html)

-module(sim_resthandler).
-behaviour(gen_server).

-export([start_link/0,init/1,terminate/2,handle_call/3,handle_cast/2]).  % gen_server Behaviour Callback Functions
-export([init/2]).                                                       % Cowboy Callback Functions

%% SrvState: null (constant, stateless server)

%%====================================================================================================================================
%%                                                  GEN_SERVER CALLBACK FUNCTIONS                                                        
%%====================================================================================================================================

%% ============================================================ INIT ============================================================ %%
init(_) ->

 % Trap exit signals so to allow cleanup operations when terminating (terminate(Reason,SrvState) callback function)
 process_flag(trap_exit,true),
 
 % Retrieve the 'rest_port' and 'remote_host' environment variables
 {ok,RESTPort} = application:get_env(rest_port),
 {ok,RemoteHost} = application:get_env(remote_host),
 
 % Start the Ranch TCP acceptor required by the Cowboy REST server
 application:start(ranch),



 Greater0 =
 fun 
  (forward,X) when X > 0 ->
   {ok,X};
  (forward,_) ->
   {error,non_greater_zero};
  (format_error,{non_greater_zero,X})  ->
   io_lib:format("~p is not > 0", [X])
 end,

 GreaterEqual0 =
 fun 
  (forward,X) when X >= 0 ->
   {ok,X};
  (forward,_) ->
   {error,non_greater_equal_zero};
  (format_error,{non_greater_equal_zero,X})  ->
   io_lib:format("~p is not >= 0", [X])
 end,


 % Initialize the list of resource paths accepted by Cowboy as of the JANET Simulator REST interface
 %
 % Paths = [{Path,Constraints,CallbackModule,InitialState}]        
 %
 Paths = [
          % add_location(), update_loc_name(), delete_location()
          {"/location/:loc_id",[{loc_id,[int,Greater0]}],?MODULE,[]},						
			 
	      % update_subloc_name()
		  {"/location/:loc_id/sublocation/:subloc:id",[{loc_id,[int,Greater0]},{subloc_id,[int,GreaterEqual0]}],?MODULE,[]},
			 
		  % update_dev_name()
		  {"/device/:dev_id",[{dev_id,[int,Greater0]}],?MODULE,[]}
	     ],
			 
 % Initialize the list of Cowboy routes by merging the list of resource
 % paths with the list of accepted hosts (the RemoteHost and localhost)
 Routes = [{"localhost",Paths},{RemoteHost,Paths}],
 
 % Compile the Cowboy Routes
 CompiledRoutes = cowboy_router:compile(Routes),
 
   
   
   
 % Cowboy
 %Dispatch = cowboy_router:compile([ {'_', [{"/",sim_resthandler, []}]} ]),
 {ok,_} = cowboy:start_clear(sim_restserver,[{port, RESTPort}],#{env => #{dispatch => CompiledRoutes}}),
	
	
 io:format("[sim_resthandler]: Initialized~n"),
 {ok,null}.  	
 

%% ===================================================== HANDLE_CALL (STUB) ===================================================== %% 

%% This represents a STUB of the handle_call() callback function, whose
%% definition is formally required by the 'gen_server' OTP behaviour
handle_call(Request,From,SrvState) ->
 
 % Report that this gen_server should not receive call requests
 io:format("[sim_resthandler]: <WARNING> Unexpected call (Request = ~w, From = ~w, SrvState = ~w)~n",[Request,From,SrvState]),

 % Reply an error and keep the SrvState
 {reply,{error,unsupported},SrvState}.


%% ===================================================== HANDLE_CAST (STUB) ===================================================== %% 

%% This represents a STUB of the handle_cast() callback function, whose
%% definition is formally required by the 'gen_server' OTP behaviour
handle_cast(Request,SrvState) ->
 
 % Report that this gen_server should not receive cast requests
 io:format("[sim_resthandler]: <WARNING> Unexpected cast (Request = ~w, SrvState = ~w)~n",[Request,SrvState]),

 % Keep the SrvState
 {noreply,SrvState}.


%% ========================================================== TERMINATE ========================================================== %% 

%% Called when the sim_resthandler is asked to shutdown by its 'sup_jsim' supervisor (which happens
%% when restarting the tree due to a crash or when shutting down the JANET Simulator application)
terminate(_,_) ->

 % Stop the JANET Simulator REST server by
 % deregistering the sim_resthandler listener 
 ok = cowboy:stop_listener(sim_restserver).


%%====================================================================================================================================
%%                                               COWBOY REST SERVER CALLBACK FUNCTIONS                                                        
%%====================================================================================================================================

init(Req0, State) ->
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/plain">>},
        <<"Hello Erlang!">>,
        Req0),
    {ok, Req, State}.


%%====================================================================================================================================
%%                                                         START FUNCTION                                                        
%%====================================================================================================================================

%% Called by the Janet Simulator top-level supervisor (sup_jsim) at boot time
start_link() ->
 gen_server:start_link(?MODULE,[],[]).