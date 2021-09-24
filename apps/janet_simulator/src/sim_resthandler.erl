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

 % Initialize the list of resource paths accepted by Cowboy as of the JANET Simulator REST interface
 %
 % Paths = [{Path,CallbackModule,InitialState}]        
 %
 Paths = [
          % add_location(), update_loc_name(), delete_location()
          {"/location/:loc_id",sim_cowroute_loc,#{}},						
			 
	      % update_subloc_name()
		  {"/location/:loc_id/sublocation/:subloc:id",?MODULE,#{}},
			 
		  % update_dev_name()
		  {"/device/:dev_id",?MODULE,#{}}
	     ],
			 
 % Initialize the list of Cowboy routes by merging the list of resource
 % paths with the list of accepted hosts (the RemoteHost and localhost)
 Routes = [{"localhost",Paths},{RemoteHost,Paths}],
 
 % Compile the Cowboy Routes
 CompiledRoutes = cowboy_router:compile(Routes),
 
 % Start the Cowboy REST Server
 {ok,_} = cowboy:start_clear(sim_restserver,                          % Listener Name
                             [{port, RESTPort}],                      % Listener Port
							 #{env => #{dispatch => CompiledRoutes}}  % Listener Routes
							),

 % Inform that the Cowboy REST Server was successfully initialized
 % and return the gen_server initial (and constant) state	
 io:format("[sim_resthandler]: Cowboy REST Server successfully initialized~n"),
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

%% 0) INIT
%% ------- 
%% PURPOSE:     Initial HTTP Request callback
%% MODIFIES:    -
%% RETURNS:     Inform Cowboy to handle the request via its REST state machine
%% IF NO MATCH: -
%%
init(Req,State) ->

 % Return an atom informing Cowboy to handle
 % the HTTP request via its REST state machine
 {cowboy_rest,Req,State}.


%% 1) KNOWN_METHODS
%% ---------------- 
%% PURPOSE:       Defines the list of HTTP methods supported by the JANET Simulator REST Server
%% MODIFIES:      -
%% RETURNS:       The list of HTTP methods supported by the JANET Simulator REST Server
%% MISMATCH RESP: 501 (NOT IMPLEMENTED)
%%
known_methods(Req,State) ->

 % Define the list of HTTP methods supported by the JANET Simulator REST Server
 %
 % NOTE: Cowboy provides a default implementation of
 %       the "OPTIONS" method which returns this list
 %
 Known_Methods = [<<"PUT">>,<<"POST">>,<<"DELETE">>,<<"OPTIONS">>],
 
 % Return the list of known methods
 {Known_Methods, Req, State}.
 
 
%% 2) ALLOWED_METHODS
%% ------------------
%% PURPOSE:       Defines the list of HTTP methods allowed by the JANET Simulator REST Server
%% MODIFIES:      -
%% RETURNS:       The list of HTTP methods allowed by the JANET Simulator REST Server
%% MISMATCH RESP: 405 (METHOD NOT ALLOWED)
%%
allowed_methods(Req,State) ->

 % Define the list of HTTP methods allowed by the JANET Simulator REST Server
 %
 % NOTE: Cowboy provides a default implementation of
 %       the "OPTIONS" method which returns this list
 %
 Allowed_Methods = [<<"PUT">>,<<"POST">>,<<"DELETE">>,<<"OPTIONS">>],
 
 % Return the list of allowed methods
 {Allowed_Methods, Req, State}.
 
 
%%====================================================================================================================================
%%                                                         START FUNCTION                                                        
%%====================================================================================================================================

%% Called by the Janet Simulator top-level supervisor (sup_jsim) at boot time
start_link() ->
 gen_server:start_link(?MODULE,[],[]).