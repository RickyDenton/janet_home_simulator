%% This module represents the REST handler in the JANET Simulator application %%
%%
%% NOTE: Even if not supposed to serve call or cast requests this module was implemented as a gen_server so to allow for
%%       an orderly startup and shutdown of the Cowboy REST server as part of the JANET Simulator supervision tree (also
%%       given the fact that the overhead of using a gen_server with respect to a barebone custom OTP-compliant process
%%       implemented via the "proc_lib" library is neglegible, see: https://erlang.org/doc/design_principles/spec_proc.html)

-module(sim_resthandler).
-behaviour(gen_server).

%% --------------------------------- GEN_SERVER CALLBACK FUNCTIONS --------------------------------- %%
-export([start_link/0,init/1,terminate/2,handle_call/3,handle_cast/2]).

%% SrvState: null (constant, stateless server)

%% ------------------------------- REST HANDLERS CALLBACK FUNCTIONS ------------------------------- %%
-export([init/2]).                  						% Cowboy root handler callback
-export([res_loc_handler/1]).								% REST resource handlers
-export([add_location_handler/2,update_loc_name_handler/2,  % REST operation handlers
         delete_location_handler/2,err_to_code_msg/1]).  

-include("reqerror.hrl").           % REST error handler record


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
          {"/location/:loc_id",?MODULE,res_loc_handler},						
			 
	      % update_subloc_name()
		  {"/location/:loc_id/sublocation/:subloc_id",?MODULE,res_loc_subloc_handler},
			 
		  % update_dev_name()
		  {"/device/:dev_id",?MODULE,res_dev_handler}
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
%%                                                 REST HANDLERS CALLBACK FUNCTIONS                                                        
%%====================================================================================================================================

%% ============================================================ INIT ============================================================ %%
init(Req,ResHandlerName) ->

 % Handle the HTTP request so to obtain the "ReplyReq"
 % HTTP response to be returned to the client
 ReplyReq = try rest_utils:handle_req(?MODULE,ResHandlerName,Req)
 catch
 
  % If an error was raised during the handling of the HTTP request, determine
  % the HTTP error response to be returned to the client depending on whether
  % the error was handled (i.e. it is structured as a 'reqerror' record) or not
  
  % Handled error
  _:HandlerError when is_record(HandlerError,reqerror) -> 
   rest_utils:get_error_response(HandlerError);

  % Unhandled error (should NOT happen)
  _:UnexpectedError ->
   rest_utils:get_unexpected_error_response(UnexpectedError,Req)
 end,
 
 % Return the HTTP response to the client
 {ok,ReplyReq,ResHandlerName}.


%% ====================================================== RESOURCE HANDLERS ====================================================== %%

%% RESOURCE:   /location/:loc_id
%%
%% OPERATIONS: - PUT    -> add_location(Loc_id,Name,User,Port)
%%             - POST   -> update_loc_name(Loc_id,Name)
%%             - DELETE -> delete_location(Loc_id)
%%
res_loc_handler(Req) ->
 
 % Define the binary list of HTTP methods allowed by this resource handler
 Allowed_Methods = [<<"PUT">>,<<"POST">>,<<"DELETE">>],
 
 % Ensure the HTTP request method to be included in the list of allowed methods
 Method = rest_utils:get_check_method(Req,Allowed_Methods),
 
 % Retrieve the "Loc_id" path binding parameter
 Loc_id = rest_utils:get_check_int_binding(Req,loc_id,1),

 % Determine the name and the expected body parameters of the operation handler associated with
 % the request from its HTTP method, with the latters being defined using the following syntax:
 %
 % - List/String parameters: {ParamName,'list','required'/'optional'}  % Required or optional
 % - Integer parameters:     {ParamName,'integer',MinValue}            % Always required and must be >= a MinValue
 % 
 {OpHandlerName,ExpBodyParams} =
 case Method of
 
  % PUT -> add_location(Loc_id,Name,User,Port)
  <<"PUT">> ->
   {add_location_handler,    % Operation handler name
    [
     {name,list,optional},   % "Name" parameter (optional)
     {user,list,optional},   % "User" parameter (optional)
     {port,integer,30000}    % "Port" parameter (>= 30000)
	]
   };

  % POST -> update_loc_name(Loc_id,Name)
  <<"POST">> ->
   {
    update_loc_name_handler, % Operation handler name
	[{name,list,required}]   % "Name" parameter (required)
   };
   
  % DELETE -> delete_location(Loc_id)
  <<"DELETE">> ->
   {delete_location_handler, % Operation handler name
    []                       % No body parameters required
   }
 end,
 
 % Return the name of the operation handler, the list of path bindings parameters and
 % the list of expected parameters to be retrieved from the body of the HTTP request
 {OpHandlerName,[Loc_id],ExpBodyParams}.
 
 
%% ===================================================== OPERATION HANDLERS ===================================================== %%

%% ADD_LOCATION (PUT /location/:loc_id)
%% ============
add_location_handler(Req,[Loc_id,Name,User,Port]) ->
 
 % Attempt to add the location and start its associated controller node
 case jsim:add_location(Loc_id,Name,User,Port) of
  {ok,ok} ->
   
   % If the location was added and its controller node was started, report the success of the operation
   io:format("[~p]: Added location (loc_id = ~w, name = ~p, user = ~p, port = ~w)~n",[?FUNCTION_NAME,Loc_id,Name,User,Port]),
   
   % Define the success HTTP response to be replied to the client
   cowboy_req:reply(201,Req);
  
  %% --------------------------------- Operation Handler Errors --------------------------------- %   
  
  % Trying to add a location that already exists
  {error,location_already_exists} ->
   throw({location_already_exists,Loc_id});
  
  % Trying to add a location whose port is already assigned to another controller  
  {error,port_already_taken} ->
   throw({port_already_taken,Port});

  % Trying to add a location whose port is currently unavailable in the host OS
  {error,host_port_taken} ->
   throw({host_port_taken,Port});
   
  % The location was added into the database, but an
  % internal error occured in starting its controller node
  {ok,Error} ->
   throw({controller_not_started,Error}) 
 end.
 
 
%% UPDATE_LOC_NAME (POST /location/:loc_id)
%% ===============
update_loc_name_handler(Req,[Loc_id,Name]) ->
 
 % Attempt to update the location name
 case jsim:update_loc_name(Loc_id,Name) of
  ok ->
   
   % If the location name was updated, report the success of the operation
   io:format("[~p]: Updated location name (loc_id = ~w, name = ~p)~n",[?FUNCTION_NAME,Loc_id,Name]),
   
   % Define the success HTTP response to be replied to the client
   cowboy_req:reply(204,Req);
  
  %% --------------------------------- Operation Handler Errors --------------------------------- %   
  
  % Trying to update the name of a location that does not exist
  {error,location_not_exists} ->
   throw({location_not_exists,Loc_id})
 end.


%% DELETE_LOCATION (DELETE /location/:loc_id)
%% ===============
delete_location_handler(Req,[Loc_id]) ->
 
 % Attempt to delete the location, along with all its sublocations and
 % devices, also stopping their associated controller and devices nodes
 case jsim:delete_location(Loc_id) of
  {ok,ok} ->
   
   % If the location was deleted, report the success of the operation
   io:format("[~p]: Deleted Location (loc_id = ~w)~n",[?FUNCTION_NAME,Loc_id]),
   
   % Define the success HTTP response to be replied to the client
   cowboy_req:reply(204,Req);
  
  %% --------------------------------- Operation Handler Errors --------------------------------- %   
  
  % Trying to delete a location that does not exist
  {error,location_not_exists} ->
   throw({location_not_exists,Loc_id});
    
  % The location along with all its sublocations and devices were deleted from 
  % the database, but an internal error occured in stopping their associated nodes
  {ok,Error} ->
   throw({stop_location_nodes_error,Error}) 
 end.

%% ================================================== OPERATION HANDLERS ERRORS ================================================== %%

%% ADD_LOCATION (PUT /location/:loc_id) 
%% ------------
% Trying to add a location that already exists
err_to_code_msg({location_already_exists,Loc_id}) ->
 {409,io_lib:format("<ERROR> A location with such \"loc_id\" (~w) already exists",[Loc_id])};
 
% Trying to add a location whose port is already assigned to another controller
err_to_code_msg({port_already_taken,Port}) ->
 {412,io_lib:format("<ERROR> The specified \"port\" (~w) is already assigned to another location controller",[Port])};

% Trying to add a location whose port is currently unavailable in the host OS
err_to_code_msg({host_port_taken,Port}) ->
 {412,io_lib:format("<ERROR> The specified \"port\" (~w) is currently unavailable in the host OS",[Port])};
 
% The location was added into the database, but an internal error occured in starting its controller node
err_to_code_msg({controller_not_started,Error}) ->
 {500,io_lib:format("<SERVER ERROR> The location was added, but an internal error occured in starting its controller node: ~w",[Error])};
 
%% UPDATE_LOC_NAME (POST /location/:loc_id) + DELETE_LOCATION (DELETE /location/:loc_id)
%% ---------------                            ---------------    
% Trying to operate on a location that does not exist
err_to_code_msg({location_not_exists,Loc_id}) ->
 {404,io_lib:format("<ERROR> A location with such \"loc_id\" (~w) does not exist",[Loc_id])};

%% DELETE_LOCATION (DELETE /location/:loc_id)
%% ---------------
% The location along with all its sublocations and devices were deleted from 
% the database, but an internal error occured in stopping their associated nodes
err_to_code_msg({stop_location_nodes_error,Error}) ->
 {500,io_lib:format("<SERVER ERROR> The location along with all its sublocations and devices were deleted from the database, but an internal error occured in stopping their associated nodes: ~w",[Error])};

%% UNKNOWN ERROR
%% ------------- 
err_to_code_msg(UnknownError) ->
 {500,io_lib:format("<SERVER ERROR> Unknown error: ~p",[UnknownError])}.
 
 
%%====================================================================================================================================
%%                                                         START FUNCTION                                                        
%%====================================================================================================================================

%% Called by the Janet Simulator top-level supervisor (sup_jsim) at boot time
start_link() ->
 gen_server:start_link(?MODULE,[],[]).