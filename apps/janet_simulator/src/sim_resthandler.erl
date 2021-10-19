%% This module represents the REST handler in the JANET Simulator application %%

-module(sim_resthandler).
-behaviour(gen_resthandler).  % Custom REST handler behaviour

%% -------------------------- gen_resthandler BEHAVIOUR CALLBACK FUNCTIONS -------------------------- %%
-export([start_link/0,init_handler/1,init/2,err_to_code_msg/1,os_port_conflict/1]).

%% -------------------------------- RESOURCES AND OPERATIONS HANDLERS -------------------------------- %%
-export([res_loc_handler/1,                                      % /location/:loc_id resource handler
         add_location_handler/2,update_loc_name_handler/2,       %                   operation handlers
		 delete_location_handler/2]).   
-export([res_loc_subloc_handler/1,                               % /location/:loc_id/sublocation/:subloc_id resource handler
         update_subloc_name_handler/2]).                         %                                          operation handlers
-export([res_dev_handler/1,                                      % /device/:dev_id resource handler
         update_dev_name_handler/2]).                            %                 operation handlers
                                                                 
%%====================================================================================================================================
%%                                                GEN_RESTHANDLER CALLBACK FUNCTIONS                                                        
%%====================================================================================================================================

%% ======================================================== INIT_HANDLER ======================================================== %%
init_handler(_) ->
 
 % Retrieve the 'sim_rest_port' environment variable
 {ok,SimRESTPort} = application:get_env(sim_rest_port),
 
 % Define the REST listener name
 ListenerName = sim_resthandler,
 
 % Define the list resource paths supported by this handler
 %
 % Paths = [{Path,CallbackModule,InitialState}]        
 %
 Paths = [
          % RESOURCE: /location/:loc_id
		  % ALLOWED METHODS: 
          %   - PUT    -> add_location(Loc_id,Name,User,Port,HostName)
          %   - POST   -> update_loc_name(Loc_id,Name)
          %   - DELETE -> delete_location(Loc_id)
		  %
          {"/location/:loc_id",?MODULE,res_loc_handler},						
		  
          % RESOURCE: /location/:loc_id/sublocation/:subloc_id
		  % ALLOWED METHODS: 
          %   - POST   -> update_subloc_name({Loc_id,Subloc_id},Name)
		  %		  
		  {"/location/:loc_id/sublocation/:subloc_id",?MODULE,res_loc_subloc_handler},
		
          % RESOURCE: /device/:dev_id
		  % ALLOWED METHODS: 
          %   - POST   -> update_dev_name(Dev_id,Name)
		  %		  		
		  {"/device/:dev_id",?MODULE,res_dev_handler}
	     ],
			
 % Return the initialization tuple to the behaviour engine
 {ok,SimRESTPort,ListenerName,Paths}.
 
 
%% ============================================================ INIT ============================================================ %%
init(Req,ResHandlerName) ->

 % Handle the HTTP request so to obtain the
 % HTTP response to be returned to the client
 ReplyReq = gen_resthandler:handle_req(?MODULE,ResHandlerName,Req),
 
 % Return the HTTP response to the client
 {ok,ReplyReq,ResHandlerName}.

%% ======================================================= ERR_TO_CODE_MSG ======================================================= %%

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

% Trying to deploy the location controller on an unallowed nodes host
err_to_code_msg({invalid_hostname,HostName}) ->
 {406,io_lib:format("<ERROR> The specified \"hostname\" (~s) does not belong to the list of allowed hosts where JANET nodes can be deployed in",[HostName])};

% The location was added into the database, but an internal error occured in starting its controller node
err_to_code_msg({controller_not_started,InternalError}) ->
 {500,io_lib:format("<SERVER ERROR> The location was added, but an internal error occured in starting its controller node: ~w",[InternalError])};
 
%% UPDATE_LOC_NAME + DELETE_LOCATION (POST /location/:loc_id + DELETE /location/:loc_id)
%% ---------------------------------    
% Trying to operate on a location that does not exist
err_to_code_msg({location_not_exists,Loc_id}) ->
 {404,io_lib:format("<ERROR> A location with such \"loc_id\" (~w) does not exist",[Loc_id])};

%% DELETE_LOCATION (DELETE /location/:loc_id)
%% ---------------
% The location along with all its sublocations and devices were deleted from 
% the database, but an internal error occured in stopping their associated nodes
err_to_code_msg({stop_location_nodes_error,Error}) ->
 {500,io_lib:format("<SERVER ERROR> The location along with all its sublocations and devices were deleted from the database, but an internal error occured in stopping their associated nodes: ~w",[Error])};

%% UPDATE_SUBLOC_NAME (POST /location/:loc_id/sublocation/:subloc_id)
%% ------------------
% Trying to update the name of a sublocation that does not exist
err_to_code_msg({sublocation_not_exists,{Loc_id,Subloc_id}}) ->
 {404,io_lib:format("<ERROR> A sublocation with such \"sub_id\" ({~w,~w}) does not exist",[Loc_id,Subloc_id])};
 
%% UPDATE_DEV_NAME (POST /device/:dev_id)
%% ---------------
% Trying to update the name of a device that does not exist
err_to_code_msg({device_not_exists,Dev_id}) ->
 {404,io_lib:format("<ERROR> A device with such \"dev_id\" (~w) does not exist",[Dev_id])};
 
%% UNKNOWN ERROR
%% ------------- 
err_to_code_msg(UnknownError) ->
 {500,io_lib:format("<UNKNOWN SERVER ERROR> Unknown error: ~p",[UnknownError])}.


%% ====================================================== OS_PORT_CONFLICT ====================================================== %% 
os_port_conflict(RESTPort) ->

 % Report that the REST server of the JANET Simulator application will not be reachable by the remote client during
 % the execution (which should NOT happen since the availability of such port was checked at the application startup)
 io:format("[sim_resthandler]: <PORT CONFLICT> Port \"~w\" is not available in the host OS, the JANET Simulator REST server will NOT be reachable by the remote client~n",[RESTPort]),

 % Return the atom 'ignore' so to prevent the 'sup_jsim' root supervisor from reattempting
 % to restart the process (which would be useless being the REST port unavailable) 
 ignore.
 
 
%%==================================================================================================================================%
%%                                                                                                                                  %
%%                                          GEN_RESTHANDLER RESOURCES AND OPERATIONS HANDLERS                                       %
%%                                                                                                                                  %
%%==================================================================================================================================%

%%=================================================================================================================================%%
%%                                                    RESOURCE: /location/:loc_id                                                  %% 
%%=================================================================================================================================%%

%% ALLOWED METHODS:
%% ---------------
%%   - PUT    -> add_location(Loc_id,Name,User,Port,HostName)
%%   - POST   -> update_loc_name(Loc_id,Name)
%%   - DELETE -> delete_location(Loc_id)
%%
res_loc_handler(Req) ->
 
 % Define the binary list of HTTP methods allowed by this resource handler
 Allowed_Methods = [<<"PUT">>,<<"POST">>,<<"DELETE">>],
 
 % Ensure the HTTP request method to be included in the list of allowed methods
 Method = gen_resthandler:get_check_method(Req,Allowed_Methods),
 
 % Retrieve the "Loc_id" path binding parameter
 Loc_id = gen_resthandler:get_check_int_binding(Req,loc_id,1),

 % Determine the name and the expected body parameters of the operation handler associated with
 % the request from its HTTP method, with the latters being defined using the following syntax:
 %
 % - List/String parameters: {ParamName,'list','required'/'optional'}  % Required or optional
 % - Integer parameters:     {ParamName,'integer',MinValue}            % Always required and must be >= a MinValue
 % 
 {OpHandlerName,ExpBodyParams} =
 case Method of
 
  % PUT -> add_location(Loc_id,Name,User,Port,HostName)
  <<"PUT">> ->
   {add_location_handler,    % Operation handler name
    [
     {name,list,optional},     % "Name" parameter     (optional)
     {user,list,optional},     % "User" parameter     (optional)
     {port,integer,30000},     % "Port" parameter     (>= 30000)
	 {hostname,list,required}  % "HostName" parameter (required)
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
add_location_handler(Req,[Loc_id,Name,User,Port,HostName]) ->
 
 % Attempt to add the location and start its associated controller node
 case jsim:add_location(Loc_id,Name,User,Port,HostName) of
  {ok,ok} ->
   
   % If the location was added and its controller node was started, report the success of the operation
   io:format("[~p]: Added location (loc_id = ~w, name = ~p, user = ~p, port = ~w, hostname = ~s)~n",[?FUNCTION_NAME,Loc_id,Name,User,Port,HostName]),
   
   % Define the success HTTP response to be replied to the client
   cowboy_req:reply(201,Req);
  
  %% ------------------------------------ Operation Errors ------------------------------------ %   
  
  % Trying to add a location that already exists
  {error,location_already_exists} ->
   throw({location_already_exists,Loc_id});
  
  % Trying to add a location whose port is already assigned to another controller  
  {error,port_already_taken} ->
   throw({port_already_taken,Port});

  % Trying to add a location whose port is currently unavailable in the host OS
  {error,host_port_taken} ->
   throw({host_port_taken,Port});
   
  % Trying to deploy the location controller on an unallowed nodes host
  {error,invalid_hostname} ->
   throw({invalid_hostname,HostName});
   
  % The location was added into the database, but an
  % internal error occured in starting its controller node
  {ok,InternalError} ->
   throw({controller_not_started,InternalError}) 
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
  
  %% ------------------------------------ Operation Errors ------------------------------------ %  
  
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
   io:format("[~p]: Deleted location (loc_id = ~w)~n",[?FUNCTION_NAME,Loc_id]),
   
   % Define the success HTTP response to be replied to the client
   cowboy_req:reply(204,Req);
  
  %% ------------------------------------ Operation Errors ------------------------------------ %   
  
  % Trying to delete a location that does not exist
  {error,location_not_exists} ->
   throw({location_not_exists,Loc_id});
    
  % The location along with all its sublocations and devices were deleted from 
  % the database, but an internal error occured in stopping their associated nodes
  {ok,Error} ->
   throw({stop_location_nodes_error,Error}) 
 end. 
 
 
%%=================================================================================================================================%%
%%                                           RESOURCE: /location/:loc_id/sublocation/:subloc_id                                    %% 
%%=================================================================================================================================%% 
 
%% ALLOWED METHODS:
%% ---------------
%%   - POST   -> update_subloc_name({Loc_id,Subloc_id},Name)
%% 
res_loc_subloc_handler(Req) ->
 
 % Define the binary list of HTTP methods allowed by this resource handler
 Allowed_Methods = [<<"POST">>],
 
 % Ensure the HTTP request method to be included in the list of allowed methods
 Method = gen_resthandler:get_check_method(Req,Allowed_Methods),
 
 % Retrieve the "Loc_id" path binding parameter
 Loc_id = gen_resthandler:get_check_int_binding(Req,loc_id,1),
 
 % Retrieve the "Subloc_id" path binding parameter
 Subloc_id = gen_resthandler:get_check_int_binding(Req,subloc_id,1),  % "1" because the name of the default sublocation cannot be changed
 
 % Determine the name and the expected body parameters of the operation handler associated with
 % the request from its HTTP method, with the latters being defined using the following syntax:
 %
 % - List/String parameters: {ParamName,'list','required'/'optional'}  % Required or optional
 % - Integer parameters:     {ParamName,'integer',MinValue}            % Always required and must be >= a MinValue
 % 
 {OpHandlerName,ExpBodyParams} =
 case Method of
 
  % POST -> update_subloc_name({Loc_id,Subloc_id},Name)
  <<"POST">> ->
   {
    update_subloc_name_handler, % Operation handler name
	[{name,list,required}]      % "Name" parameter (required)
   }
 end,
 
 % Return the name of the operation handler, the list of path bindings parameters and
 % the list of expected parameters to be retrieved from the body of the HTTP request
 {OpHandlerName,[{Loc_id,Subloc_id}],ExpBodyParams}.

%% ===================================================== OPERATION HANDLERS ===================================================== %% 
 
%% UPDATE_SUBLOC_NAME (POST /location/:loc_id/sublocation/:subloc_id)
%% ==================
update_subloc_name_handler(Req,[{Loc_id,Subloc_id},Name]) ->
 
 % Attempt to update the sublocation name
 case jsim:update_subloc_name({Loc_id,Subloc_id},Name) of
  ok ->
   
   % If the sublocation name was updated, report the success of the operation
   io:format("[~p]: Updated sublocation name (sub_id = {~w,~w}, name = ~p)~n",[?FUNCTION_NAME,Loc_id,Subloc_id,Name]),
   
   % Define the success HTTP response to be replied to the client
   cowboy_req:reply(204,Req);
  
  %% ------------------------------------ Operation Errors ------------------------------------ %  
  
  % Trying to update the name of a sublocation that does not exist
  {error,sublocation_not_exists} ->
   throw({sublocation_not_exists,{Loc_id,Subloc_id}})
 end.


%%=================================================================================================================================%%
%%                                                     RESOURCE: /device/:dev_id/                                                  %% 
%%=================================================================================================================================%% 
 
%% ALLOWED METHODS:
%% ---------------
%%   - POST   -> update_dev_name(Dev_id,Name)
%% 
res_dev_handler(Req) ->
 
 % Define the binary list of HTTP methods allowed by this resource handler
 Allowed_Methods = [<<"POST">>],
 
 % Ensure the HTTP request method to be included in the list of allowed methods
 Method = gen_resthandler:get_check_method(Req,Allowed_Methods),
 
 % Retrieve the "Dev_id" path binding parameter
 Dev_id = gen_resthandler:get_check_int_binding(Req,dev_id,1),

 % Determine the name and the expected body parameters of the operation handler associated with
 % the request from its HTTP method, with the latters being defined using the following syntax:
 %
 % - List/String parameters: {ParamName,'list','required'/'optional'}  % Required or optional
 % - Integer parameters:     {ParamName,'integer',MinValue}            % Always required and must be >= a MinValue
 % 
 {OpHandlerName,ExpBodyParams} =
 case Method of
 
  % POST -> update_subloc_name({Loc_id,Subloc_id},Name)
  <<"POST">> ->
   {
    update_dev_name_handler,  % Operation handler name
	[{name,list,required}]    % "Name" parameter (required)
   }
 end,
 
 % Return the name of the operation handler, the list of path bindings parameters and
 % the list of expected parameters to be retrieved from the body of the HTTP request
 {OpHandlerName,[Dev_id],ExpBodyParams}.

%% ===================================================== OPERATION HANDLERS ===================================================== %% 
 
%% UPDATE_DEV_NAME (POST /device/:dev_id)
%% ===============
update_dev_name_handler(Req,[Dev_id,Name]) ->
 
 % Attempt to update the device name
 case jsim:update_dev_name(Dev_id,Name) of
  ok ->
   
   % If the device name was updated, report the success of the operation
   io:format("[~p]: Updated device name (dev_id = ~w, name = ~p)~n",[?FUNCTION_NAME,Dev_id,Name]),
   
   % Define the success HTTP response to be replied to the client
   cowboy_req:reply(204,Req);
  
  %% ------------------------------------ Operation Errors ------------------------------------ %  
  
  % Trying to update the name of a device that does not exist
  {error,device_not_exists} ->
   throw({device_not_exists,Dev_id})
 end.


%%====================================================================================================================================
%%                                                         START FUNCTION                                                        
%%====================================================================================================================================

%% Called by the Janet Simulator top-level supervisor (sup_jsim) at boot time
start_link() ->
 gen_resthandler:start_link(?MODULE,[]).