%% This module represents the REST handler in the JANET Controller application %%

-module(ctr_resthandler).
-behaviour(gen_resthandler).  % Custom REST handler behaviour

%% -------------------------- gen_resthandler BEHAVIOUR CALLBACK FUNCTIONS -------------------------- %%
-export([start_link/0,init_handler/1,init/2,err_to_code_msg/1]). % gen_resthandler behaviour callback functions

%% -------------------------------- RESOURCES AND OPERATIONS HANDLERS -------------------------------- %%
-export([res_subloc_handler/1,                  % /sublocation/:subloc_id resource handler
         add_sublocation_handler/2,             %                         operation handlers
		 delete_sublocation_handler/2]).        %                         
-export([res_dev_handler/1,                     % /device/:dev_id resource handler
         add_device_handler/2]).                %                 operation handlers
		    
%%====================================================================================================================================
%%                                                GEN_RESTHANDLER CALLBACK FUNCTIONS                                                        
%%====================================================================================================================================

%% ======================================================== INIT_HANDLER ======================================================== %%
init_handler(_) ->
 
 % Retrieve the 'rest_port' and 'remote_host' environment variables
 {ok,RESTPort} = application:get_env(rest_port),
 {ok,RemoteHost} = application:get_env(remote_host),
 
 % Define the REST listener name
 ListenerName = ctr_resthandler,
 
 % Define the list resource paths supported by this handler
 %
 % Paths = [{Path,CallbackModule,InitialState}]        
 %
 Paths = [
          % RESOURCE: /sublocation/:subloc_id
		  % ALLOWED METHODS: 
          %   - PUT    -> add_sublocation({Loc_id,Subloc_id},Name)
          %   - DELETE -> delete_sublocation({Loc_id,Subloc_id})
		  %
          {"/sublocation/:subloc_id",?MODULE,res_subloc_handler},						
		
          % RESOURCE: /device/:dev_id
		  % ALLOWED METHODS: 
          %   - PUT    -> add_device(Dev_id,Name,{Loc_id,Subloc_id},Type)
		  %   - POST   -> update_dev_subloc(Dev_id,{Loc_id,Subloc_id})
          %   - DELETE -> delete_device(Dev_id)
		  %		
		  {"/device/:dev_id",?MODULE,res_dev_handler},
			 
		  % Device commands
		  %% [TODO]
		  {"/device",?MODULE,res_devcommand_handler}
	     ],
			
 % Return the initialization tuple to the behaviour engine
 {ok,RESTPort,RemoteHost,ListenerName,Paths}.
 
 
%% ============================================================ INIT ============================================================ %%
init(Req,ResHandlerName) ->

 % Handle the HTTP request so to obtain the
 % HTTP response to be returned to the client
 ReplyReq = gen_resthandler:handle_req(?MODULE,ResHandlerName,Req),
 
 % Return the HTTP response to the client
 {ok,ReplyReq,ResHandlerName}.

%% ======================================================= ERR_TO_CODE_MSG ======================================================= %%

%% ADD_SUBLOCATION (PUT /sublocation/:subloc_id) 
%% ---------------
% Trying to add a sublocation that already exists
err_to_code_msg({sublocation_already_exists,{Loc_id,Subloc_id}}) ->
 {409,io_lib:format("<ERROR> A sublocation with such \"sub_id\" ({~w,~w}) already exists",[Loc_id,Subloc_id])};
 
%% DELETE_SUBLOCATION + ADD_DEVICE (DELETE /sublocation/:subloc_id + PUT /device/:dev_id) 
%% -------------------------------
% Trying to operate on a sublocation that does not exist
err_to_code_msg({sublocation_not_exists,{Loc_id,Subloc_id}}) ->
 {404,io_lib:format("<ERROR> A sublocation with such \"sub_id\" ({~w,~w}) does not exist",[Loc_id,Subloc_id])}; 

%% ADD_DEVICE (PUT /device/:dev_id) 
%% ---------- 
% Trying to add a device of invalid type
err_to_code_msg({invalid_devtype,Type}) ->
 {400,io_lib:format("<ERROR> The specified device \"type\" (~p) is invalid",[Type])}; 

% Trying to add a device that already exists
err_to_code_msg({device_already_exists,Dev_id}) ->
 {409,io_lib:format("<ERROR> A device with such \"dev_id\" (~w) already exists",[Dev_id])};

% The device was added into the database, but an internal error occured in starting its node
err_to_code_msg({device_not_started,InternalError}) ->
 {500,io_lib:format("<SERVER ERROR> The device was added, but an internal error occured in starting its node: ~p",[InternalError])};

   


% Trying to add a location whose port is already assigned to another controller
err_to_code_msg({port_already_taken,Port}) ->
 {412,io_lib:format("<ERROR> The specified \"port\" (~w) is already assigned to another location controller",[Port])};

% Trying to add a location whose port is currently unavailable in the host OS
err_to_code_msg({host_port_taken,Port}) ->
 {412,io_lib:format("<ERROR> The specified \"port\" (~w) is currently unavailable in the host OS",[Port])};
 
% The location was added into the database, but an internal error occured in starting its controller node
err_to_code_msg({controller_not_started,Error}) ->
 {500,io_lib:format("<SERVER ERROR> The location was added, but an internal error occured in starting its controller node: ~w",[Error])};
 
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
 
%% UPDATE_DEV_NAME (POST /device/:dev_id)
%% ---------------
% Trying to update the name of a device that does not exist
err_to_code_msg({device_not_exists,Dev_id}) ->
 {404,io_lib:format("<ERROR> A device with such \"dev_id\" (~w) does not exist",[Dev_id])};
 
%% UNKNOWN ERROR
%% ------------- 
err_to_code_msg(UnknownError) ->
 {500,io_lib:format("<UNKNOWN SERVER ERROR> Unknown error: ~p",[UnknownError])}.
 
 
%%==================================================================================================================================%
%%                                                                                                                                  %
%%                                          GEN_RESTHANDLER RESOURCES AND OPERATIONS HANDLERS                                       %
%%                                                                                                                                  %
%%==================================================================================================================================%

%%=================================================================================================================================%%
%%                                                 RESOURCE: /sublocation/:subloc_id                                               %% 
%%=================================================================================================================================%%

%% ALLOWED METHODS:
%% ---------------
%%   - PUT    -> add_sublocation({Loc_id,Subloc_id},Name)
%%   - DELETE -> delete_sublocation({Loc_id,Subloc_id})
%%
res_subloc_handler(Req) ->
 
 % Define the binary list of HTTP methods allowed by this resource handler
 Allowed_Methods = [<<"PUT">>,<<"DELETE">>],
 
 % Ensure the HTTP request method to be included in the list of allowed methods
 Method = gen_resthandler:get_check_method(Req,Allowed_Methods),
 
 % Retrieve the "Subloc_id" path binding parameter
 Subloc_id = gen_resthandler:get_check_int_binding(Req,subloc_id,1),  % '1' because the default sublocation cannot be added or deleted

 % Determine the name and the expected body parameters of the operation handler associated with
 % the request from its HTTP method, with the latters being defined using the following syntax:
 %
 % - List/String parameters: {ParamName,'list','required'/'optional'}  % Required or optional
 % - Integer parameters:     {ParamName,'integer',MinValue}            % Always required and must be >= a MinValue
 % 
 {OpHandlerName,ExpBodyParams} =
 case Method of
 
  % PUT -> add_sublocation({Loc_id,Subloc_id},Name)
  <<"PUT">> ->
   {add_sublocation_handler, % Operation handler name
    [
     {name,list,optional}    % "Name" parameter (optional)
	]
   };

  % DELETE -> delete_sublocation({Loc_id,Subloc_id})
  <<"DELETE">> ->
   {delete_sublocation_handler, % Operation handler name
    []                          % No body parameters required
   }
 end,
 
 % Return the name of the operation handler, the list of path bindings parameters and
 % the list of expected parameters to be retrieved from the body of the HTTP request
 {OpHandlerName,[Subloc_id],ExpBodyParams}.
 
%% ===================================================== OPERATION HANDLERS ===================================================== %% 
 
%% ADD_SUBLOCATION (PUT /sublocation/:subloc_id)
%% ===============
add_sublocation_handler(Req,[Subloc_id,Name]) ->
 
 % Retrieve the controller's location ID
 {ok,Loc_id} = application:get_env(janet_controller,loc_id),
 
 % Define the operation to be executed in the Simulator and Controller databases
 DBFun = add_sublocation,
 
 % Define the list of parameters of the operation in the Simulator database
 SimArgsList = [{Loc_id,Subloc_id},Name],

 % Define the list of parameters of the operation in the Controller database
 CtrArgsList = [Subloc_id],
  
 % Attempt to execute the operation in the Simulator and Controller databases
 case sim_db_sync(DBFun,SimArgsList,CtrArgsList) of
  {ok,ok} ->
   
   % If the sublocation was added in both databases, report the success of the operation
   io:format("[~p-~w]: Added sublocation (subloc_id = ~w, name = ~p)~n",[?FUNCTION_NAME,Loc_id,Subloc_id,Name]),
   
   % Define the success HTTP response to be replied to the client
   cowboy_req:reply(201,Req);
  
  %% ------------------------------------ Operation Errors ------------------------------------ %   
  
  % Trying to add a location that already exists
  {error,sublocation_already_exists} ->
   throw({sublocation_already_exists,{Loc_id,Subloc_id}})
 end.
 

%% DELETE_SUBLOCATION (DELETE /sublocation/:subloc_id)
%% ==================
delete_sublocation_handler(Req,[Subloc_id]) ->
 
 % Retrieve the controller's location ID
 {ok,Loc_id} = application:get_env(janet_controller,loc_id),
 
 % Define the operation to be executed in the Simulator and Controller databases
 DBFun = delete_sublocation,
 
 % Define the list of parameters of the operation in the Simulator database
 SimArgsList = [{Loc_id,Subloc_id}],

 % Define the list of parameters of the operation in the Controller database
 CtrArgsList = [Subloc_id],
  
 % Attempt to execute the operation in the Simulator and Controller databases
 case sim_db_sync(DBFun,SimArgsList,CtrArgsList) of
  {ok,ok} ->
   
   % If the sublocation was deleted from both databases, report the success of the operation
   io:format("[~p-~w]: Deleted sublocation (subloc_id = ~w)~n",[?FUNCTION_NAME,Loc_id,Subloc_id]),
   
   % Define the success HTTP response to be replied to the client
   cowboy_req:reply(204,Req);
  
  %% ------------------------------------ Operation Errors ------------------------------------ %   
  
  % Trying to delete a sublocation that does not exist
  {error,sublocation_not_exists} ->
   throw({sublocation_not_exists,{Loc_id,Subloc_id}})
 end.
 

%%=================================================================================================================================%%
%%                                                    RESOURCE: /device/:dev_id                                                    %% 
%%=================================================================================================================================%%

%% ALLOWED METHODS:
%% ---------------
%%   - PUT    -> add_device(Dev_id,Name,{Loc_id,Subloc_id},Type)
%%   - POST   -> update_dev_subloc(Dev_id,{Loc_id,Subloc_id})
%%   - DELETE -> delete_device(Dev_id)
%%
res_dev_handler(Req) ->
 
 % Define the binary list of HTTP methods allowed by this resource handler
 Allowed_Methods = [<<"PUT">>,<<"POST">>,<<"DELETE">>],
 
 % Ensure the HTTP request method to be included in the list of allowed methods
 Method = gen_resthandler:get_check_method(Req,Allowed_Methods),
 
 % Retrieve the "Subloc_id" path binding parameter
 Dev_id = gen_resthandler:get_check_int_binding(Req,dev_id,1),

 % Determine the name and the expected body parameters of the operation handler associated with
 % the request from its HTTP method, with the latters being defined using the following syntax:
 %
 % - List/String parameters: {ParamName,'list','required'/'optional'}  % Required or optional
 % - Integer parameters:     {ParamName,'integer',MinValue}            % Always required and must be >= a MinValue
 % 
 {OpHandlerName,ExpBodyParams} =
 case Method of
 
  % PUT -> add_device(Dev_id,Name,{Loc_id,Subloc_id},Type)
  <<"PUT">> ->
   {add_device_handler,        % Operation handler name
    [
	 {name,list,optional},     % "Name" parameter (optional)
	 {subloc_id,integer,0},    % "Subloc_id" parameter (>= 0)
	 {type,list,required}      % "Type" parameter (required)
	]
   };

  % POST -> update_dev_subloc(Dev_id,{Loc_id,Subloc_id})
  <<"POST">> ->
   {update_dev_subloc_handler, % Operation handler name
    [
	 {subloc_id,integer,0}     % "Subloc_id" parameter (>= 0)
	]
   };
   
  % DELETE -> delete_device(Dev_id)
  <<"DELETE">> ->
   {delete_device_handler,     % Operation handler name
    []                         % No body parameters required
   }
 end,
 
 % Return the name of the operation handler, the list of path bindings parameters and
 % the list of expected parameters to be retrieved from the body of the HTTP request
 {OpHandlerName,[Dev_id],ExpBodyParams}. 
 
 
%% ADD_DEVICE (PUT /device/:dev_id)
%% ==========
add_device_handler(Req,[Dev_id,Name,Subloc_id,CapitalType]) ->
 
 % Cast the device CapsType to lowercase
 % (server-side compatibility) and then to an atom
 Type = list_to_atom(string:casefold(CapitalType)),
 
 % Retrieve the controller's location ID
 {ok,Loc_id} = application:get_env(janet_controller,loc_id),
 
 % Define the operation to be executed in the Simulator and Controller databases
 DBFun = add_device,
 
 % Define the list of parameters of the operation in the Simulator database
 SimArgsList = [Dev_id,Name,{Loc_id,Subloc_id},Type],

 % Define the list of parameters of the operation in the Controller database
 CtrArgsList = [Dev_id,Subloc_id,Type],
  
 % Attempt to execute the operation in the Simulator and Controller databases
 case sim_db_sync(DBFun,SimArgsList,CtrArgsList) of
  {ok,ok,ok} ->
   
   % If the device was added in both databases and its node was started, report the success of the operation
   io:format("[~p-~w]: Added device (dev_id = ~w, name = ~p, sub_id = {~w,~w}, type = ~p)~n",[?FUNCTION_NAME,Loc_id,Dev_id,Name,Loc_id,Subloc_id,Type]),
 
   % Retrieve the device initial configuration in JSON format
   DefaultConfigJSON = utils:get_devtype_default_config_json(Type),
   
   % Define the HTTP response to be replied to the client
   cowboy_req:reply(
	                200,                                                  % HTTP Response Code
	                #{<<"content-type">> => <<"application/json">>},      % "Content Type" header
		            DefaultConfigJSON,                                    % Response Body
			        Req                                                   % Associated HTTP Request
			       );
  
  %% ------------------------------------ Operation Errors ------------------------------------ %   

  % Trying to add a device of invalid type
  {error,invalid_devtype} ->
   throw({invalid_devtype,Type});

  % Trying to add a device that already exists
  {error,device_already_exists} ->
   throw({device_already_exists,Dev_id});
   
  % Trying to add a device in a sublocation that does not exist
  {error,sublocation_not_exists} ->
   throw({sublocation_not_exists,{Loc_id,Subloc_id}});
   
  % The device was added into the database, but an internal error occured in starting its node
  {ok,InternalError,ok} ->
   throw({device_not_started,InternalError})
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
%%   - POST   -> update_loc_name(Loc_id,Name)
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


%%====================================================================================================================================
%%                                                SIMULATOR DATABASE BRIDGE FUNCTIONS
%%==================================================================================================================================== 

%% Attempts to perform an operation in the JANET Simulator database and,
%% if successful, attempts to mirror it in the controller's database
sim_db_sync(DBFun,SimArgsList,CtrArgsList) ->

 % Attempt to perform the "DBFun" function with "SimArgsList" arguments in the JANET Simulator database
 % by routing the request via the 'ctr_simserver' to the 'ctr_manager' associated with the controller
 case gen_sim_command(db,DBFun,SimArgsList) of
 
  {error,Error} ->
  
   % If an error was raised in performing the operation, whether it occured in the JANET Simulator database or
   % within the communication, return it without attempting to mirror the operation on the controller's database
   {error,Error};
   
  SimDBRes ->
   
   % If the JANET Simulator database operation was successful, regardless of the results of its
   % side-effects, if any (which are carried in the "SimDBRes" variable), attempt to mirror such
   % operation in the location controller, concatenating in order the results of the two operations
   print_sim_db_sync_result(SimDBRes,apply(ctr_db,DBFun,CtrArgsList))
 end.
   
   
%% Concatenates the result of a database operation in the JANET Simulator with the result of its mirroring
%% in the controller's database into a tuple (sim_db_sync(DBFun,SimArgsList,CtrArgsList) helper function) 
print_sim_db_sync_result(SimDBRes,CtrDBRes) when is_atom(SimDBRes), is_atom(CtrDBRes) ->
 {SimDBRes,CtrDBRes};
print_sim_db_sync_result(SimDBRes,CtrDBRes) when is_atom(SimDBRes), is_tuple(CtrDBRes) ->
 list_to_tuple([SimDBRes] ++ [CtrDBRes]);
print_sim_db_sync_result(SimDBRes,CtrDBRes) when is_tuple(SimDBRes), is_atom(CtrDBRes) ->
 list_to_tuple(tuple_to_list(SimDBRes) ++ [CtrDBRes]);
print_sim_db_sync_result(SimDBRes,CtrDBRes) when is_tuple(SimDBRes), is_tuple(CtrDBRes) ->
 list_to_tuple(tuple_to_list(SimDBRes) ++ [CtrDBRes]).

 
%% Attempts to synchronously execute a command on the JANET Simulator by routing the request
%% via the 'ctr_simserver' to the 'ctr_manager' associated with this controller, returning
%% the result of the operation (sim_db_sync(DBFun,SimArgsList,CtrArgsList) helper function) 
gen_sim_command(Module,Function,ArgsList) ->
 
 % Route the command via the 'ctr_simserver' to the 'ctr_manager' associated
 % with this controller and wait for a response up to a predefined timeout
 try gen_server:call(ctr_simserver,{sim_command,Module,Function,ArgsList},5000)
 catch
    exit:{timeout,_} ->
	   
     % Command timeout
	 {error,request_timeout}
 end.
 
 
%%====================================================================================================================================
%%                                                         START FUNCTION                                                        
%%====================================================================================================================================

%% Called by the Janet Simulator top-level supervisor (sup_jsim) at boot time
start_link() ->
 gen_resthandler:start_link(?MODULE,[]).