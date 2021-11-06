%% This module represents the REST handler in the JANET Controller application %%

-module(ctr_resthandler).
-behaviour(gen_resthandler).                          % Custom REST handler behaviour

-include("ctr_mnesia_tables_definitions.hrl").        % Janet Controller Mnesia Tables Records Definitions
-include("devtypes_configurations_definitions.hrl").  % Janet Device Configuration Records Definitions

%% -------------------------- gen_resthandler BEHAVIOUR CALLBACK FUNCTIONS -------------------------- %%
-export([start_link/0,init_handler/1,init/2,err_to_code_msg/1,os_port_conflict/1]).

%% -------------------------------- RESOURCES AND OPERATIONS HANDLERS -------------------------------- %%
-export([res_subloc_handler/1,              % /sublocation/:subloc_id resource handler
         add_sublocation_handler/2,         %                         operation handlers
		 delete_sublocation_handler/2]).    %                         
-export([res_dev_handler/1,                 % /device/:dev_id resource handler
         add_device_handler/2,              %                 operation handlers
		 update_dev_subloc_handler/2,       %
		 delete_device_handler/2]).         %   
-export([res_devcommands_handler/1,         % /devcommands resource handler
         devcommands_handler/2]).           %              operation handler

%% ---------------------------- DEVICE COMMANDS UTILITIES (PATCH /device) ---------------------------- %%

%% This records represents the information associated with
%% a valid comand to be issued to a device (PATCH /device)
-record(valdevcmd,
        {
		 dev_id,         % The 'dev_id' of the device node the command must be issued to
		 cfgcommand,     % The configuration command to be issued
		 devhandler_pid, % The PID of the device's 'dev_handler' where to issue the command
		 devhandler_ref  % A reference used for issuing the command to the 'dev_handler'
		}).

%% Maximum time for collecting the device commands' responses
-define(Devresponses_timeout,4700).    % Default: 4700 (4.7 seconds)


%%====================================================================================================================================
%%                                                GEN_RESTHANDLER CALLBACK FUNCTIONS                                                        
%%====================================================================================================================================

%% ======================================================== INIT_HANDLER ======================================================== %%
init_handler(_) ->
 
 % Retrieve the 'ctr_rest_port' environment variable
 {ok,CtrRESTPort} = application:get_env(ctr_rest_port),
 
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
          %   - PUT    -> add_device(Dev_id,Name,{Loc_id,Subloc_id},Type,HostName)
		  %   - POST   -> update_dev_subloc(Dev_id,{Loc_id,Subloc_id})
          %   - DELETE -> delete_device(Dev_id)
		  %		
		  {"/device/:dev_id",?MODULE,res_dev_handler},
			 
          % RESOURCE: /devcommands
		  % ALLOWED METHODS: 
          %   - PATCH -> devcommands([{Dev_id,[Commands]}])
		  %		
		  {"/devcommands",?MODULE,res_devcommands_handler}
	     ],
			
 % Return the initialization tuple to the behaviour engine
 {ok,CtrRESTPort,ListenerName,Paths}.
 
 
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
 
%% DELETE_SUBLOCATION + ADD_DEVICE + UPDATE_DEV_SUBLOC (DELETE /sublocation/:subloc_id + PUT /device/:dev_id + POST /device/:dev_id) 
%% ---------------------------------------------------
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

% Trying to deploy the device on an unallowed nodes host
err_to_code_msg({invalid_hostname,HostName}) ->
 {406,io_lib:format("<ERROR> The specified \"hostname\" (~s) does not belong to the list of allowed hosts where JANET nodes can be deployed in",[HostName])};

% The device was added into the database, but an internal error occured in starting its node
err_to_code_msg({device_not_started,Dev_id,InternalError}) ->
 {500,io_lib:format("<SERVER ERROR> The device of \"dev_id\" ~w was added, but an internal error occured in starting its node: ~p",[Dev_id,InternalError])};

%% UPDATE_DEV_SUBLOC + DELETE_DEVICE (POST /device/:dev_id + DELETE /device/:dev_id)
%% ---------------------------------
% Trying to operate on a device that does not exist
err_to_code_msg({device_not_exists,Dev_id}) ->
 {404,io_lib:format("<ERROR> A device with such \"dev_id\" (~w) does not exist",[Dev_id])};

%% DELETE_DEVICE (DELETE /device/:dev_id)
%% -------------
% The device was deleted, but an internal error occured in stopping its node
err_to_code_msg({device_node_not_stopped,Dev_id,InternalError}) ->
 {500,io_lib:format("<SERVER ERROR> The device of \"dev_id\" ~w was deleted, but an internal error occured in stopping its node (~p)",[Dev_id,InternalError])};   

%% DEVCOMMANDS (PATCH /device)
%% -----------
% HTTP request body could not be interpreted in JSON format
err_to_code_msg(body_not_json) ->
 {415,"<ERROR> Request body could not be interpreted in JSON format"}; 

% The binary body of the HTTP request was not cast to a JSON list by the JSONE library
err_to_code_msg({not_a_list,devcommands}) ->
 {400,"<ERROR> The request body could not be interpreted as a JSON list"};

% The list of device commands is empty
err_to_code_msg({empty,devcommands}) ->
 {400,"<ERROR> The list of device commands is empty"};

% The device commands' results could not be encoded in JSON format
err_to_code_msg(jsone_encode_error) ->
 {500,"<SERVER ERROR> The device commands' results could not be encoded in JSON"};

% NOTE: The per-devcommand error handling is performed manually within the operation handler
% ----

%% UNKNOWN ERROR
%% ------------- 
err_to_code_msg(UnknownError) ->
 {500,io_lib:format("<UNKNOWN SERVER ERROR> Unknown error: ~p",[UnknownError])}.
 

%% ====================================================== OS_PORT_CONFLICT ====================================================== %% 
os_port_conflict(RESTPort) ->

 % Retrieve the controller's location ID
 {ok,Loc_id} = application:get_env(janet_controller,loc_id),

 % Report that the REST server of the JANET Controller application will not be reachable by the remote client during the execution
 io:format("[ctr_resthandler-~w]: <PORT CONFLICT> Port \"~w\" is not available in the host OS, the JANET Controller REST server will NOT be reachable by the remote client~n",[Loc_id,RESTPort]),
 
 % Return the atom 'ignore' so to prevent the 'sup_jctr' root supervisor from reattempting
 % to restart the process (which would be useless being the REST port unavailable) 
 ignore.
 
 
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
%%   - PUT    -> add_device(Dev_id,Name,{Loc_id,Subloc_id},Type,HostName)
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
 
  % PUT -> add_device(Dev_id,Name,{Loc_id,Subloc_id},Type,HostName)
  <<"PUT">> ->
   {add_device_handler,        % Operation handler name
    [
	 {name,list,optional},     % "Name" parameter      (optional)
	 {subloc_id,integer,0},    % "Subloc_id" parameter (>= 0)
	 {type,list,required},     % "Type" parameter      (required)
	 {hostname,list,required}  % "HostName" parameter  (required)
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
add_device_handler(Req,[Dev_id,Name,Subloc_id,CapitalType,HostName]) ->
 
 % Cast the device CapsType to lowercase
 % (server-side compatibility) and then to an atom
 Type = list_to_atom(string:casefold(CapitalType)),
 
 % Retrieve the controller's location ID
 {ok,Loc_id} = application:get_env(janet_controller,loc_id),
 
 % Define the operation to be executed in the Simulator and Controller databases
 DBFun = add_device,
 
 % Define the list of parameters of the operation in the Simulator database
 SimArgsList = [Dev_id,Name,{Loc_id,Subloc_id},Type,HostName],

 % Define the list of parameters of the operation in the Controller database
 CtrArgsList = [Dev_id,Subloc_id,Type],
  
 % Attempt to execute the operation in the Simulator and Controller databases
 case sim_db_sync(DBFun,SimArgsList,CtrArgsList) of
  {ok,ok,ok} ->
   
   % If the device was added in both databases and its node was started, report the success of the operation
   io:format("[~p-~w]: Added device (dev_id = ~w, name = ~p, sub_id = {~w,~w}, type = ~p, hostname = ~s)~n",[?FUNCTION_NAME,Loc_id,Dev_id,Name,Loc_id,Subloc_id,Type,HostName]),
 
   % Build the response body in JSON format by concatenating
   % the "Dev_id" with the device initial configuration
   RespBody = list_to_binary(lists:flatten(io_lib:format("{\"dev_id\":~w,\"state\":" ++ utils:get_devtype_default_config_json(Type) ++ "}",[Dev_id]))),

   % Define the HTTP response to be replied to the client
   cowboy_req:reply(
	                200,                                              % HTTP Response Code
	                #{<<"content-type">> => <<"application/json">>},  % "Content Type" header
		            RespBody,                                         % Response Body
			        Req                                               % Associated HTTP Request
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
   
  % Trying to deploy the device on an unallowed nodes host
  {error,invalid_hostname} ->
   throw({invalid_hostname,HostName});   
   
  % The device was added into the database, but an internal error occured in starting its node
  {ok,InternalError,ok} ->
   throw({device_not_started,Dev_id,InternalError})
 end. 
 

%% UPDATE_DEV_SUBLOC (POST /device/:dev_id)
%% =================
update_dev_subloc_handler(Req,[Dev_id,Subloc_id]) ->
 
 % Retrieve the controller's location ID
 {ok,Loc_id} = application:get_env(janet_controller,loc_id),
 
 % Define the operation to be executed in the Simulator and Controller databases
 DBFun = update_dev_subloc,
 
 % Define the list of parameters of the operation in the Simulator database
 SimArgsList = [Dev_id,{Loc_id,Subloc_id}],

 % Define the list of parameters of the operation in the Controller database
 CtrArgsList = [Dev_id,Subloc_id],
  
 % Attempt to execute the operation in the Simulator and Controller databases
 case sim_db_sync(DBFun,SimArgsList,CtrArgsList) of
  {ok,ok} ->
   
   % If the device sublocation was updated in both databases, report the success of the operation
   io:format("[~p-~w]: Updated device sublocation (dev_id = ~w, sub_id = {~w,~w})~n",[?FUNCTION_NAME,Loc_id,Dev_id,Loc_id,Subloc_id]),
 
   % Define the success HTTP response to be replied to the client
   cowboy_req:reply(204,Req);
  
  %% ------------------------------------ Operation Errors ------------------------------------ %   

  % Trying to update the sublocation of a device that does not exist
  {error,device_not_exists} ->
   throw({device_not_exists,Dev_id});

  % Trying to move the device in a sublocation that does not exist
  {error,sublocation_not_exists} ->
   throw({sublocation_not_exists,{Loc_id,Subloc_id}});
   
  % Trying to update the sublocation of a device in a different location
  % (throw that the device does not exist in the current location)
  {error,different_locations} ->
   throw({device_not_exists,Dev_id})
 end. 


%% DELETE_DEVICE (DELETE /device/:dev_id)
%% =============
delete_device_handler(Req,[Dev_id]) ->
 
 % Ensure the device to belong to the current location
 % (for preventing the deletion of devices in other locations)
 case ctr_db:get_record(ctr_device,Dev_id) of
  
  % If the device was found in
  % the current location, continue
  {ok,_DevRecord} ->
   ok;
   
  % If the device was NOT found in the
  % current location, throw an error
  {error,not_found} ->
   throw({device_not_exists,Dev_id})
 end,
 
 % Retrieve the controller's location ID
 {ok,Loc_id} = application:get_env(janet_controller,loc_id),
 
 % Define the operation to be executed in the Simulator and Controller databases
 DBFun = delete_device,
 
 % Define the list of parameters of the operation in the Simulator database
 SimArgsList = [Dev_id],

 % Define the list of parameters of the operation in the Controller database
 CtrArgsList = [Dev_id],
  
 % Attempt to execute the operation in the Simulator and Controller databases
 case sim_db_sync(DBFun,SimArgsList,CtrArgsList) of
  {ok,ok,ok} ->
   
   % If the device was deleted in both database and its node was stopped, report the success of the operation
   io:format("[~p-~w]: Deleted device (dev_id = ~w)~n",[?FUNCTION_NAME,Loc_id,Dev_id]),
 
   % Define the success HTTP response to be replied to the client
   cowboy_req:reply(204,Req);
  
  %% ------------------------------------ Operation Errors ------------------------------------ %   

  % Trying to delete a device that does not exist
  {error,device_not_exists} ->
   throw({device_not_exists,Dev_id});

  % The device was deleted, but an internal error occured in stopping its node
  {ok,InternalError,ok} ->
   throw({device_node_not_stopped,Dev_id,InternalError})
 end. 


%%=================================================================================================================================%%
%%                                                      RESOURCE: /devcommands                                                     %% 
%%=================================================================================================================================%%

%% ALLOWED METHODS:
%% ---------------
%%   - PATCH -> devcommands([{Dev_id,[Commands]}])
%%
res_devcommands_handler(Req) ->
 
 % Define the binary list of HTTP methods allowed by this resource handler
 Allowed_Methods = [<<"PATCH">>],
 
 % Ensure the HTTP request method to be included in the list of allowed methods
 Method = gen_resthandler:get_check_method(Req,Allowed_Methods),

 % Determine the name and the expected body parameters of the
 % operation handler associated with the request from its HTTP method
 {OpHandlerName,ExpBodyParams} =
 case Method of
 
  % PATCH -> devcommands([{Dev_id,[Commands]}])
  <<"PATCH">> ->
   {
    devcommands_handler,  % Operation handler name
    custom                % Inform the gen_resthandler to return the binary body of the HTTP request        
   }
 end,
 
 % Return the name of the operation handler, the list of path bindings parameters and
 % the list of expected parameters to be retrieved from the body of the HTTP request
 {OpHandlerName,[],ExpBodyParams}.
 

%% DEVCOMMANDS (PATCH /devcommands)
%% ===========
devcommands_handler(Req,[BinBody]) ->
 
 % Retrieve the list of device commands from the binary body
 DevCommands = get_devcommands(BinBody),
 
 % Parse the list of device commands, obtaining
 % the lists of valid and invalid commands
 {ValidCommands,InvalidCommands} = parse_devcommands(DevCommands,[],[],[]),
 
 % Issue the valid commands to their respective devices,
 % obtaining the lists of successful and failed device commands
 {SuccessfulCommands,FailedCommands} = issue_devcommands(ValidCommands),
 
 % Retrieve the status code and body of the HTTP response to be returned to the client
 {RespCode,RespBody} = build_devcommands_response(SuccessfulCommands,FailedCommands,InvalidCommands),
 
 % Print a summary of the operation
 print_devcommands_summary(SuccessfulCommands,FailedCommands,ValidCommands,InvalidCommands,RespCode),
 
 % Return the HTTP response to the client
 cowboy_req:reply(
                  RespCode,                                         % HTTP Response Code
	              #{<<"content-type">> => <<"application/json">>},  % "Content Type" header
		          RespBody,                                         % Response Body
			      Req                                               % Associated HTTP Request
			     ).


%%====================================================================================================================================%
%%                                                                                                                                    %
%%                                                     PRIVATE HELPER FUNCTIONS                                                       %
%%                                                                                                                                    %
%%====================================================================================================================================%

%%====================================================================================================================================
%%                                             SIMULATOR DATABASE BRIDGE HELPER FUNCTIONS
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
   
   % If the JANET Simulator database operation was successful, regardless of the results of its side-effects, if
   % any (which are carried in the "SimDBRes" variable), attempt to mirror such operation in the controller database
   case apply(ctr_db,DBFun,CtrArgsList) of
   
    % If an error occured in mirroring the operation in the controller database, the data inconsistency with
	% the JANET Simulator database must be fixed by restarting the controller node, which is performed by:
    %
	% 1) CONCEALING the error from the REST client, returning him that the operation was successful
	% 2) Spawning a temporary process which, after a delay allowing the 'ctr_resthandler' to reply
	%    the "success" of the operation to the client, stops the JANET Controller application
    %
    {error,CtrDBError} ->
	
	 % Retrieve the controller's location ID
     {ok,Loc_id} = application:get_env(janet_controller,loc_id),

	 % Log that the controller node will soon be restarted to
	 % fix the inconsistency with the JANET Simulator database
     io:format("[ctr_resthandler~w]: <CONSISTENCY ERROR> The controller node will be restarted due to an inconsistency with the JANET Simulator database (DBFun = ~0p, SimArgsList = ~0p, SimDBRes = ~0p, CtrDBError = ~0p)~n",[Loc_id,DBFun,SimArgsList,SimDBRes,CtrDBError]),

	 % Spawn a temporary process which, after a delay allowing to reply the client
	 % that the operation was successful, stops the JANET controller application
	 spawn_link(fun() -> timer:sleep(250), application:stop(janet_controller) end),
	 
	 % Concatenate the result of the database operation in the JANET Simulator with a fake
	 % 'ok' concealing the error occured in mirroring the operation in the JANET Controller
     print_sim_db_sync_result(SimDBRes,ok);
	 
	% Otherwise if the operation in the controller database was successful ('ok'),
	% concatenate it with the result of the successful operation in the JANET Simulator
	CtrDBSuccess ->
	 print_sim_db_sync_result(SimDBRes,CtrDBSuccess)
  end
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
%%                                              DEVICE COMMANDS ISSUING HELPER FUNCTIONS
%%====================================================================================================================================  

%% ===================================================== COMMANDS PARSING ========================================================== %% 

%% DESCRIPTION:  Retrieves the list of device commands (DevCommands) from the binary body of an HTTP request
%%
%% ARGUMENTS:    - BinBody: The binary body of the HTTP request from which retrieve the list of device commands
%%
%% RETURNS:      - DevCommands -> The list of device commands
%%
%% THROWS:       - body_not_json            -> The binary body could not be interpreted in JSON format
%%               - {not_a_list,devcommands} -> The binary body of the HTTP request was not
%%                                             cast to a JSON list by the JSONE library
%%               - {empty,devcommands}      -> The list of device commands is empty
%%
get_devcommands(BinBody) ->
    
 % Interpret the binary body of the HTTP
 % request as JSON via the JSONE library	
 DevCommands = try jsone:decode(BinBody)
               catch
		   
		        % If the binary body could not be
				% interpred as JSON, throw an error
		        error:badarg ->
			     throw(body_not_json)
		       end,
 
 % Ensure that the binary body was
 % cast to a list by the JSONE library 
 if
  is_list(DevCommands) =:= false ->
   
   % If it is not, throw an error
   throw({not_a_list,devcommands});
   
  true ->
  
   % If it is, ensure it not to be empty
   if
   
    % If it is not, return the
	% list of device commands
    length(DevCommands) > 0 ->
	 DevCommands;
	
    % If it is, throw an error	
	true ->
	 throw({empty,devcommands})
   end
 end.
 
 
%% DESCRIPTION:  Parses a list of device commands, returning the map lists of valid and invalid commands
%%
%% ARGUMENTS:    - DevCommands:     The list of device commands to be parsed
%%               - ValidCommands:   The list of valid device commands (accumulator)
%%               - InvalidCommands: The list of invalid device commands (accumulator)
%%               - ValDevIds:       The list of "Dev_id"s associated with already parsed valid commands
%%                                  (used for checking duplicate commands towards the same "Dev_id")
%%
%% RETURNS:      - {ValidCommands,InvalidCommands} -> The list of valid and invalid device commands
%%
% Base case (return the lists of valid and invalid device commands)
parse_devcommands([],ValidCommands,InvalidCommands,_ValDevIds) ->
 {ValidCommands,InvalidCommands};
 
% Recursive case when a device command was correctly cast to a map by the JSONE library
parse_devcommands([DevCommand|NextDevCommand],ValidCommands,InvalidCommands,ValDevIds) when is_map(DevCommand) ->

 % Parse the device command map
 ParsedCommand =
 try
  
  % Attempt to retrieve the "dev_id" key from the device command
  Dev_id = get_devcommand_dev_id(DevCommand),
  
  try
   
   % Ensure that a valid command towards such "Dev_id" was not already parsed
   ok = check_devcommand_duplicate(Dev_id,ValDevIds), 
   
   % Attempt to retrieve the "actions" map from the device command
   Actions = get_devcommand_actions(DevCommand), 
   
   % Ensure device "Dev_id" to belong to the location and to be currently paired
   % with the controller, and obtain its Type and the PID of its 'ctr_devhandler'
   {Type,HandlerPID} = get_devcommand_devinfo(Dev_id), 
   
   % Parse the "actions" map according to the device type so to obtain the configuration
   % command record of the appropriate #devtype to be issued to the device 
   CfgCommand = build_cfgcommand(Actions,Type),
   
   % Initialize a reference that will be used for issuing the
   % command to the dev_handler associated with the device
   HandlerRef = make_ref(),
   
   % Return the #valdevcmd record associated with the successfully parsed DevCommand
   {ok,#valdevcmd{dev_id = Dev_id, cfgcommand = CfgCommand, devhandler_pid = HandlerPID, devhandler_ref = HandlerRef}}
   
  catch
  
   %% ----------------------------------- Command Parsing Inner Errors (with "Dev_id") ----------------------------------- %
   
   % A valid command towards such "Dev_id" was already parsed
   {duplicate,devid} ->	
    throw(#{dev_id => Dev_id, status => 400, errorReason => <<"A valid command towards such device was already issued">>});
	
   % The "actions" map is missing
   {missing_param,actions} ->
    throw(#{dev_id => Dev_id, status => 400, errorReason => <<"The 'actions' object is missing">>});
	
   % The "actions" parameter was not interpreted as a map
   {not_a_map,actions} ->
    throw(#{dev_id => Dev_id, status => 400, errorReason => <<"The 'actions' parameter could not be interpreted as a JSON object">>});
   
   % The "actions" map is empty
   {empty,actions} ->
    throw(#{dev_id => Dev_id, status => 400, errorReason => <<"The 'actions' object is empty">>});
	
   % Device "Dev_id" does not belong to the controller's location
   device_not_exists ->
    throw(#{dev_id => Dev_id, status => 404, errorReason => <<"A device with such 'dev_id' does not exist">>});
   
   % The device is currently not paired with the controller
   device_offline ->
    throw(#{dev_id => Dev_id, status => 307, errorReason => <<"The device is currently offline">>});
   
   % Invalid action valiue
   {invalid_action,Action,InvalidValue} ->
    throw(#{dev_id => Dev_id, status => 400, errorReason => list_to_binary("Invalid value '" ++ InvalidValue ++ "' of action '" ++ Action ++ "'")});   

   % Invalid actions (keys) in the "actions" map for the associated device type
   {invalid_actions,DevType} ->
    throw(#{dev_id => Dev_id, status => 400, errorReason => list_to_binary("The 'actions' object contains invalid actions for the associated device type (" ++ atom_to_list(DevType) ++ ")")});
	
   % Invalid state (door)
   {invalid_state,door} ->
    throw(#{dev_id => Dev_id, status => 400, errorReason => <<"The state {'open','lock'} is invalid for device type 'door'">>});
	
   % An invalid trait value was cast to an unknown Erlang term() by the JSONE library
   {unknown_jsone_cast,InvalidTrait} ->
    throw(#{dev_id => Dev_id, status => 500, errorReason => list_to_binary("Invalid value of trait '"++ atom_to_list(InvalidTrait) ++ "' was cast to an unknown term() by the JSONE library")});
   
   % The device type in the controller Mnesia database is invalid
   {invalid_db_devtype,InvalidDevType} ->
    throw(#{dev_id => Dev_id, status => 500, errorReason => list_to_binary("The device type in the controller database (" ++ atom_to_list(InvalidDevType) ++ ") is invalid")});
   
   % Unhandled Error
   _:UnhandledErrorInner ->
   
    % Retrieve the controller's location ID for logging purposes
    {ok,Loc_id_in} = application:get_env(janet_controller,loc_id),
	
	% Log and throw the unhandled error
    io:format("[parse_devcommands (inner)-~w]: Unhandled error: ~p (dev_id: ~p)~n",[Loc_id_in,UnhandledErrorInner,Dev_id]),
    throw(#{dev_id => Dev_id, status => 500, errorReason => list_to_binary(lists:flatten(io_lib:format("Unhandled server error while parsing the command: ~p",[UnhandledErrorInner])))})
  end
  
 catch
 
  %% ------------------ Command Parsing Outer Errors (without "Dev_id", or already parsed inner error) ------------------ %
  
  % "Dev_id" missing
  {missing_param,dev_id} ->
   {error,#{dev_id => unknown, status => 400, errorReason => <<"Required parameter 'dev_id' is missing">>}};
  
  % "Dev_id" not an integer
  {not_an_integer,InvalidDevId} ->
   {error,#{dev_id => invalid, status => 400, errorReason => list_to_binary(lists:flatten(io_lib:format("Parameter 'dev_id' is not an integer (~s)",[catch(gen_resthandler:jsone_term_to_list(InvalidDevId,dev_id))])))}};
  
  % "Dev_id" <= 0
  {out_of_range,dev_id,InvalidDevId} ->
   {error,#{dev_id => invalid, status => 400, errorReason => list_to_binary("Invalid value of parameter 'dev_id' (" ++ integer_to_list(InvalidDevId) ++ ")")}};
  
  % Already parsed inner error
  ErrorMap when is_map(ErrorMap) ->
   {error,ErrorMap};
   
  % Unhandled Error
  _:UnhandledErrorOuter ->
  
   % Retrieve the controller's location ID for logging purposes
   {ok,Loc_id_out} = application:get_env(janet_controller,loc_id),
	
   % Log and throw the unhandled error
   io:format("[parse_devcommands (outer)-~w]: Unhandled Error: ~p~n",[Loc_id_out,UnhandledErrorOuter]),
   {error,throw(#{dev_id => unknown, status => 500, errorReason => list_to_binary(lists:flatten(io_lib:format("Unhandled server error while parsing the command: ~p",[UnhandledErrorOuter])))})}
 end,
 
 % Depending on whether the command was successfully parsed
 case ParsedCommand of
 
  % If it was, append it and the "Dev_id" of its associated device in the
  % "ValidCommands" and "ValDevIds" lists and parse the next device command
  {ok,ValidCommand} ->
   parse_devcommands(NextDevCommand,ValidCommands ++ [ValidCommand],InvalidCommands,ValDevIds ++ [ValidCommand#valdevcmd.dev_id]);
  
  % Otherwise append it into the list of invalid commands and parse the next one
  {error,InvalidCommand} ->
   parse_devcommands(NextDevCommand,ValidCommands,InvalidCommands ++ [InvalidCommand],ValDevIds)
 end;
 
% Recursive case when a device command was NOT cast to a map by the JSONE library
parse_devcommands([_|NextDevCommand],ValidCommands,InvalidCommands,ValDevIds) ->

 % Appent the command in the list of invalid commands and parse the next one
 parse_devcommands(NextDevCommand,ValidCommands,InvalidCommands ++ [#{dev_id => unknown, status => 400, errorReason => <<"The device command could not be interpreted as a JSON object">>}],ValDevIds).


%% DESCRIPTION:  Returns the 'dev_id' associated with a device command
%%
%% ARGUMENTS:    - DevCommand: The device command map
%%
%% RETURNS:      - Dev_id -> The 'dev_id' associated with the command
%%
%% THROWS:       - {missing_param,dev_id}  -> The "dev_id" key was not found in the device command map
%%               - {not_an_integer,dev_id} -> The "dev_id" is not an integer
%%               - {out_of_range,dev_id}   -> The "dev_id" is <= 0
%%
get_devcommand_dev_id(DevCommand) -> 

 % Attempt to retrieve the value of key "dev_id" from the device command map
 Dev_id = try maps:get(<<"dev_id">>,DevCommand)
          catch
		
		   % If the "dev_id" key was
		   % not found, throw an error
 	       error:{badkey,<<"dev_id">>} ->
		    throw({missing_param,dev_id})
		  end,

 % Ensure the "Dev_id" to be an integer
 if
  
  % If it is, ensure it to be > 0
  is_integer(Dev_id) ->
   if
    
	% If it is, return it
	Dev_id > 0 ->
     Dev_id;
	 
	% If it is not, throw an error
	true ->
	 throw({out_of_range,dev_id,Dev_id})
   end;
   
  % If it is not an integer, throw an error
  true ->
   throw({not_an_integer,Dev_id})
 end.
   

%% DESCRIPTION:  Ensures that a valid command towards a given "Dev_id" was not already parsed
%%
%% ARGUMENTS:    - Dev_id:    The "Dev_id" to be checked for duplicate commands
%%               - ValDevIds: The list of "Dev_id"s associated with valid commands already parsed
%%
%% RETURNS:      - ok -> No valid command towards such "Dev_id" was already parsed
%%
%% THROWS:       - {duplicate,devid} -> A valid command towards such "Dev_id" was already parsed
%%
check_devcommand_duplicate(Dev_id,ValDevIds) ->

 % Check if ""Dev_id" belongs to the list of "ValDevIds"
 % associated with valid commands already parsed
 case lists:member(Dev_id,ValDevIds) of
 
  % If it does, throw an error
  true ->
   throw({duplicate,devid});
  
  % Otherwise return 'ok'
  false ->
   ok
 end.
 
 
%% DESCRIPTION:  Returns the "actions" map of a device command
%%
%% ARGUMENTS:    - DevCommand: The device command which to retrieve the "action" map
%%
%% RETURNS:      - Actions -> The device command "action" map
%%
%% THROWS:       - {missing_param,actions} -> The "actions" key was not found in the device command map
%%               - {not_a_map,actions}     -> The "actions" parameter was not
%%                                            interpreted as a map by the JSONE library
%%               - {empty,actions}         -> The "actions" map is empty
%%
get_devcommand_actions(DevCommand) ->
  
 % Attempt to retrieve the value of key "actions" from the device command map
 Actions = try maps:get(<<"actions">>,DevCommand)
           catch
	 		 
		    % If the "actions" key was
		    % not found, throw an error
	        error:{badkey,<<"actions">>} ->
		     throw({missing_param,actions})
	       end,

 % Ensure "Actions" to be a map
 if
  is_map(Actions) =:= true ->
  
   % If it is, ensure it not to be empty
   if
    map_size(Actions) > 0 ->
   
     % If it not, return the "actions" map
	 Actions;
	 
	true ->
	
	 % If it is, throw an error
	 throw({empty,actions})
   end;
  
  true -> 
  
   % If it is not a map, throw an error
   throw({not_a_map,actions})
 end.  


%% DESCRIPTION:  Ensures a device "Dev_id" to belong to the location and to be currently paired with
%%               the controller, and returns its type and the PID of its 'ctr_devhandler' process
%%
%% ARGUMENTS:    - Dev_id: The "Dev_id" of the device to check
%%
%% RETURNS:      - {Type,HandlerPID} -> The device's type and the PID of its 'ctr_devhandler' process
%%
%% THROWS:       - device_not_exists -> Device "Dev_id" does not belong to the controller location
%%               - device_offline    -> Device "Dev_id" is currently not paired with the controller
%%
get_devcommand_devinfo(Dev_id) ->

 % Ensure the device to belong to the location
 case ctr_db:get_record(ctr_device,Dev_id) of 
  {error,not_found} ->
  
   % If it does not, throw an error
   throw(device_not_exists);
   
  {ok,CtrDevRecord} ->
  
   % If it does, ensure it to be currently paired
   % with the controller via the 'handler_pid' field
   case CtrDevRecord#ctr_device.handler_pid of
	
	% If the device is currently not paired
	% with the controller, throw an error	
	'-' ->
	 throw(device_offline);
	
	% Otherwise return the device Type and HandlerPID
	HandlerPID when is_pid(HandlerPID) ->
	 {CtrDevRecord#ctr_device.type,HandlerPID}
   end
 end.
   

%% DESCRIPTION:  Parses the "actions" map of a device command and returns the configuration
%%               command record of the appropriate #devtype to be issued to the device
%%
%% ARGUMENTS:    - Actions: The "actions" map associated with the device command
%%               - Type:    The device type (atom)
%%
%% RETURNS:      - CfgCommand -> The configuration command record of the
%%                               appropriate #devtype to be issued to the device
%%
%% THROWS:       - {invalid_action,Action,InvalidValue} -> Action "Action" is of invalid value "InvalidValue"
%%               - {invalid_actions,Type}               -> The "actions" map contains invalid actions for the device Type
%%               - {invalid_state,door}                 -> The configuration command would set the device
%%                                                         of type 'door' in the invalid state {open,lock}
%%               - {unknown_jsone_cast,InvalidTrait}    -> An invalid trait value was cast to an
%%                                                         unknown Erlang term() by the JSONE library 
%%               - {invalid_db_devtype,InvalidDevType}  -> The device type in the controller Mnesia database is invalid
%%
%% NOTE:         No duplicate actions (keys) can be present since the JSONE library only returns the first
%%               occurence of each key in a map (https://github.com/sile/jsone/blob/master/doc/jsone.md#types)
%%
build_cfgcommand(Actions,Type) ->

 % Build the configuration command according to the device type,
 % also obtaining the residual contents of the "actions" map
 {CfgCommand,EmptyActions} = build_cfgcommand_type(Actions,Type),

 % Ensure the residual "actions" map to be empty, meaning that
 % were no invalid actions for the associated device type
 if
  map_size(EmptyActions) =:= 0 ->
  
   % If no invalid actions were found,
   % return the configuration command
   CfgCommand;
   
  true ->
  
   % If invalid actions were
   % found, throw an error
   throw({invalid_actions,Type})
 end.


%% Retrieves the device traits' values from an "action" map and returns the configuration command record of the appropriate 
%% #devtype as well as the residual contents of the "actions" map (parse_devcommand_actions(Actions,Type) helper function)
%%
%% NOTE: The residual "actions" map is derived by removing from it each device-specific action being processed
%%
%% ---------------- Build Fan Configuration Command ---------------- %%
build_cfgcommand_type(Actions,fan) ->
 
 % 'onoff' trait ('on'|'off')
 {OnOff,Actions1} = get_trait_value(onoff,Actions),
 
 % 'fanspeed' trait	(0 < fanspeed <= 100) 
 {FanSpeed,Actions2} = get_trait_value(fanspeed,Actions1),
 
 % Return the configuration command and the residual "Actions" map
 {#fancfg{onoff = OnOff, fanspeed = FanSpeed},Actions2};
 
%% --------------- Build Light Configuration Command --------------- %%
build_cfgcommand_type(Actions,light) ->
 
 % 'onoff' trait ('on'|'off')
 {OnOff,Actions1} = get_trait_value(onoff,Actions),
 
 % 'brightness' trait (0 < brightness <= 100)
 {Brightness,Actions2} = get_trait_value(brightness,Actions1),
 
 % "colorsetting" trait (any list/string)
 {ColorSetting,Actions3} = get_trait_value(colorsetting,Actions2),

 % Return the configuration command and the residual "Actions" map
 {#lightcfg{onoff = OnOff, brightness = Brightness, colorsetting = ColorSetting},Actions3};

%% ---------------- Build Door Configuration Command ---------------- %%
build_cfgcommand_type(Actions,door) ->
 
 % 'openclose' trait ('open'|'close')
 {OpenClose,Actions1} = get_trait_value(openclose,Actions),
 
 % 'lockunlock' trait ('lock'|'unlock')
 {LockUnlock,Actions2} = get_trait_value(lockunlock,Actions1),
 
 % Ensure that the resulting configuration command would
 % not set the door in the invalid state {open,lock}
 if
  OpenClose =:= 'open' andalso LockUnlock =:= 'lock' ->
  
   % If it would, throw an error
   throw({invalid_state,door});
   
  true ->
 
  % Otherwise return the configuration command and the residual "Actions" map
  {#doorcfg{openclose = OpenClose, lockunlock = LockUnlock},Actions2}
 end;

%% ------------- Build Thermostat Configuration Command ------------- %%
build_cfgcommand_type(Actions,thermostat) ->
 
 % 'onoff' trait ('on'|'off')
 {OnOff,Actions1} = get_trait_value(onoff,Actions),
 
 % 'temp_target' trait (0 <= temp_target <= 50)
 {TempTarget,Actions2} = get_trait_value(temp_target,Actions1),

 % NOTE: The 'temp_current' trait cannot be changed via a device command

 % Return the configuration command and the residual "Actions" map
 {#thermocfg{onoff = OnOff, temp_target = TempTarget, temp_current = '$keep'},Actions2};
 
%% ------------ Build Conditioner Configuration Command ------------ %%
build_cfgcommand_type(Actions,conditioner) ->
 
 % 'onoff' trait ('on'|'off')
 {OnOff,Actions1} = get_trait_value(onoff,Actions),
 
 % 'temp_target' trait (0 <= temp_target <= 50)
 {TempTarget,Actions2} = get_trait_value(temp_target,Actions1),
 
 % NOTE: The 'temp_current' trait cannot be changed via a device command 
 
 % 'fanspeed' trait	(0 < fanspeed <= 100) 
 {FanSpeed,Actions3} = get_trait_value(fanspeed,Actions2),
 
 % Return the configuration command and the residual "Actions" map
 {#condcfg{onoff = OnOff, temp_target = TempTarget, temp_current = '$keep', fanspeed = FanSpeed},Actions3}; 

%% ---------------------- Invalid Device Type ---------------------- %%
build_cfgcommand_type(_,InvalidDevType) ->
 
 % If the device type in the controller's
 % database is invalid, throw an error
 throw({invalid_db_devtype,InvalidDevType}).
 

%% Returns the value of a device trait to be used in a configuration command, which is obtained by
%% extracting its associated action from the "actions" map (if present) or set to '$keep' otherwise,
%% also returning the residual "actions" map (build_cfgcommand_type(Actions,Type) helper function) 
%%
%% ---------------- "onOff" action -> 'onoff' trait ---------------- %%
% Valid "OnOff" action ("on"|"off")
get_trait_value(onoff,Action=#{<<"onOff">> := OnOff}) when OnOff =:= <<"on">> orelse OnOff =:= <<"off">> ->
 {binary_to_atom(OnOff),maps:remove(<<"onOff">>,Action)};

% Invalid "OnOff" action 
get_trait_value(onoff,#{<<"onOff">> := InvalidOnOff}) ->
 throw({invalid_action,"onOff",gen_resthandler:jsone_term_to_list(InvalidOnOff,onoff)});

%% ------------- "fanSpeed" action -> 'fanspeed' trait ------------- %%
% Valid "fanSpeed" action (0 < fanSpeed <= 100)
get_trait_value(fanspeed,Action=#{<<"fanSpeed">> := FanSpeed}) when is_integer(FanSpeed), FanSpeed > 0, FanSpeed =< 100 ->
 {FanSpeed,maps:remove(<<"fanSpeed">>,Action)};
  
% Invalid "fanSpeed" action
get_trait_value(fanspeed,#{<<"fanSpeed">> := InvalidFanSpeed}) ->
 throw({invalid_action,"fanSpeed",gen_resthandler:jsone_term_to_list(InvalidFanSpeed,fanspeed)});
 
%% ----------- "brightness" action -> 'brightness' trait ----------- %%
% Valid "brightness" action (0 < brightness <= 100)
get_trait_value(brightness,Action=#{<<"brightness">> := Brightness}) when is_integer(Brightness), Brightness > 0, Brightness =< 100 ->
 {Brightness,maps:remove(<<"brightness">>,Action)};

% Invalid "brightness" action
get_trait_value(brightness,#{<<"brightness">> := InvalidBrightness}) ->
 throw({invalid_action,"brightness",gen_resthandler:jsone_term_to_list(InvalidBrightness,brightness)});
  
%% ------------- "color" action -> 'colorsetting' trait ------------- %% 
% Valid "color" action (any)
get_trait_value(colorsetting,Action=#{<<"color">> := Color}) ->
 {gen_resthandler:jsone_term_to_list(Color,colorsetting),maps:remove(<<"color">>,Action)};
 
% NOTE: The "color" action cannot be invalid, with its value always being interpreted as a list

%% ------------- "openClose" action -> 'openclose' trait ------------- %% 
% Valid "openClose" action ("open"|"close")
get_trait_value(openclose,Action=#{<<"openClose">> := OpenClose}) when OpenClose =:= <<"open">> orelse OpenClose =:= <<"close">> ->
 {binary_to_atom(OpenClose),maps:remove(<<"openClose">>,Action)};
 
% Invalid "openClose" action
get_trait_value(openclose,#{<<"openClose">> := InvalidOpenClose}) ->
 throw({invalid_action,"openClose",gen_resthandler:jsone_term_to_list(InvalidOpenClose,openclose)});
 
%% ------------ "lockUnlock" action -> 'lockunlock' trait ------------ %%  
% Valid "lockUnlock" action ("lock"|"unlock")
get_trait_value(lockunlock,Action=#{<<"lockUnlock">> := LockUnlock}) when LockUnlock =:= <<"lock">> orelse LockUnlock =:= <<"unlock">> ->
 {binary_to_atom(LockUnlock),maps:remove(<<"lockUnlock">>,Action)};
 
% Invalid "lockUnlock" action
get_trait_value(lockunlock,#{<<"lockUnlock">> := InvalidLockUnlock}) ->
 throw({invalid_action,"lockUnlock",gen_resthandler:jsone_term_to_list(InvalidLockUnlock,lockunlock)});

%% ------------ "tempTarget" action -> 'temp_target' trait ------------ %%  

% Valid "tempTarget" action (0 <= tempTarget <= 50)
get_trait_value(temp_target,Action=#{<<"tempTarget">> := TempTarget}) when is_integer(TempTarget), TempTarget >= 0, TempTarget =< 50 ->
 {TempTarget,maps:remove(<<"tempTarget">>,Action)};

% Invalid "tempTarget" action
get_trait_value(temp_target,#{<<"tempTarget">> := InvalidTempTarget}) ->
 throw({invalid_action,"tempTarget",gen_resthandler:jsone_term_to_list(InvalidTempTarget,temp_target)});
 
%% NOTE: The 'temp_current' trait has no associated action (it cannot be changed via a command)

%% ------------- Trait action not found action -> '$keep' ------------- %%   
get_trait_value(_,Action) ->
 {'$keep',Action}.


%% ===================================================== COMMANDS ISSUING ========================================================== %% 

%% DESCRIPTION:  Issues a list of valid commands to their respective devices, processing
%%               their responses and returning the lists of successful and failed commands
%%
%% ARGUMENTS:    - ValidCommands: The list of valid commands to be issued to their respective devices
%%
%% RETURNS:      - {SuccessfulCommands,FailedCommands} -> The lists of successful and failed device commands
%%
issue_devcommands([]) ->
 
 % If no valid command was passed, directly return the empty lists of successful and failed commands
 {[],[]};
 
% If at least a valid device command was passed
issue_devcommands(ValidCommands) ->

 % Issue the command to their respective devices and process their responses
 Devresponses = 
 try
 
  % Retrieve the process PID
  ParentPID = self(),
  
  % Spawn a "CmdClient" process for issuing the commands to their respective
  % devices via non-blocking calls and collecting their responses
  CmdClientPid = proc_lib:spawn(fun() -> cmdclient(ParentPID,ValidCommands) end), 

  % Create a monitor towards the CmdClient
  CmdClientRef = monitor(process,CmdClientPid),

  % Wait for the CmdClient to return the devices'
  % responses up to a predefined timeout
  receive
 
   % All devices' responses were collected by the CmdClient
   {devresponses_ready,CmdClientPid} ->
    ok;
	    
   % The CmdClient has terminated normally (which
   % implies that all devices' responses were collected)
   {'DOWN',CmdClientRef,process,CmdClientPid,normal} ->
    ok;
   
   % If the CmdClient has crashed, throw an error
   {'DOWN',CmdClientRef,process,CmdClientPid,CmdClientCrashReason} ->
    throw({cmdclient_crash,CmdClientCrashReason})

  % If the device responses timeout
  % expires, kill the CmdClient process
  after ?Devresponses_timeout ->
   exit(CmdClientPid,shutdown)
	
  end,
  
  % If still active, remove the monitor towards the CmdClient,
  % also flushing possible notifications from the message queue  
  demonitor(CmdClientRef,[flush]),
 
  % Retrieve and parse the device commands' responses,
  % obtaining the lists of successful and failed commands
  {SuccessfulDevCommands,FailedDevCommands} = parse_devcommands_responses(ValidCommands,[],[]),
  
  % Return the lists of successful and failed device commands
  {ok,SuccessfulDevCommands,FailedDevCommands}
  
 catch
 
  % If the CmdClient has crashed, return an error notifying
  % that the devices' responses could not be parsed
  {cmdclient_crash,CrashReason} ->
   {error,{cmdclient_crash,CrashReason}};
   
  % If an unhandled error occurs, return that the
  % the devices' responses could not be parsed     
  _:UnhandledError ->
   {error,{unhandled,UnhandledError}}
 end,
 
 % Depending on whether the devices' responses were correctly parsed
 case Devresponses of
 
  % If they were, return the lists of
  % successful and failed device commands
  {ok,SuccessfulCommands,FailedCommands} ->
   {SuccessfulCommands,FailedCommands};
  
  % If an error occured in parsing the devices' responses
  {error,Reason} ->
  
   % Retrieve the controller's location ID for logging purposes
   {ok,Loc_id} = application:get_env(janet_controller,loc_id),
   
   % Report the error
   io:format("[issue_devcommands-~w]: error: ~p~n",[Loc_id,Reason]),
   
   % Convert all valid in failed commands with the same error Reason
   FailedCommands = [#{dev_id => Dev_id,
                       status => 500,
					   errorReason => list_to_binary(lists:flatten(io_lib:format("Server error in issuing the device commands: ~p",[Reason])))}
					 || {_,Dev_id,_,_,_} <- ValidCommands ],
   
   % Return the lists of successful (empty) and failed commands
   {[],FailedCommands}
 end.

  
%% DESCRIPTION:  Retrieves and parses the device commands responses and
%%               returns the lists of successful and failed commands
%%
%% ARGUMENTS:    - ValidCommands: The list of valid commands that were issued
%%
%% RETURNS:      - {SuccessfulCommands,FailedCommands} -> The lists of successful and failed device commands
%%  
parse_devcommands_responses([],SuccessfulCommands,FailedCommands) ->

 % Return the lists of successful and failed device commands
 {SuccessfulCommands,FailedCommands};

parse_devcommands_responses([IssuedCommand|NextIssuedCommand],SuccessfulCommands,FailedCommands) ->

 % Retrieve the "Dev_id" associated with the current command
 Dev_id = IssuedCommand#valdevcmd.dev_id,
 
 % Retrieve the command response of device "Dev_id"
 % and determine whether it was successful or not 
 CommandResponse =
 try 
  receive
 
   % If the command response of device "Dev_id" was received
   {devresponse,Dev_id,Devhandler_Reply} ->
  
    % Depending on the response returned by the
	% 'ctr_devhandler' associated with the device
    case Devhandler_Reply of

     %% ---------------------------------- Device Command Successful ---------------------------------- %
	 
     % If the device command was successfully executed
     {ok,{UpdatedCfgMap,Timestamp}} ->
	 
	  % Return the successful device command
	  {ok,#{dev_id => IssuedCommand#valdevcmd.dev_id, status => 200, updatedState => UpdatedCfgMap, timestamp => utils:timestamp_to_binary(Timestamp)}};
	 
	 %% ------------------------------------ Device Command Failed ------------------------------------ %

	 % The device rejected the configuration command
     {error,invalid_devconfig} ->
	  {error,#{dev_id => IssuedCommand#valdevcmd.dev_id, status => 406, errorReason => <<"The command was rejected by the device for it is invalid in its current state">>}};
   
     % Device node timeout
     {error,dev_timeout} ->
	  {error,#{dev_id => IssuedCommand#valdevcmd.dev_id, status => 504, errorReason => <<"The device node is not responding">>}};
	 
	 % Device state machine timeout
	 {error,statem_timeout} ->
	  {error,#{dev_id => IssuedCommand#valdevcmd.dev_id, status => 504, errorReason => <<"The device state machine is not responding">>}};
	
 	 % An unexpected response was received from the devhandler
	 UnexpectedResponse ->
      io:format("[parse_devcommands_responses]: An unexpected device command response was returned by the devhandler of device \"~p\": ~p",[Dev_id,UnexpectedResponse]),
      {error,#{dev_id => IssuedCommand#valdevcmd.dev_id, status => 500, errorReason => list_to_binary(lists:flatten(io_lib:format("An unexpected device command response was received: ~p",[UnexpectedResponse])))}}
    end
 
 
  % Otherwise if a command response from device "Dev_id" was not received, which may happen from reasons ranging from the device and/or
  % the controller being overloaded, the device/handler stopped inbetween the command processing, etc, set the command as failed
  after
   0 ->
    io:format("[parse_devcommands_responses]: Missing command response of device \"~p\"~n",[IssuedCommand#valdevcmd.dev_id]),
    {error,#{dev_id => IssuedCommand#valdevcmd.dev_id, status => 504, errorReason => <<"No command response was returned from the device">>}}
  end

 catch
 
  % The updated configuration returned by the device is malformed
  {error,invalid_devtype} ->
    {error,#{dev_id => IssuedCommand#valdevcmd.dev_id, status => 500, errorReason => <<"The updated state returned by the device is malformed">>}};

  % Unhandled error in parsing the device command response
  _:UnhandledError ->
   io:format("[parse_devcommands_responses]: Unhandled error: ~p~n",[UnhandledError]),
   {error,#{dev_id => IssuedCommand#valdevcmd.dev_id, status => 500, errorReason => list_to_binary(lists:flatten(io_lib:format("Unhandled server error in parsing the command response: ~p",[UnhandledError])))}}
 
 end,  
 
 % Depending on whether the command towards device "Dev_id" was successful or not
 case CommandResponse of
 
  % If it was, append it into the list of successful commands and proceed with the next issued command
  {ok,SuccessfulCommand} ->
   parse_devcommands_responses(NextIssuedCommand,SuccessfulCommands ++ [SuccessfulCommand],FailedCommands);
   
  % If it was not, append it into the list of failed commands and proceed with the next issued command
  {error,FailedCommand} ->
   parse_devcommands_responses(NextIssuedCommand,SuccessfulCommands,FailedCommands ++ [FailedCommand])
 end.
 

%% ------------------------------------------------------- CMDCLIENT PROCESS ------------------------------------------------------- %% 

%% DESCRIPTION:  Body function of the "CmdClient" process spawned by the 'ctr_resthandler' for issuing the
%%               commands to their respective devices via non-blocking calls and collecting their responses
%%
%% ARGUMENTS:    - ParentPID:     The PID of the parent 'ctr_resthandler' process
%%               - ValidCommands: The list of valid commands to be issued to their respective devices
%%
%% RETURNS:      none
%%    
cmdclient(ParentPID,ValidCommands) ->

 % Create a monitor towards the
 % 'ctr_resthandler' parent process
 monitor(process,ParentPID),
 
 % Send the commands to their
 % respective devices in multicall 
 ok = cmdclient_send_devcommands(ValidCommands),
 
 % Retrieve the device commands responses
 % and forward them to the '
 
 % Retrieve the devices responses and forward
 % them to the 'ctr_resthandler' parent process
 cmdclient_receive_devresponses(ValidCommands,ParentPID).
  
  
%% Sends a list of device commands to their respective device handlers via
%% non-blocking calls (cmdclient(ParentPID,ValidCommands) helper function)
cmdclient_send_devcommands([]) ->

 % All device commands were sent
 ok;
 
cmdclient_send_devcommands([ValDevCmd|NextValDevCmd]) ->

 % Forward the command to the 'ctr_devhandler' associated with the device via a low-level non-blocking call
 erlang:send(ValDevCmd#valdevcmd.devhandler_pid,{'$gen_call',{self(),ValDevCmd#valdevcmd.devhandler_ref},{dev_config_change,ValDevCmd#valdevcmd.cfgcommand}}),
 
 % Proceed with the next command
 cmdclient_send_devcommands(NextValDevCmd).
  
  
%% Retrieves the device commands' responses and forwards them to the 'ctr_resthandler'
%% parent process (cmdclient(ParentPID,ValidCommands) helper function)  
cmdclient_receive_devresponses([],ParentPID) ->
 
 % If all device responses were forwarded to the 'ctr_resthandler'
 % parent process, signal it that they are ready to be collected
 ParentPID ! {devresponses_ready,self()};
 
cmdclient_receive_devresponses(PendingCmdResponses,ParentPID) ->

 % Wait for a device command response so to determine
 % the new list of commands pending a response
 NewPendingCmdResponses = 
 receive
 
  % If the 'ctr_resthandler' parent process
  % has crashed, report the error and exit
  {'DOWN',_Ref,process,_Parent,_Reason} ->
   io:format("[ctr_cmdclient]: <ERROR> The 'ctr_resthandler' parent has crashed, exiting~n"),
   exit(normal);
   
  % If a device command response has been received
  {Devhandler_Ref,Devhandler_Reply} ->
  
   % Attempt to match the response to its source command via the 'devhandler_ref' field
   case lists:keytake(Devhandler_Ref,5,PendingCmdResponses) of 
    false ->
	
	 % If the response did not match any command, report the error
	 % and return the same list of commands pending a response
	 io:format("[ctr_cmdclient]: <ERROR> An unexpected command response with reference \"~p\" was received: ~p~n",[Devhandler_Ref,Devhandler_Reply]),
	 PendingCmdResponses;
   
    {value,MatchedCommand,NewPendingCmdResponses_} ->
	
	 % If the response was successfully matched to its source
 	 % command, forward it to the 'ctr_resthandler' parent process
     ParentPID ! {devresponse,MatchedCommand#valdevcmd.dev_id,Devhandler_Reply},
	 
	 % Return the updated list of commands pending a response
	 NewPendingCmdResponses_ 
   end
 end,
 
 % Wait for the next response using the updated list of commands pending a response
 cmdclient_receive_devresponses(NewPendingCmdResponses,ParentPID).


%% ================================================= HTTP RESPONSE GENERATION ====================================================== %%

%% DESCRIPTION:  Determines and returns the status code and body of the HTTP response
%%               to be returned to the client that issued a list of device commands
%%
%% ARGUMENTS:    - SuccessfulCommands: The list of successful device commands
%%               - FailedCommands:     The list of failed device commands
%%               - InvalidCommands:    The list of invalid device commands
%%
%% RETURNS:      - {RespCode,RespBody} -> The status code and body of the HTTP response to be
%%                                        returned to the client, the latter encoded in JSON
%% THROWS:       - jsone_encode_error -> The lists of device commands results
%%                                       could not be encoded in JSON format
%%
build_devcommands_response(SuccessfulCommands,FailedCommands,InvalidCommands) ->
 
 % Define the list of unsuccessful commands as the
 % concatenation of the lists of failed and invalid commands
 UnsuccessfulCommands = FailedCommands ++ InvalidCommands,
 
 % Set the body of the HTTP response to be returned to the client as the encode in JSON
 % format of the concatenation of the lists of successful and unsuccessful commands
 ResBody = try jsone:encode(SuccessfulCommands ++ UnsuccessfulCommands)
           catch
		   
		    % If the concatenation of the two lists could
			% not be encoded in JSON format, throw an error
		    error:badarg ->
		 	 throw(jsone_encode_error)
		   end,
 
 % Determine the HTTP status code to be replied to the client
 ResCode = get_devcommands_statuscode(SuccessfulCommands,UnsuccessfulCommands),
			
 % Return the status code and the body of the
 % HTTP response to be replied to the client
 {ResCode,ResBody}.


%% Determines the HTTP status code to be replied to a client that issued a list of device commands
%% (build_devcommands_response(SuccessfulCommands,InvalidResponses,InvalidCommands) helper function)
get_devcommands_statuscode(_SuccessfulCommands,[]) ->

 % If all commands were successful, return 200 (OK)
 200;

get_devcommands_statuscode([],UnsuccessfulCommands) ->

 % If all commands were unsuccessful, return the highest among their error codes
 lists:foldl(fun(#{status := ErrCode},MaxCode) -> max(ErrCode,MaxCode) end,0,UnsuccessfulCommands);
 
get_devcommands_statuscode(_ValidResponses,_UnsuccessfulCommands) ->
 
 % If there are both successful and unsuccessful commands, return 202 (ACCEPTED)
 202.
 

%% DESCRIPTION:  Prints a summary of the device commands issuing operation
%%
%% ARGUMENTS:    - SuccessfulCommands: The list of successful device commands
%%               - FailedCommands:     The list of failed device commands
%%               - ValidCommands:      The list of valid device commands
%%               - InvalidCommands:    The list of invalid device commands
%%
%% RETURNS:      ok -> A summary of the device commands issuing operation was printed
%%
print_devcommands_summary(SuccessfulCommands,FailedCommands,ValidCommands,InvalidCommands,RespCode) ->

 % Retrieve the controller's location ID
 {ok,Loc_id} = application:get_env(janet_controller,loc_id),

 % Print a summary of the device commands issuing operation
 print_devcommands_summary(SuccessfulCommands,FailedCommands,ValidCommands,InvalidCommands,RespCode,Loc_id).

%% ------------------------------------------ Single Device Command ------------------------------------------ %%
% Successful command
print_devcommands_summary([#{dev_id := Dev_id, updatedState := UpdatedStateMap}],[],[ValidCommand],[],200,Loc_id) when ValidCommand#valdevcmd.dev_id =:= Dev_id ->
 io:format("~n[devcommands_handler-~w]: <SUCCESS> dev_id = ~w, command = ~200p, updatedState = ~200p (RespCode = 200)~n",[Loc_id,Dev_id,ValidCommand#valdevcmd.cfgcommand,UpdatedStateMap]);

% Failed command
print_devcommands_summary([],[#{dev_id := Dev_id, errorReason := ErrorReason}],[ValidCommand],[],RespCode,Loc_id) when ValidCommand#valdevcmd.dev_id =:= Dev_id ->
 io:format("~n[devcommands_handler-~w]: <FAILED> dev_id = ~w, command = ~200p, errorReason = ~200p (RespCode = ~w)~n",[Loc_id,Dev_id,ValidCommand#valdevcmd.cfgcommand,ErrorReason,RespCode]);
 
% Invalid command
print_devcommands_summary([],[],[],[#{dev_id := Dev_id, errorReason := ErrorReason}],RespCode,Loc_id) ->
 io:format("~n[devcommands_handler-~w]: <INVALID> dev_id = ~p, errorReason = ~200p (RespCode = ~w)~n",[Loc_id,Dev_id,ErrorReason,RespCode]);
 
%% ------------------------------------------- Multi Device Command ------------------------------------------- %%
print_devcommands_summary(SuccessfulCommands,FailedCommands,ValidCommands,InvalidCommands,RespCode,Loc_id) ->

 % Print the header of the multiple device commands issuing operation
 io:format("~n[devcommands_handler-~w]: MULTI-COMMAND (RespCode = ~w)~n",[Loc_id,RespCode]),
 
 % Print the list of SUCCESSFUL device commands indented as a tree, also obtaining the list of valid, but failed, commands
 ValidFailedCommands = print_successful_devcommands(SuccessfulCommands,ValidCommands,devcommands_summary_indent(FailedCommands++InvalidCommands)),
  
 % Print the list of FAILED device commands indented as a tree
 print_failed_devcommands(FailedCommands,ValidFailedCommands,devcommands_summary_indent(InvalidCommands)),
 
 % Print the list of INVALID device commands indented as a tree (NOTE: in this case the indentation is fixed)
 print_invalid_devcommands(InvalidCommands,"   ").
 
%% -------------------------------------- SUCCESSFUL Commands -------------------------------------- %%

%% Prints the tree of successful device commands (print_devcommands_summary(SuccessfulCommands,
%% FailedCommands,ValidCommands,InvalidCommands,RespCode,Loc_id) helper function)
print_successful_devcommands([],ValidFailedCommands,_Indent) ->

 % If no command was successful, directly return the list of valid
 % commands, all of which will be associated with failed commands
 ValidFailedCommands;
 
print_successful_devcommands(SuccessfulCommands,ValidCommands,Indent) ->
 
  % If at least one command was successful,
  % print the successful commands tree header
  io:format("|--[SUCCESSFUL]~n"),
 
  % Print each successful command indented as a tree
  print_successful_devcommands_tree(SuccessfulCommands,ValidCommands,Indent).
 
%% Print each successful device command indented as a tree (print_devcommands_summary(SuccessfulCommands,FailedCommands,ValidCommands,
%% InvalidCommands,RespCode,Loc_id) -> print_successful_devcommands(SuccessfulCommands,ValidCommands,Indent) helper function)
print_successful_devcommands_tree([],ValidFailedCommands,_Indent) ->

 % After printing each successful command return the residual list of
 % valid commands, which will be associated with failed commands
 ValidFailedCommands;
 
print_successful_devcommands_tree([#{dev_id := Dev_id, updatedState := UpdatedStateMap}|NextSuccessCommand],ValidCommands,Indent) ->

 % Match the successful to its associated valid command
 {value,MatchValidCommand,NewValidCommands} = lists:keytake(Dev_id,2,ValidCommands),
 
 % Print a summary of the successful command
 io:format("~s|-- dev_id = ~w, command = ~200p, updatedState = ~200p (RespCode = 200)~n",[Indent,Dev_id,MatchValidCommand#valdevcmd.cfgcommand,UpdatedStateMap]),
 
 % Proceed with the next successful command
 print_successful_devcommands_tree(NextSuccessCommand,NewValidCommands,Indent).

%% ---------------------------------------- FAILED Commands ---------------------------------------- %%

%% Prints the tree of failed device commands (print_devcommands_summary(SuccessfulCommands,
%% FailedCommands,ValidCommands,InvalidCommands,RespCode,Loc_id) helper function)
print_failed_devcommands([],_ValidFailedCommands,_Indent) ->

 % If there are no failed commands, return
 ok;
 
print_failed_devcommands(FailedCommands,ValidFailedCommands,Indent) ->

 % If at least one command failed, print
 % the failed commands tree header
 io:format("|--[FAILED]~n"),
 
 % Print each failed command indented as a tree
 print_failed_devcommands_tree(FailedCommands,ValidFailedCommands,Indent).

%% Print each failed device command indented as a tree (print_devcommands_summary(SuccessfulCommands,FailedCommands,ValidCommands,
%% InvalidCommands,RespCode,Loc_id) -> print_failed_devcommands(FailedCommands,ValidFailedCommands,Indent) helper function)
print_failed_devcommands_tree([],_ValidFailedCommands,_Indent) ->

 % When all failed commands
 % have been printed, return
 ok;
 
print_failed_devcommands_tree([#{dev_id := Dev_id, status := Status, errorReason := ErrorReason}|NextFailedCommand],ValidFailedCommands,Indent) ->

 % Match the failed to its associated valid command
 {value,MatchValidCommand,NewValidFailedCommands} = lists:keytake(Dev_id,2,ValidFailedCommands),
 
 % Print a summary of the failed command
 io:format("~s|-- dev_id = ~w, command = ~200p, errorReason = ~200p (RespCode = ~w)~n",[Indent,Dev_id,MatchValidCommand#valdevcmd.cfgcommand,ErrorReason,Status]),
 
 % Proceed with the next failed command
 print_failed_devcommands_tree(NextFailedCommand,NewValidFailedCommands,Indent).
 
%% --------------------------------------- INVALID Commands --------------------------------------- %%

%% Prints the tree of invalid device commands (print_devcommands_summary(SuccessfulCommands,
%% FailedCommands,ValidCommands,InvalidCommands,RespCode,Loc_id) helper function)
print_invalid_devcommands([],_Indent) ->

 % If there are no invalid commands, return
 ok;
 
print_invalid_devcommands(InvalidCommands,Indent) ->

 % If at least one command was invalid,
 % print the invalid commands tree header
 io:format("|--[INVALID]~n"),
 
 % Print each invalid command indented as a tree
 print_invalid_devcommands_tree(InvalidCommands,Indent).
 
%% Print each invalid device command indented as a tree (print_devcommands_summary(SuccessfulCommands,FailedCommands,
%% ValidCommands,InvalidCommands,RespCode,Loc_id) -> print_invalid_devcommands(InvalidCommands,Indent) helper function)
print_invalid_devcommands_tree([],_Indent) ->
 
 % When all invalid commands
 % have been printed, return
 ok;
 
print_invalid_devcommands_tree([#{dev_id := Dev_id, status := Status, errorReason := ErrorReason}|NextInvalidCommand],Indent) ->

 % Print a summary of the invalid command
 io:format("~s|-- dev_id = ~p, errorReason = ~200p (RespCode = ~w)~n",[Indent,Dev_id,ErrorReason,Status]),
 
 % Proceed with the next invalid command
 print_invalid_devcommands_tree(NextInvalidCommand,Indent).
 

%% Determines the indentation to be applied when printing each tree of successful, failed and invalid commands
%% (print_devcommands_summary(SuccessfulCommands,FailedCommands,ValidCommands,InvalidCommands,RespCode,Loc_id) helper function)
devcommands_summary_indent(List) when is_list(List), length(List) > 0 ->

 % If there are other lists of commands to
 % print after this one (Failed or Invalid)
 "|  ";
 
devcommands_summary_indent(List) when is_list(List) ->
 
 % If this is the last list of command to print
 "   ".
 
 
%%====================================================================================================================================
%%                                                         START FUNCTION                                                        
%%====================================================================================================================================

%% Called by the Janet Controller top-level supervisor (sup_jctr) at boot time
start_link() ->
 gen_resthandler:start_link(?MODULE,[]).