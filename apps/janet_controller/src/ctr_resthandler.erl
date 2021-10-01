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
         add_device_handler/2,                  %                 operation handlers
		 update_dev_subloc_handler/2,           %
		 delete_device_handler/2]).             %   
-export([res_devcommands_handler/1,             % /devcommands resource handler
         devcommands_handler/2]).               %              operation handler

-include("ctr_mnesia_tables_definitions.hrl").  % Janet Controller Mnesia Tables Records Definitions
-include("devtypes_configurations_definitions.hrl").  % Janet Device Configuration Records Definitions

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
			 
          % RESOURCE: /devcommands
		  % ALLOWED METHODS: 
          %   - PATCH -> devcommands([{Dev_id,[Commands]}])
		  %		
		  {"/devcommands",?MODULE,res_devcommands_handler}
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

% The device was added into the database, but an internal error occured in starting its node
err_to_code_msg({device_not_started,Dev_id,InternalError}) ->
 {500,io_lib:format("<SERVER ERROR> The device of \"dev_id\" ~w was added, but an internal error occured in starting its node: ~p",[Dev_id,InternalError])};

% The device was added into the Simulator database, but internal errors occured in
% starting its node and in adding it into the Controller database (should NEVER happen)
err_to_code_msg({add_dev_start_ctrdb_fail,Dev_id,InternalSimError,CtrDBError}) ->
 {500,io_lib:format("<SERVER ERROR> The device of \"dev_id\" ~w was added in the Simulator database, but internal errors occured in " ++
                    "starting its node (~p) and in adding it into the Controller database (~p)",[Dev_id,InternalSimError,CtrDBError])};   

%% UPDATE_DEV_SUBLOC + DELETE_DEVICE (POST /device/:dev_id + DELETE /device/:dev_id)
%% ---------------------------------
% Trying to operate on a device that does not exist
err_to_code_msg({device_not_exists,Dev_id}) ->
 {404,io_lib:format("<ERROR> A device with such \"dev_id\" (~w) does not exist",[Dev_id])};

%% UPDATE_DEV_SUBLOC (POST /device/:dev_id)
%% -----------------
% The device sublocation was updated in the Simulator but
% not in the Controller database (should NEVER happen)
err_to_code_msg({ctrdb_update_dev_subloc_fail,Dev_id,CtrDBError}) ->
 {500,io_lib:format("<SERVER ERROR> Sublocation of device with \"dev_id\" ~w was updated in the Simulator database, but "
                    "an internal error occured in updating it in the Controller database (~p)",[Dev_id,CtrDBError])};   

%% DELETE_DEVICE (DELETE /device/:dev_id)
%% -------------
% The device was deleted, but an internal error occured in stopping its node
err_to_code_msg({device_node_not_stopped,Dev_id,InternalError}) ->
 {500,io_lib:format("<SERVER ERROR> The device of \"dev_id\" ~w was deleted, but an internal error occured in stopping its node (~p)",[Dev_id,InternalError])};   
   
% The device was deleted from the Simulator database, but internal errors occured in
% stopping its node and deleting it from the Controller database (should NEVER happen)
err_to_code_msg({delete_dev_start_ctrdb_fail,Dev_id,InternalError,CtrDBError}) ->
 {500,io_lib:format("<SERVER ERROR> The device of \"dev_id\" ~w was deleted from the Simulator database, but internal errors occured " ++
                    "in stopping its node (~p) and deleting it from the Controller database (~p)",[Dev_id,InternalError,CtrDBError])};   
   
%% DEVCOMMANDS (PATCH /device)
%% -----------
% HTTP request body could not be interpreted in JSON format
err_to_code_msg(body_not_json) ->
 {415,"<ERROR> Request body could not be interpreted in JSON format"}; 

% The request body ("DevCommands") is not a list
err_to_code_msg({not_a_list,devcommands}) ->
 {400,"<ERROR> The request body could not be interpreted as a JSON list"};

% The request body ("DevCommands") is empty
err_to_code_msg({empty,devcommands}) ->
 {400,"<ERROR> The list of device commands is empty"};

% NOTE: The error handling of each individual device command is performed manually within the operation handler
% ----

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
   throw({device_not_started,Dev_id,InternalError});

  % The device was added into the Simulator database, but an internal error occured
  % starting its node AND in adding it into the Controller database (should NEVER happen)
  {ok,InternalSimError,CtrDBError} ->
   throw({add_dev_start_ctrdb_fail,Dev_id,InternalSimError,CtrDBError})
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
   throw({device_not_exists,Dev_id});
   
  % The device sublocation was updated in the Simulator but
  % not in the Controller database (should NEVER happen)
  {ok,CtrDBError} ->
   throw({ctrdb_update_dev_subloc_fail,Dev_id,CtrDBError})
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
   throw({device_node_not_stopped,Dev_id,InternalError});
   
  % The device was deleted from the Simulator database, but internal errors occured in
  % stopping its node and deleting it from the Controller database (should NEVER happen)
  {ok,InternalError,CtrDBError} ->
   throw({delete_dev_start_ctrdb_fail,Dev_id,InternalError,CtrDBError})
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
 
 % If it is, parse the received commands
 ParsedCommands = parse_devcommands(DevCommands,[]),
   
 io:format("ParsedCommands = ~p~n~n",[ParsedCommands]),
			     
 cowboy_req:reply(204,Req).
 
 
%% -------------------------------------------- 
get_devcommands(BinBody) ->
     
 % Interpret the binary body of the HTTP request as JSON
 % and attempt to cast it into a list via the JSONE library
 DevCommands = try jsone:decode(BinBody)
               catch
		   
		        % If the body of the HTTP request could
			    % not be interpred as JSON, throw an error
		        error:badarg ->
			     throw(body_not_json)
		       end,
 
 % Ensure that what was returned by
 % the JSONE library is indeed a list
 if
  is_list(DevCommands) =:= false ->
   
   % If it is not, throw an error
   throw({not_a_list,devcommands});
   
  true ->
  
   % If it is, ensure it to contain at least one element
   if
   
    % If it does, return the list of DevCommands
    length(DevCommands) > 0 ->
	 DevCommands;
	
    % Otherwise, throw an error	
	true ->
	 throw({empty,devcommands})
   end
 end.
 
 
%% -------------------------------------------- 
% Base case (return the list of ParsedCommands
parse_devcommands([],ParsedCommands) ->
 ParsedCommands;
 
% Recursive case (when a DevCommand was correctly interpreted as a map)  
parse_devcommands([DevCommand|NextDevCommand],ParsedCommands) when is_map(DevCommand) ->

 ParsedCommand =
 try
  
  % Attempt to retrieve the "Dev_id" key from the DevCommand map
  Dev_id = get_devcommand_dev_id(DevCommand),
  
  io:format("~nDev_id = ~w~n",[Dev_id]),
  
  try
   
   % Retrieve the DevComman actions' map
   Actions = get_devcommand_actions(DevCommand),
   
   io:format("Actions = ~p~n",[Actions]),
   
   % Ensure device "Dev_id" to belong to the location and to be currently paired
   % with the controller, also obtaining its Type and the PID of its 'ctr_devhandler'
   {Type,HandlerPID} = get_devcommand_devinfo(Dev_id), 
  
   io:format("Type = ~w, HandlerPID = ~p~n",[Type,HandlerPID]),
   
   % Prepare the configuration command record of the
   % appropriate #devtype by parsing the actions' map
   CfgCommand = parse_devcommand_actions(Type,Actions),
   
   io:format("CfgCommand = ~p~n",[CfgCommand]),
   
   % Return the parsed DevCommand
   #{dev_id => Dev_id, cfgcommand => CfgCommand, devhandler_pid => HandlerPID}
   
  catch
   {missing_param,actions} ->
    throw(#{dev_id => Dev_id, status => 400, errorReason => "The \"actions\" object is missing"});
   {not_a_map,actions} ->
    throw(#{dev_id => Dev_id, status => 400, errorReason => "The \"actions\" parameter could not be interpreted as a JSON object"});
   {empty,actions} ->
    throw(#{dev_id => Dev_id, status => 400, errorReason => "The \"actions\" object is empty"});  
   device_not_exists ->
    throw(#{dev_id => Dev_id, status => 404, errorReason => "A device with such \"dev_id\" does not exist"});
   device_offline ->
    throw(#{dev_id => Dev_id, status => 307, errorReason => "The device is currently offline"});
   {invalid_devtrait,Trait,InvalidValue} ->
    throw(#{dev_id => Dev_id, status => 400, errorReason => io_lib:format("Invalid value \"~p\" of trait \"~p\"",[InvalidValue,Trait])});
   {empty_cfgcmd,DevType} ->
    throw(#{dev_id => Dev_id, status => 00, errorReason => "The command would have no effect on the device according to its type (" ++ atom_to_list(DevType) ++ ")"});
	
   _:UnhandledErrorDev ->
    io:format("unhandled error: ~p (dev_id: ~p)~n",[UnhandledErrorDev,Dev_id]),
    throw(#{dev_id => Dev_id, status => 500, errorReason => io_lib:format("Unhandled server error: ~p",[UnhandledErrorDev])})
  end
  
 catch
  {missing_param,dev_id} ->
   #{dev_id => unknown, status => 400, errorReason => "Required parameter \"dev_id\" is missing"};
  {not_an_integer,dev_id} ->
   #{dev_id => invalid, status => 400, errorReason => "Parameter \"dev_id\" is not an integer"};
  {out_of_range,dev_id,InvalidDevId} ->
   io:format("is integer? ~w~n",[is_integer(InvalidDevId)]),
   io:format("is list? ~w~n",[is_list(InvalidDevId)]),
   #{dev_id => invalid, status => 400, errorReason => "Invalid value of parameter \"dev_id\" (" ++ integer_to_list(InvalidDevId) ++ ")"};
  ErrorMap when is_map(ErrorMap) ->
   ErrorMap;
  _:UnhandledError ->
   io:format("unhandled error: ~p~n",[UnhandledError]),
   throw(#{dev_id => unknown, status => 500, errorReason => io_lib:format("Unhandled server error: ~p",[UnhandledError])})
 end,
 
 % Parse the next DevCommand
 parse_devcommands(NextDevCommand,ParsedCommands ++ [ParsedCommand]);
 
% Recursive case where the DevCommand was not correctly interpreted as a map
parse_devcommands([_|NextDevCommand],ParsedCommands) ->
 parse_devcommands(NextDevCommand,ParsedCommands ++ [#{dev_id => unknown, status => 400, errorReason => "The device command could not be interpreted as a JSON object"}]).


%% --------------------------------------------
get_devcommand_dev_id(DevCommand) -> 

 % Attempt to retrieve the "dev_id" key from the DevCommand map
 Dev_id = try maps:get(<<"dev_id">>,DevCommand)
          catch
		
		   % If the "dev_id" was not found, mark the
		   % DevCommand as invalid by throwing an error
 	       error:{badkey,<<"dev_id">>} ->
		    throw({missing_param,dev_id})
		  end,

 % Ensure the "Dev_id" to be an integer
 if
  
  % If it is, ensure it to be >0
  is_integer(Dev_id) ->
   if
    
	% If it is, return the Dev_id
	Dev_id > 0 ->
     Dev_id;
	 
	% If it is not, throw an error
	true ->
	 throw({out_of_range,dev_id,Dev_id})
   end;
   
  % If it is not an integer, throw an error
  true ->
   throw({not_an_integer,dev_id})
 end.
   
   
%% --------------------------------------------  
get_devcommand_actions(DevCommand) ->
  
 % Attempt to retrieve the "actions" map from the DevCommand
 Actions = try maps:get(<<"actions">>,DevCommand)
           catch
	 		 
	        % If the "action" map was not found
	        % in the DevCommand, throw an error
	        error:{badkey,<<"actions">>} ->
		     throw({missing_param,actions})
	       end,

 % Ensure "Actions" to be indeed a map
 if
  is_map(Actions) =:= true ->
  
   % If it is, ensure it to contain at least one element
   if
    map_size(Actions) > 0 ->
   
     % If it does, return the map
	 Actions;
	 
	true ->
	
	 % Otherwise, throw an error
	 throw({empty,actions})
   end;
  
  true -> 
  
   % If it is not a map, throw an error
   throw({not_a_map,actions})
 end.  
  
  
  
  
  
  
%% --------------------------------------------
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
	
	% If the device is currently offline, throw an error
	'-' ->
	 throw(device_offline);
	
	% Otherwise if the device is currently
	% paired, return its Type and HandlerPID
	HandlerPID when is_pid(HandlerPID) ->
	 {CtrDevRecord#ctr_device.type,HandlerPID}
   end
 end.
 

 


parse_devcommand_actions(fan,Actions) ->
 
 % OnOff trait
 OnOff = parse_devcommand_trait(onoff,Actions),
 
 % FanSpeed trait
 FanSpeed = parse_devcommand_trait(fanspeed,Actions),
 
 % Ensure that the resulting configuration command could have
 % an effect, i.e. it's not composed only by '$keep' parameters
 ok = check_cfgcmd_empty([OnOff,FanSpeed],fan),

 % Return the configuration command of type "fan"
 #fancfg{onoff = OnOff, fanspeed = FanSpeed}.
 
 



check_cfgcmd_empty([],Type) ->

 % Throw an error signaling that the resulting
 % configuration command would not have any effect
 throw({empty_cfgcmd,Type}); 
 
check_cfgcmd_empty(['$keep'|NextTrait],Type) ->
 
 % If the trait is '$keep', search the next one
 check_cfgcmd_empty(NextTrait,Type);
 
check_cfgcmd_empty(NoKeepTrait,_) ->
 
 % If the trait is not '$keep', return ok
 ok.
 
 
 
 
 
parse_devcommand_trait(onoff,DevCommand=#{<<"onOff">> := OnOff}) when OnOff =:= <<"on">> orelse OnOff =:= <<"off">> ->
 binary_to_atom(OnOff);
parse_devcommand_trait(onoff,DevCommand=#{<<"onOff">> := InvalidOnOff}) ->
 throw({invalid_devtrait,onoff,InvalidOnOff});
 
parse_devcommand_trait(fanspeed,DevCommand=#{<<"setFanSpeed">> := FanSpeed}) when is_integer(FanSpeed), FanSpeed > 0, FanSpeed =< 100 ->
 FanSpeed;
parse_devcommand_trait(fanspeed,DevCommand=#{<<"setFanSpeed">> := InvalidFanSpeed}) ->
 throw({invalid_devtrait,fanspeed,InvalidFanSpeed});
 
parse_devcommand_trait(_,_) ->
 '$keep'.
 
 






%build_devhandler_command([],DevHandlerCommand) ->
% DevHandlerCommand;
%build_devhandler_command([Command=#{<<"command">> := <<"actions.devices.commands.onoff">>,<<"value">> := <<"on">>}|NextCommand],DevHandlerCommand) -> 
% build_devhandler_command(NextCommand,DevHandlerCommand#{onoff => on});
%build_devhandler_command([Command=#{<<"command">> := <<"actions.devices.commands.onoff">>,<<"value">> := <<"off">>}|NextCommand],DevHandlerCommand) -> 
% build_devhandler_command(NextCommand,DevHandlerCommand#{onoff => off});
%build_devhandler_command(_,DevHandlerCommand) -> 
% io:format("build_devhandler_command error~n"),
% DevHandlerCommand.
 



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