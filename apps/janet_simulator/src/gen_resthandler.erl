%% This module represents a custom behaviour for implementing REST handlers based on the Cowboy HTTP server %%

-module(gen_resthandler).

%% -------------------------------- OTP INTEGRATION PUBLIC FUNCTIONS -------------------------------- %%
-export([init/3,system_continue/3,system_terminate/4]).

%% ----------------------------------- GEN_RESTHANDLER PUBLIC API ----------------------------------- %%
-export([start_link/2,handle_req/3,get_check_method/2,get_check_int_binding/3,jsone_term_to_list/2]).


%% This record describes an error that may occur in the handling of an HTTP request
-record(reqerror,    
        {
		 handlername,  % The name of the resource or operation handler where the error was raised
		 error,        % The error that was raised, which may consist in either
		               % an atom 'errorType' or a tuple {'errorType',errorInfo}
		 errormod,     % The name of the module where to retrieve the HTTP code and the plain text message
		               % associated with the error (which in the handling logic is set to 'gen_resthandler'
					   % if the error occured during the resource handler or in retrieving the body 
					   % parameters, or the custom handler if the error occured during a operation handler)
		 req,          % The map associated to the HTTP request used by Cowboy when the error occured
		 binbody       % The body of the HTTP request as a binary (if any)
		}).

%%====================================================================================================================================
%%                                              CALLBACK FUNCTIONS INTERFACES DEFINITION                                                       
%%====================================================================================================================================

%% DESCRIPTION:     gen_resthandler callback module initializer
%%
%% CALLED WHEN:     The gen_resthandler is initialized (init(Parent,Module,Args))
%%
%% ARGUMENTS:       - Args: The list of arguments received via the start_link() function
%%
%% ALLOWED RETURNS: - {ok,RESTPort,RemoteHost,ListenerName,Paths}
%%                      - RESTPort     -> The port to be used by the Cowboy listener
%%                      - ListenerName -> The name (atom) by which register
%%                                        the module as a cowboy listener
%%                      - Paths        -> The list of resource handlers paths
%%                                        implemented by the callback module
%%
-callback init_handler(Args :: term()) ->
 {ok,RESTPort::integer(),ListenerName::atom(),Paths::list()}.

 
%% DESCRIPTION:     Cowboy root handler
%%
%% CALLED WHEN:     - (called by Cowboy)
%%
%% ARGUMENTS:       - Req:            The HTTP Request map as returned by Cowboy
%%                  - ResHandlerName: The name of the resource handler
%%                                    associated with the HTTP request
%%
%% ALLOWED RETURNS: - {ok,ReplyReq,ResHandlerName}
%%                      - ReplyReq -> The HTTP Response map to be returned to the client
%%                                    (returned by gen_resthandler:handle_req/3)
%%                      - ResHandlerName -> The name of the resource handler associated with
%%                                          the HTTP request (the same of the argument)
%%
-callback init(Req::map(),ResHandlerName::any()) ->
 {ok,ReplyReq::map(),ResHandlerName::any()}.
  

%% DESCRIPTION:     Operation error handlers
%%
%% CALLED WHEN:     An exception is thrown from a custom operation handler
%%
%% ARGUMENTS:       - Error: The error that was thrown by the operation handler
%%
%% ALLOWED RETURNS: - {HTTPErrorCode,PlainTextError}
%%                      - HTTPErrorCode -> The HTTP error code to be returned to the client
%%                      - ErrorDescr    -> A description in plain text of the error that has
%%                                         occured to be returned in the body of the HTTP response
%%
-callback err_to_code_msg(Error::term()) ->
  {HTTPErrorCode::integer(),ErrorDescr::list()}.


%% DESCRIPTION:     Host OS Port allocation conflict callback
%%
%% CALLED WHEN:     The "RESTPort" passed by the callback module in the
%%                  "init_handler" callback function is not available in the host OS
%%
%% ARGUMENTS:       - RESTPort: The port that is not available in the host OS (int > 0)
%%
%% ALLOWED RETURNS: - {stop,Reason} -> The gen_resthandler will return the error Reason to its
%%                                     parent supervisor and then exit via the exit(Reason) BIF
%%	                - ignore        -> The gen_resthandler will return 'ignore' to its parent
%%                                     supervisor and then exit via the exit(normal) BIF 
%%
-callback os_port_conflict(RESTPort::integer()) ->
 {stop,Reason::term()} | ignore. 
  
  
%%====================================================================================================================================
%%                                                    GEN_RESTHANDLER PUBLIC API                                                       
%%====================================================================================================================================

%% DESCRIPTION:  Synchronously start the gen_resthandler linking it
%%               to its parent supervisor in an OTP supervision tree
%%
%% ARGUMENTS:    - Module: The name of the gen_resthandler callback module
%%               - Args:   The list of arguments to be passed to the callback
%%                         module init_handler(Args) callback function
%%
%% RETURNS:      The "Ret" argument of the "proc_lib:init_ack(Parent,Ret)"
%%               function called by the OTP behaviour initializer (init())
%%               once the initialization is complete:
%%                 - {ok,ProcessPID} -> Process initialization successful
%%                 - {error,Reason}  -> Error in initializing the process
%%
start_link(Module,Args) ->

 % Call via the proc_lib the OTP behaviour initializer
 % (init()), also passing the PID of the parent supervisor
 proc_lib:start_link(?MODULE,init,[self(),Module,Args]).
 

%% DESCRIPTION:  Handles an HTTP request received by the callback
%%               module via the init(Req,ResHandlerName) function
%%
%% ARGUMENTS:    - CallbackModule: The name of the callback module (atom)
%%               - Req:            The HTTP request map returned by Cowboy
%%               - ResHandlerName: The name of the resource handler
%%                                 associated with the request
%%
%% RETURNS:      The "ReplyReq" map specifiying the HTTP
%%               response to be replied to the client
%%
handle_req(CallbackModule,ResHandlerName,Req) ->

 % Attempt to handle the HTTP request so to obtain
 % the HTTP response to be returned to client  
 try handle_req_try(CallbackModule,ResHandlerName,Req)
 catch
 
  % If an error was raised in the handling of the HTTP request, determine the
  % HTTP error response to be returned to the client depending on whether the
  % error was handled (i.e. it is structured as a 'reqerror' record) or not
  
  % Handled error
  _:HandlerError when is_record(HandlerError,reqerror) -> 
   get_error_response(HandlerError);

  % Unhandled error (should NOT happen)
  _:UnexpectedError ->
   get_unexpected_error_response(UnexpectedError,CallbackModule,Req)
 end.


%% DESCRIPTION:  Called by a resource handler to ensure the HTTP method of
%%               an HTTP request map belongs to its set of allowed methods
%%
%% ARGUMENTS:    - Req:             The HTTP request map
%%               - Allowed_Methods: The list of allowed HTTP methods
%%
%% RETURNS:      - ReqMethodBin -> The request HTTP method as a binary
%%
%% THROWS:       - {unallowed_method,ReqMethodBin} -> Unallowed method for
%%                                                    the resource handler
%%
get_check_method(Req,Allowed_Methods) ->
 
 % Retrieve the HTTP Request method as a binary
 ReqMethodBin = cowboy_req:method(Req),
 
 % Ensure the HTTP Request method to be included in
 % the list of methods allowed by the resource handler
 case lists:member(ReqMethodBin,Allowed_Methods) of

  % If it is, return the method 
  true ->
   ReqMethodBin;
   
  % Otherwise, throw an error
  false ->
   throw({unallowed_method,ReqMethodBin})
 end.


%% DESCRIPTION:  Extracts from an HTTP request map "Req" a path binding parameter of name "ParamName"
%%               and ensures it to be an integer of value greater or equal than a given MinValue
%%
%% ARGUMENTS:    - Req:       The HTTP request map
%%               - ParamName: The name of the path binding parameter to be extracted (atom)
%%               - MinValue:  The minimum allowed value for the extracted parameter 
%%
%% RETURNS:      - IntParam -> The path binding parameter converted to an integer
%%
%% THROWS:       - {not_an_integer,ParamName,BinParam} -> The path binding parameter is not an integer
%%               - {out_of_range,ParamName,IntParam}   -> The path binding parameter is lower than MinValue
%%
get_check_int_binding(Req,ParamName,MinValue) ->
 
 % Retrieve the path binding parameter of name "ParamName" as a binary
 BinParam = cowboy_req:binding(ParamName,Req),
 
 % Attempt to cast the path binding parameter to an integer
 IntParam = try binary_to_integer(BinParam)
 catch
 
  % If the binary parameter could not be
  % converted to an integer, throw an error
  error:badarg ->
   throw({not_an_integer,ParamName,BinParam})
 end,
 
 % Ensure the integer path binding parameter
 % to be greater or equal than "MinValue"
 if
 
  % If it is, return the path binding parameter
  IntParam >= MinValue ->
   IntParam;
   
  % If it is not, throw an error
  true ->
   throw({out_of_range,ParamName,IntParam})
 end.
 

%% DESCRIPTION:  Converts a term() returned by the jsone:decode(Json_Object) function to list 
%%
%% ARGUMENTS:    - Param:     The term() to be converted to list
%%               - ParamName: The atom() name associated with the term()
%%
%% RETURNS:      - ParamList -> The term() converted to stringThe string converted to atom 
%% 
%% THROWS:       - {unknown_jsone_cast,ParamName,Param} -> The parameter was cast to an unknown
%%                                                         term() type by the JSONE library
%%
jsone_term_to_list(Param,_) when is_binary(Param) ->
 binary_to_list(Param);
jsone_term_to_list(Param,_) when is_integer(Param) ->
 integer_to_list(Param);
jsone_term_to_list(Param,_) when is_float(Param) ->
 float_to_list(Param);
jsone_term_to_list(Param,_) when is_tuple(Param) ->
 tuple_to_list(Param);
jsone_term_to_list(Param,_) when is_atom(Param) ->
 atom_to_list(Param);
jsone_term_to_list(Param,_) when is_list(Param) ->
 Param;

% If the parameter was mapped by the JSONE library to an
% unknown Erlang type (which should NOT happen), throw an error
jsone_term_to_list(_,ParamName)  ->
 throw({unknown_jsone_cast,ParamName}).
 

%%====================================================================================================================================
%%                                                    PRIVATE HELPER FUNCTIONS
%%==================================================================================================================================== 

%% DESCRIPTION:  Attempts to handle an HTTP request
%%               passed by the handle_req() function
%%
%% ARGUMENTS:    - CallbackModule: The name of the callback module (atom)
%%               - Req:            The HTTP request map returned by Cowboy
%%               - ResHandlerName: The name of the resource handler
%%                                 associated with the request
%%
%% RETURNS:      The "ReplyReq" map specifiying the HTTP
%%               response to be replied to the client
%%
%% THROWS:       - A 'reqerror' record describing an error
%%                 occured in the handling of the HTTP request
%%
handle_req_try(CallbackModule,ResHandlerName,Req) ->

 %% ----------------------------- Resource Handler Call ----------------------------- %%         

 % Pass the HTTP request map to its associated resource handler, obtaining:
 %
 % - The name of the operation handler associated with the request
 % - The list of path binding parameters to be passed to the operation handler
 % - The list of expected parameters to be retrieved from the body of the HTTP request
 %
 {OpHandlerName,PathParams,ExpBodyParams} =
 try CallbackModule:ResHandlerName(Req)
 catch
  _:ResHandlerError ->
  
   % If an error was raised by the resource handler, initialize the associated
   % 'reqerror' record and propagate it to the root handler "init()" callback
   throw(#reqerror{handlername = ResHandlerName, error = ResHandlerError, errormod = ?MODULE, req = Req, binbody = <<"<not_read>">>})
 end,

 
 %% --------------------------- Body Parameters Retrieval --------------------------- %%

 % Depending on whether there are expected parameters to be
 % retrieved from the body of the HTTP request, determine:
 %
 % 1) The complete list of arguments to be passed to the operation handler
 % 2) If it must be read, the HTTP request body as a binary
 % 3) If it must be read, the updated HTTP request map
 %
 {OpHandlerArguments,BinBody,ReqBody} = 
 case ExpBodyParams of
  
  [] ->
    
   % If no parameters must be read from the body:
   %
   % 1- The "OpHandlerArguments" coincide with the "PathParams"
   % 2- The "BinBody" is to be left to its default <<"<not_read>">> value
   % 3- The "ReqRead" coincides with the initial "Req"
   %
   {PathParams,<<"<not_read>">>,Req};
	
  custom ->
  
   % If the operation handler is to be given the
   % binary body of the HTTP request, retrieve it
   {ok,ReadBinBody,ReadReq} = cowboy_req:read_body(Req),
	
   % Return the required tuple
   {PathParams ++ [ReadBinBody],ReadBinBody,ReadReq};

  _ ->

   % If there are parameters to be read from the body:
   %
   % 1- The "OpHandlerArguments" is given by the concatenation of the "PathParams" with the "ExpectedBodyParams"
   % 2- The "BinBody" is to be set once it has been read
   % 3- The "ReqRead" is to be set to the modified map returned by Cowboy once the body has been read
	
   % Retrieve the HTTP request body as a binary
   {ok,ReadBinBody,ReadReq} = cowboy_req:read_body(Req),
	
   % Attempt to retrieve the list of expected parameters
   % from the binary body assuming it is JSON-encoded
   ExpectedBodyParams =
   try get_expected_body_params(ReadBinBody,ExpBodyParams)
   catch 
	_:BodyParamsError ->
	 
	 % If an error was raised in retrieving the list of expected parameter, initialize the
	 % associated 'reqerror' record and propagate it to the root handler "init()" callback
	 %
	 % NOTE: Even if it has not been called yet, the 'handlername' field is set to the
	 %       "OpHandlerName" for a more informative logging in the get_error_response()
	 %
     throw(#reqerror{handlername = OpHandlerName, error = BodyParamsError, errormod = ?MODULE, req = ReadReq, binbody = ReadBinBody})
   end,
	
   % Return the required tuple
   {PathParams ++ ExpectedBodyParams,ReadBinBody,ReadReq}
 end,
  
 %% ----------------------------- Operation Handler Call ----------------------------- %% 
  
 % Call the operation handler with the defined list of parameters, which
 % if successful returns the HTTP response to be replied to the client
 try CallbackModule:OpHandlerName(Req,OpHandlerArguments)
 catch 
  _:OpHandlerError ->
	 
   % If an error was raised by the operation handler, initialize its associated
   % the 'reqerror' record and propagate it to the root handler "init()" callback
   throw(#reqerror{handlername = OpHandlerName, error = OpHandlerError, errormod = CallbackModule, req = ReqBody, binbody = BinBody})
 end.
  

%% DESCRIPTION: Extracts a list of expected parameters from the binary body of an HTTP request  
%%
%% ARGUMENTS:    - BodyBin:       The binary body of the HTTP request
%%               - ExpBodyParams: The list of parameters to be extracted from the
%%                                body specified in the following format:
%%                                 - List/String parameters: {ParamName,'list','required'/'optional'}
%%                                 - Integer parameters:     {ParamName,'integer',MinValue}
%%
%% RETURNS:      - BodyParams -> The list of extracted body parameters
%%
%% THROWS:       - body_not_json                        -> The body could not be
%%                                                         interpreted in JSON format
%%               - {missing_param,ParamName}            -> Required parameter "ParamName
%%                                                         could not be found in the body
%%               - {not_an_integer,ParamName,Param}     -> Expected integer parameter
%%                                                         "ParamName" is not an integer
%%               - {out_of_range,ParamName,Param}       -> Integer parameter "ParamName"
%%                                                         is lower than its MinValue
%%               - {unknown_jsone_cast,ParamName,Param} -> List parameter "ParamName" was cast to an
%%                                                         unknown Erlang type by the JSONE library
%%
get_expected_body_params(BinBody,ExpBodyParams) ->

 % Interpret the binary body of the HTTP request as JSON
 % and attempt to cast it into a map via the JSONE library
 BodyMap = try jsone:decode(BinBody)
           catch
		   
		    % If the body of the HTTP request could
			% not be interpred as JSON, throw an error
		    error:badarg ->
			 throw(body_not_json)
		   end,

 % Ensure that what was returned by the JSONE library is indeed a map
 if
  is_map(BodyMap) =:= true ->  
  
   % If it is, attempt to retrieve the list
   % of expected parameters from the body map
   extract_expected_bodymap_params(ExpBodyParams,[],BodyMap);
   
  true ->
  
   % If it is not, an error occured in processing
   % the body as JSON, and so throw an error
   throw(body_not_json)
 end.
 
 
%% Extracts a list of expected parameters from the binary body of an HTTP
%% request (get_expected_body_params(BinBody,ExpBodyParams) helper function)

% Base case
extract_expected_bodymap_params([],BodyParams,_) ->
 
 % Return the list of expected parameters
 % extracted from the body of the HTTP request
 BodyParams;

% Recursive case
extract_expected_bodymap_params([{ParamName,ParamType,ParamConstraint}|NextParam],BodyParams,BodyMap) ->

 % Convert the name of the expected parameter to a binary
 ParamNameBin = atom_to_binary(ParamName),
 
 % Attempt to retrieve the parameter from the body map
 Param = try maps:get(ParamNameBin,BodyMap)
         catch
		 
		  % If the parameter was not found in the body map
		  error:{badkey,ParamNameBin} ->
		   if
		   
		    % If the parameter is a list and its optional,
			% use an empty binary list as its value
			ParamType =:= list andalso ParamConstraint =:= optional ->
	         <<"">>;
			
			% In all other cases the required parameter
			% is missing, and so throw an error
			true ->
			 throw({missing_param,ParamName})
		   end
		 end,
 		 
 % Parse the extracted parameter depending on its expected type
 BodyParam = case ParamType of
              list ->
			  
			   % If the parameter is expected to be a list,
			   % ensure it to be depending on the type on
			   % which it was mapped by the JSONE library
               jsone_term_to_list(Param,ParamName);
		  
		      integer ->
		        
			   % If the parameter is expected to be an integer,
			   % ensure it to be and to be >= ParamConstraint
			   check_int_bodyparam(Param,ParamConstraint,ParamName)
			 end,
		  
 % Append the parsed BodyParam in the list of
 % BodyParams and extract the next expected parameter
 extract_expected_bodymap_params(NextParam,BodyParams ++ [BodyParam],BodyMap).
		  

%% Ensures a body parameter to be an integer and >= MinValue (get_expected_body_params(BinBody,ExpBodyParams) ->
%% extract_expected_bodymap_params([{ParamName,ParamType,ParamConstraint}|NextParam],BodyParams,BodyMap) helper function)

% Parameter is an integer and >= MinValue -> Return the parameter
check_int_bodyparam(Param,MinValue,_) when is_integer(Param), Param >= MinValue ->
 Param;

% Parameter is an integer but < MinValue -> throw an error
check_int_bodyparam(Param,_,ParamName) when is_integer(Param) ->
 throw({out_of_range,ParamName,Param});
 
% Parameter is not an integer -> throw an error
check_int_bodyparam(Param,_,ParamName) ->
 throw({not_an_integer,ParamName,Param}).
 
 
%% DESCRIPTION:  Parses an error occured during the handling of an HTTP request described via a
%%               'reqerror' record and returns the HTTP error response to be replied to the client
%%
%% ARGUMENTS:    - ReqError: The 'reqerror' record describing the error that
%%                           has occured in the handling of the HTTP request
%%
%% RETURNS:      - ReplyReq -> The error HTTP response to be replied to the client 
%%
get_error_response(ReqError) when is_record(ReqError,reqerror) ->
 
 % If any, convert the body of the HTTP request
 % which caused the error from binary to list
 Body = binary_to_list(ReqError#reqerror.binbody),

 % Retrieve the HTTP code and a description in plain text of the error that has occured
 % by calling the 'err_to_code_msg()' function in module "ReqError#reqerror.errmod"
 {ErrorCode,ErrorMsg} = case ReqError#reqerror.errormod of
 
  % gen_resthandler (resource handler or body parameters retrieval error)
  ?MODULE ->
   err_to_code_msg(ReqError#reqerror.error);
   
  % Callback module (operation handler error)
  CallbackModule ->
   CallbackModule:err_to_code_msg(ReqError#reqerror.error)
 end,
   
 % Log the error
 io:format("[~s]: ~s (ReqBody = ~s, HTTPCode = ~w) ~n",[ReqError#reqerror.handlername,ErrorMsg,Body,ErrorCode]),

 % Return the HTTP response to be replied to the client
 cowboy_req:reply(
	              ErrorCode,                                  % HTTP Response Code
	              #{<<"content-type">> => <<"text/plain">>},  % "Content Type" header
			      ErrorMsg,                                   % Response Body
				  ReqError#reqerror.req                      % Associated HTTP Request
				 ).


%% DESCRIPTION:  Parses an unexpected error occured during the handling of an
%%               HTTP request (i.e. one not described via a 'reqerror' record)
%%               and returns the HTTP error response to be replied to the client
%%
%% ARGUMENTS:    - UnexpectedError: The unexpected error that has occured
%%                                  during the handling of the HTTP request
%%               - CallbackModule:  The name of the callback module (atom)
%%               - Req:             The HTTP request map which caused the error
%%
%% RETURNS:      - ReplyReq -> The error HTTP response to be replied to the client 
%%
get_unexpected_error_response(UnexpectedError,CallbackModule,Req) ->

 % Attempt to convert the unexpected error to list (string)
 UnexpectedErrorStr = try io_lib:format("~s",[UnexpectedError])
                      catch
					   error:badarg ->
					    
						% If the error could not be converted
						% to list, keep it as it is
                        UnexpectedError
					  end,
					  
 % Log the unexpected error
 io:format("[~p]: <UNEXPECTED SERVER ERROR>: ~p (Req = ~p)~n",[CallbackModule,UnexpectedErrorStr,Req]),
   
 % Return the error HTTP response to be replied to the client
 cowboy_req:reply(
	              500,                                                  % HTTP Response Code
	              #{<<"content-type">> => <<"text/plain">>},            % "Content Type" header
		          "<UNEXPECTED SERVER ERROR>: " ++ UnexpectedErrorStr,  % Response Body
			      Req                                                   % Associated HTTP Request
			     ).


%% DESCRIPTION:  Returns the HTTP code and a description in plain text of an
%%               error occured in processing the contents of an HTTP request
%% 
%% ARGUMENTS:    - ErrorDescr: A description of the error that occured in the
%%                             processing of the contents of the HTTP request
%%
%% RETURNS:      - {HTTPErrorCode,ErrorDescr} -> The HTTP code and a description in plain
%%                                               text of the error that has occured
%%

% Unallowed HTTP method in resource handler 
err_to_code_msg({unallowed_method,ReqMethodBin}) ->
 {405,io_lib:format("<ERROR> Method \"~s\" is not allowed in the specified path",[ReqMethodBin])};
 
% Required integer parameter is not an integer
err_to_code_msg({not_an_integer,ParamName,Param}) ->
 {400,io_lib:format("<ERROR> Parameter \"~s\" is not an integer (~p)",[ParamName,binary_to_list(Param)])};
 
% Required integer parameter of invalid value
err_to_code_msg({out_of_range,ParamName,Param}) ->
 {400,io_lib:format("<ERROR> Invalid value of parameter \"~s\" (~w)",[ParamName,Param])};
 
% HTTP request body could not be interpreted in JSON format
err_to_code_msg(body_not_json) ->
 {415,"<ERROR> Request body could not be interpreted in JSON format"}; 
 
% Required HTTP body parameter is missing
err_to_code_msg({missing_param,ParamName}) ->
 {400,io_lib:format("<ERROR> Required parameter \"~s\" is missing",[ParamName])}; 

% A body list parameter was cast to an unknown Erlang type by the JSONE library
err_to_code_msg({unknown_jsone_cast,ParamName}) ->
 {500,io_lib:format("<SERVER ERROR> Unknown JSONE type for parameter \"~s\"",[ParamName])};

% <UNKNOWN ERROR> 
err_to_code_msg(UnknownError) ->
 {500,io_lib:format("<UNKNOWN SERVER ERROR> Unknown error: ~p",[UnknownError])}.
 
 
%%====================================================================================================================================
%%                                                     OTP INTEGRATION FUNCTIONS                                                      
%%==================================================================================================================================== 
 
%% DESCRIPTION:  OTP Behaviour initializer (called by the parent supervisor via the proc_lib:start_link() function)
%%
%% ARGUMENTS:    - Parent: The PID of the parent supervisor
%%               - Module: The name of the callback module from which the behaviour was started
%%               - Args:   An optional list of arguments to be passed to the callback module init_handler() callback function
%%
%% RETURNS:      - ok                         -> JANET Simulator succesfully started
%%               - {error,already_running}    -> The janet_simulator application is already running on the node
%%               - {error,mnesia_init_failed} -> The Janet Simulator cannot be started due to an invalid Mnesia configuration
%%               - {error,Reason}             -> Internal error in starting the application
%%               - {error,badarg}             -> Invalid arguments
%%
init(Parent,Module,Args) ->
    
 try

  % Trap exit signals so to allow cleanup operations when terminating ({'EXIT',Parent,Reason} in main receive loop)
  process_flag(trap_exit,true),
 
  % Set a default debug structure for the
  % process (sys debug facilities support)
  DbgStruct = sys:debug_options([]),
 
  % Call the init_handler() callback function with the Args passed by the
  % start_link() function, which returns the following information as a tuple:
  %
  % - RESTPort:     The port to be used by the Cowboy listener
  % - ListenerName: The name (atom) by which register the
  %                 callback module as a cowboy listener
  % - Paths:        The list of resource handlers paths
  %                 implemented by the callback module
  % 
  {ok,RESTPort,ListenerName,Paths} = Module:init_handler(Args),
 
  % Ensure the port to be used by the Cowboy listener to be available
  case utils:is_localhost_port_available(RESTPort) of
   
   false ->
	  
    %% -------------------------------- Port Allocation Conflict -------------------------------- %
	  
    % It it is not, determine the gen_resthandler shutdown
    % strategy via the 'os_port_conflict' callback function
    case Module:os_port_conflict(RESTPort) of
	 ignore ->
	 
	  % Inform the parent supervisor that
	  % this child process should be ignored
	  proc_lib:init_ack(Parent,ignore),
	  
	  % Exit with reason 'normal' so to prevent the parent
	  % supervisor from reattempting to restart the gen_resthandler
	  exit(normal);
	  
	 {stop,Reason} ->
	 
	  % Inform the parent supervisor that this child process
	  % is stopping for an error of the given Reason
	  proc_lib:init_ack(Parent,{error,Reason}),
	    
	  % Exit with the same Reason
	  exit(Reason)
	end;
 
   true -> 
	
    %% -------------------------------------- Cowboy Setup -------------------------------------- %	
	 
	% If the port is available, start the Cowboy application and all its dependencies
    {ok, _StartedApps} = application:ensure_all_started(cowboy),
   
	% Define the Cowboy routes by associating connections coming from
	% any host with the list of paths returned by the callback module
	Routes = [{'_',Paths}],
    
	% Compile the Cowboy routes
    CompiledRoutes = cowboy_router:compile(Routes),
 
    % Attempt to start the Cowboy Listener
    {ok,_} = cowboy:start_clear(ListenerName,                             % Listener Name
                                [{port, RESTPort}],                       % Listener Port
		     		            #{
								  env => #{dispatch => CompiledRoutes},  % Listener Routes
								  request_timeout => infinity            % Connection persistence
	   						     }
							   ),

    % Inform the parent process that the
	% synchronous initialization is complete	
	proc_lib:init_ack(Parent,{ok,self()}),
	
    % Enter the 'gen_resthandler' main loop
    loop(Parent,DbgStruct,ListenerName)
	  
  end

 catch
  
  % Unhandled error in the gen_resthandler initialization
  error:UnhandledError ->
   
    % Log the error
    io:format("[gen_resthandler]: <FATAL> Unhandled error: ~p",[UnhandledError]),
	
	% Inform the parent supervisor that this child
	% process is stopping for an error of of such reason
	proc_lib:init_ack(Parent,{error,UnhandledError}),
   
	% Exit with the same reason as the unhandled error
    exit(UnhandledError)  
	
 end.


%% DESCRIPTION:  Called by the 'sys' module after parsing a message received via the
%%               "sys:handle_system_message()" if the process is allowed to continue
%%
%% ARGUMENTS:    - Parent:       The PID of the parent supervisor
%%               - Dbg:          The debug structure for the 'sys' debug facilities
%%               - ListenerName: The name (atom) by which the cowboy listener was registered
%%
%% RETURNS:      - (return to the main process loop)
%%
system_continue(Parent,Dbg,ListenerName) ->
 loop(Parent,Dbg,ListenerName).
 
 
%% DESCRIPTION:  Called by the 'sys' module after parsing a message received via
%%               the "sys:handle_system_message()" if the process should terminate
%%
%% ARGUMENTS:    - Reason:       The reason by which the process should terminate
%%               - Parent:       The PID of the parent supervisor
%%               - Dbg:          The debug structure for the 'sys' debug facilities
%%               - ListenerName: The name (atom) by which the cowboy listener was registered
%%
%% RETURNS:      - (return to the main process loop)
%%
system_terminate(Reason,_,_,_) ->
 exit(Reason).
 
 
%% DESCRIPTION:  OTP Process main loop
%%
%% ARGUMENTS:    - Parent:       The PID of the parent supervisor
%%               - Dbg:          The debug structure for the 'sys' debug facilities
%%               - ListenerName: The name (atom) by which the cowboy listener was registered
%%
%% RETURNS:      - (can only exit with the same Reason of an exit
%%                  signal received from its parent supervisor)
%%
loop(Parent,Dbg,ListenerName) ->
 receive
  {system,From,Request} ->
  
   % Pass any received system messages to the 'sys' module
   sys:handle_system_msg(Request,From,Parent,?MODULE,Dbg,ListenerName);
		
  {'EXIT',Parent,Reason} ->
  
   % When an exit signal is received from the parent supervisor,
   % stop the cowboy listener associated with the callback module
   ok = cowboy:stop_listener(ListenerName),
   
   % Exit with the same Reason of the received exit signal
   exit(Reason)
 end.