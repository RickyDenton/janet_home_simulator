%% This module offers a set of utility functions for the REST manager of the JANET Simulator and JANET Controller applications %%

-module(rest_utils).
-export([handle_req/3,get_check_method/2,get_check_int_binding/3,get_error_response/1,get_unexpected_error_response/2]).

-include("reqerror.hrl").  % REST error handler record


handle_req(HandlerMod,ResHandlerName,Req) ->

 %% ----------------------------- Resource Handler Call ----------------------------- %%         
 
 % Pass the HTTP request map to its associated resource handler, obtaining:
 %
 % - The name of the operation handler associated with the request
 % - The list of path binding parameters to be passed to the operation handler
 % - The list of expected parameters to be retrieved from the body of the HTTP request
 %
 {OpHandlerName,PathParams,ExpBodyParams} =
 try HandlerMod:ResHandlerName(Req)
 catch
  _:ResHandlerError ->
   
   % If an error was raised, initialize its associated 'reqerror'
   % record and propagate it to the root handler "init()" callback
   throw(#reqerror{handler = ResHandlerName, error = ResHandlerError, req = Req})
 end,

 % Depending on whether there are expected parameters
 % to be retrieved from the body of the HTTP request
 case ExpBodyParams of
   
  %% ------------------ Operation Handler Call (no body parameters) ------------------ %%  
  [] -> 
  
   % If there are not, call the operation handler with the list of
   % path binding parameters as returned by the resource handler
   try HandlerMod:OpHandlerName(Req,PathParams)
   catch 
	_:OpHandlerError ->
	 
	 % If an error was raised, initialize its associated 'reqerror'
     % record and propagate it to the root handler "init()" callback
     throw(#reqerror{handler = OpHandlerName, error = OpHandlerError, req = Req})
   end;
	
  %% ----------------- Operation Handler Call (with body parameters) ----------------- %%   
  _ ->
   
   % Otherwise, if there are parameters to be retrieved
   % from the body of the HTTP request, read it as a binary
   {ok,BinBody,Req1} = cowboy_req:read_body(Req),
 
   try
	  
    % Attempt to retrieve the list of expected parameters
	% from the binary body assuming it is JSON-encoded
	ExpectedBodyParams = get_expected_body_params(BinBody,ExpBodyParams),
	
	% Call the operation handler by concatenating the list of
	% path bindings with the list of expected body parameters
	HandlerMod:OpHandlerName(Req,PathParams ++ ExpectedBodyParams)
   catch 
	_:OpHandlerError ->
	 
	 % If an error was raised, initialize its associated 'reqerror'
     % record and propagate it to the root handler "init()" callback
     throw(#reqerror{handler = OpHandlerName, error = OpHandlerError, req = Req1, binbody = BinBody})
   end
 end.	  




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
 



%% DESCRIPTION:  Attempts to convert a binary value to an integer and checks if it is
%%               greater or equal than a given MinValue (used by the REST handlers)
%%
%% ARGUMENTS:    - Bin:      The binary value to be converted to an integer
%%               - MinValue: The minimum value for the converted integer to pass the test
%%
%% RETURNS:      - 'Num'                -> The integer associated with the binary value,
%%                                         value, if the conversion was successful and
%%                                         it is greater or equal than MinValue
%%
%% THROWS:       - {not_an_integer,ParamName}
%%               - {out_of_range,ParamName}
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
   throw({not_an_integer,ParamName})
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
 

%% DESCRIPTION:  Parses the binary body of an HTTP request so to return a set of expected parameters  
%%
%% ARGUMENTS:    - BodyBin:      The binary body of the HTTP request to parse
%%               - ParamDefList: The list of parameters to return, each attuting to the following format:
%%                                 - The parameter name (atom)
%%                                 - The parameter type ('list' | 'integer')
%%                                    - (list only) whether the parameter is required (true) or not (false)
%%                                    - (integer only): the minimum allowed value for the parameter
%%
%% RETURNS:      - ParamsTuple -> The ordered tuple of expected parameters
%%
%% THROWS:       - body_not_json -> The body could not be interpreted in JSON format
%%               - ParseError    -> Error in parsing the parameters (see extract_bodymap_params)
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
 
 
%% DESCRIPTION:  Extracts a list of expected parameters from the body of an HTTP request cast into a map  
%%
%% ARGUMENTS:    - ParamDefList: The list of parameters to return, each attuting to the following format:
%%                                 - The parameter name (atom)
%%                                 - The parameter type ('list' | 'integer')
%%                                    - (list only) whether the parameter is required (true) or not (false)
%%                                    - (integer only): the minimum allowed value for the parameter
%%               - ParamsList:   The recursive list of extracted parameters
%%               - BodyMap:      The body of the HTTP request cast into a map
%%   
%% RETURNS:      - ParamsList -> The list of expected parameters in the body of the HTTP request
%%
%% THROWS:       - {not_an_integer,ParamNameBin} -> Expected integer parameter is not an integer
%%               - {out_of_range,ParamNameBin}   -> Expected integer parameter has an invalid value  
%%               - {unknown_jsone_type}          -> JSONE-converted parameter could not
%%                                                  be cast to a list (should not happen)
%% RAISES:       - error:badarg                -> Error in casting an argument or parameter (bad request)
%%               - error:{badkey,ParamNameBin} -> A required parameter is missing
%%

extract_expected_bodymap_params([],BodyParams,_) ->
 
 % Return the list of expected parameters
 % extracted from the body of the HTTP request
 BodyParams;
 
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
               force_bodyparam_to_list(Param,ParamName);
		  
		      integer ->
		        
			   % If the parameter is expected to be an integer,
			   % ensure it to be and to be >= ParamConstraint
			   check_int_bodyparam(Param,ParamConstraint,ParamName)
			 end,
		  
 % Append the parsed BodyParam in the list of
 % BodyParams and extract the next expected parameter
 extract_expected_bodymap_params(NextParam,BodyParams ++ [BodyParam],BodyMap).
		  


%% Casts a term of undefined type to list
%% (extract_bodymap_params(ParamsDefList,ParamsList,BodyMap) helper function)

% binary -> list
force_bodyparam_to_list(Param,_) when is_binary(Param) ->
 binary_to_list(Param);

% integer -> list
force_bodyparam_to_list(Param,_) when is_integer(Param) ->
 integer_to_list(Param);

% float -> list
force_bodyparam_to_list(Param,_) when is_float(Param) ->
 float_to_list(Param);

% tuple -> list
force_bodyparam_to_list(Param,_) when is_tuple(Param) ->
 tuple_to_list(Param);

% atom -> list
force_bodyparam_to_list(Param,_) when is_atom(Param) ->
 atom_to_list(Param);

% list (native)
force_bodyparam_to_list(Param,_) when is_list(Param) ->
 Param;

% If the parameter was mapped by the JSONE library to an
% unknown Erlang type (which should NOT happen), throw an error
force_bodyparam_to_list(Param,ParamName)  ->
 throw({unknown_jsone_cast,ParamName,Param}).
 

%% Check a body Param to be an integer and to be >= MinValue

% Parameter is an integer and >= MinValue -> Return the parameter
check_int_bodyparam(Param,MinValue,_) when is_integer(Param), Param >= MinValue ->
 Param;

% Parameter is an integer but < MinValue -> throw an error
check_int_bodyparam(Param,_,ParamName) when is_integer(Param) ->
 throw({out_of_range,ParamName,Param});
 
% Parameter is not an integer -> throw an error
check_int_bodyparam(_,_,ParamName) ->
 throw({not_an_integer,ParamName}).
 
 
 
 
%% DESCRIPTION:  Logs an error raised during the execution of a REST handler and
%%               replies with the appropriate error message to the HTTP client
%%
%% ARGUMENTS:    - HandlerName: The name of the cowboy handler the error was raised from
%%               - BodyBin:     The binary body of the HTTP request which caused the error (if any)
%%               - ErrReason:   The error reason ({error,ErrReason})
%%               - Req:         The HTTP request map associated with the error
%%               - State:       The state of the HTTP handler when the error was raised
%%   
%% RETURNS:      - {stop,ReqErr,State} -> A tuple informing cowboy to abort its
%%                                        REST state machine for this HTTP request
%%

get_error_response(HandlerError) when is_record(HandlerError,reqerror) ->
 
 % If any, convert the body of the HTTP request
 % which caused the error from binary to list
 Body = binary_to_list(HandlerError#reqerror.binbody),

 % Determine the HTTP code and the plain
 % text message associated with the error
 {ErrorCode,ErrorMsg} = err_to_code_msg(HandlerError#reqerror.error),
 
 % Log the error
 io:format("[~s]: ~s (body = ~s, code = ~w) ~n",[HandlerError#reqerror.handler,ErrorMsg,Body,ErrorCode]),

 % Return the HTTP error response to be replied to the client
 cowboy_req:reply(
	              ErrorCode,                                  % HTTP Response Code
	              #{<<"content-type">> => <<"text/plain">>},  % "Content Type" header
			      ErrorMsg,                                   % Response Body
				  HandlerError#reqerror.req                  % Associated HTTP Request
				 ).


get_unexpected_error_response(UnexpectedError,Req) ->

 % Attempt to convert the unexpected error to list (string)
 UnexpectedErrorStr = io_lib:format("~s",[UnexpectedError]),
 
 % Log the unexpected error
 io:format("[sim_resthandler]: <UNEXPECTED SERVER ERROR>: ~s~n",[UnexpectedErrorStr]),
   
 % Return the error HTTP response to be replied to the client
 cowboy_req:reply(
	              500,                                                  % HTTP Response Code
	              #{<<"content-type">> => <<"text/plain">>},            % "Content Type" header
		          "<UNEXPECTED SERVER ERROR>: " ++ UnexpectedErrorStr,  % Response Body
			      Req                                                   % Associated HTTP Request
			     ).

				   

%% Returns the HTTP error code and the error message associated with an error occured during
%% a REST handler (parse_error(HandlerName,BodyBin,ErrReason,Req,State) helper function)
 
%% ---------------------------------- Request Contents Errors ---------------------------------- % 

% Unallowed HTTP method in resource handler 
err_to_code_msg({unallowed_method,ReqMethodBin}) ->
 {405,io_lib:format("<ERROR> Method \"~s\" is not allowed in the specified path",[ReqMethodBin])};
 
% Required integer parameter is not an integer
err_to_code_msg({not_an_integer,ParamName}) ->
 {400,io_lib:format("<ERROR> Parameter \"~s\" is not an integer",[ParamName])};
 
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
err_to_code_msg({unknown_jsone_cast,ParamName,Param}) ->
 {500,io_lib:format("<SERVER ERROR> Unknown JSONE type for parameter \"~s\" (value = ~s)",[ParamName,Param])};

%% --------------------------------- Operation Handlers Errors --------------------------------- % 

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
 
%% UPDATE_LOC_NAME (POST /location/:loc_id)
%% ---------------
% Trying to operate on a location that does not exist
err_to_code_msg({location_not_exists,Loc_id}) ->
 {404,io_lib:format("<ERROR> A location with such \"loc_id\" (~w) does not exist",[Loc_id])};

%% DELETE_LOCATION (DELETE /location/:loc_id)
%% ---------------
% The location along with all its sublocations and devices were deleted from 
% the database, but an internal error occured in stopping their associated nodes
err_to_code_msg({stop_location_nodes_error,Error}) ->
 {500,io_lib:format("<SERVER ERROR> The location along with all its sublocations and devices were deleted from the database, but an internal error occured in stopping their associated nodes: ~w",[Error])};

%% --------------------------------------- Unknown Error --------------------------------------- % 
err_to_code_msg(UnknownError) ->
 {500,io_lib:format("<SERVER ERROR> Unknown error: ~p",[UnknownError])}.