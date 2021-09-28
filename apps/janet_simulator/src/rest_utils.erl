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
	
   %% [TODO]: Insert "custom" here
	
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
  try HandlerMod:OpHandlerName(Req,OpHandlerArguments)
  catch 
   _:OpHandlerError ->
	 
    % If an error was raised by the operation handler, initialize its associated
	% the 'reqerror' record and propagate it to the root handler "init()" callback
    throw(#reqerror{handlername = OpHandlerName, error = OpHandlerError, errormod = HandlerMod, req = ReqBody, binbody = BinBody})
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

 % Retrieve the HTTP code and plain text message associated with the error by
 % calling the 'err_to_code_msg()' function in module "HandlerError#reqerror.errmod"
 {ErrorCode,ErrorMsg} = case HandlerError#reqerror.errormod of
 
  % Current module (resource handler or body parameters retrieval error)
  ?MODULE ->
   err_to_code_msg(HandlerError#reqerror.error);
   
  % Custom module (operation handler error)
  CustomMod ->
   CustomMod:err_to_code_msg(HandlerError#reqerror.error)
 end,
   
 % Log the error
 io:format("[~s]: ~s (body = ~s, code = ~w) ~n",[HandlerError#reqerror.handlername,ErrorMsg,Body,ErrorCode]),

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

%% --------------------------------------- Unknown Error --------------------------------------- % 
err_to_code_msg(UnknownError) ->
 {500,io_lib:format("<SERVER ERROR> Unknown error: ~p",[UnknownError])}.