%% This module represents the Cowboy REST handler for the "/location/{loc_id}" route

-module(sim_cowroute_loc).

%% ----------------------------- COWBOY REST HANDLER CALLBACK FUNCTIONS ----------------------------- %%
-export([init/2,known_methods/2,allowed_methods/2,malformed_request/2,resource_exists/2]).                  % Common Callbacks
-export([content_types_provided/2,allow_missing_post/2,content_types_accepted/2,loc_put_post_callback/2]).  % PUT, POST, PATCH Callbacks
-export([delete_resource/2]).                                                                               % DELETE Callbaks

%%====================================================================================================================================
%%                                              COWBOY REST HANDLER CALLBACK FUNCTIONS ("/location/{loc_id}")                                                        
%%====================================================================================================================================

%% ====================================================== COMMON CALLBACKS ====================================================== %% 

%% 0) INIT
%% ------- 
%% PURPOSE:     Initial HTTP request callback
%% RETURNS:     An atom informing Cowboy to handle the HTTP request via its REST state machine
%% NEW STATE:   -
%% IF NO MATCH: -
%%
init(Req,State) ->

 % Return an atom informing Cowboy to handle
 % the HTTP request via its REST state machine
 {cowboy_rest,Req,State}.


%% 1) KNOWN_METHODS
%% ---------------- 
%% PURPOSE:       Define the list of HTTP methods supported by this handler
%% RETURNS:       The list of HTTP methods supported by this handler
%% NEW STATE:     -
%% MISMATCH RESP: 501 (NOT IMPLEMENTED)
%%
known_methods(Req,State) ->

 % Define the list of HTTP methods supported by this handler
 Known_Methods = [<<"PUT">>,<<"POST">>,<<"DELETE">>],
 
 % Return the list of known HTTP methods supported by this handler
 {Known_Methods,Req,State}.
 
 
%% 2) ALLOWED_METHODS
%% ------------------
%% PURPOSE:       Define the list of HTTP methods allowed by this handler
%% RETURNS:       The list of HTTP methods allowed by this handler
%% NEW STATE:     -
%% MISMATCH RESP: 405 (METHOD NOT ALLOWED)
%%
allowed_methods(Req,State) ->

 % Define the list of HTTP methods allowed by this handler
 Allowed_Methods = [<<"PUT">>,<<"POST">>,<<"DELETE">>],
 
 % Return the list of HTTP methods allowed by this handler
 {Allowed_Methods,Req,State}.
 
 
%% 3) MALFORMED REQUEST
%% --------------------
%% PURPOSE:       Check if the request is malformed
%%                (in this context, by validating its bindings)
%% RETURNS:       - 'false' (valid request)
%%                - 'true'  (malformed request)
%% NEW STATE:     The parsed "loc_id" is added into the state map
%% MISMATCH RESP: 400 (BAD REQUEST)
%%
malformed_request(Req,State) ->

 % Retrieve the ":loc_id" path binding
 case cowboy_req:binding(loc_id, Req) of
  undefined ->
   
   % If it couldn't be retrieved, the request is malformed
   {true,Req,State};
  
  Bin ->
   
   % If it was retrieved, ensure it to be an integer >=1
   case utils:bin_to_int_comp(Bin,1) of
    {error,_} ->
	
	 % If it is not an integer or it is <1, the request is malformed
	 {true,Req,State};
	 
	Loc_id ->
	
	 % Otherwise the request is not malformed, and so add
	 % the parsed "Loc_id" to the state map before continuing 
	 {false,Req,State#{loc_id => Loc_id}}
   end
 end.
 

%% 4) CONTENT_TYPES_PROVIDED
%% -------------------------
%% PURPOSE:       Define the list of MIME types for the response this handler can provide
%% RETURNS:       The list of MIME types for the response this handler can provide
%% NEW STATE:     -
%% MISMATCH RESP: 406 (NOT ACCEPTABLE) (shouldn't happen, since it
%%                    can happen only for GET and HEAD requests)
%%
content_types_provided(Req,State) ->

 % Define the list of MIME types for the response this handler can provide
 Content_Types_Provided = 
  [
   {{<<"application">>, <<"json">>, '*'}, get_json},  % JSON
   {{<<"text">>, <<"plain">>, '*'}, get_text}         % Plain text
  ],
  
 % Return the list of MIME types for the response this handler can provide
 {Content_Types_Provided,Req,State}.
 

%% 5) RESOURCE_EXISTS
%% ------------------
%% PURPOSE:       Determine if the specified resource exists
%% RETURNS:       - 'true'  (resource exists)
%%                - 'false' (resourse does not exist)
%% NEW STATE:     A boolean "exists" is added into the state map (optimization purposes,
%%                avoides a duplicate transaction into IS_CONFLICT callback for the PUT method)
%% MISMATCH RESP: -
%%
resource_exists(Req,State) ->

 % Retrieve the "Loc_id" from the state map
 #{loc_id := Loc_id} = State,

 % Check if the location "Loc_id" exists in the Mnesia database
 case db:get_record(location,Loc_id) of
 
  % If the location was found and so the resource exists, set
  % accordingly the 'exists' variable in the state map before continuing
  {ok,_} ->
   {true,Req,State#{exists => true}};
  
  % If the location was NOT found and so the resource does not exist, set
  % accordingly the 'exists' variable in the state map before continuing
  {error,_} ->
   {false,Req,State#{exists => false}}
 end.
 
 
%% ================================================= PUT, POST, PATCH CALLBACKS ================================================= %% 
 
%% ================ NO! DO LATER TO PREVENT RACE CONDITIONS AND TO RETURN A MESSAGE ================ % 
 
%% 0) IS_CONFLICT
%% ----------------
%% PURPOSE:       Determine if a conflict arises in creating the resource via the PUT
%%                method (in this instance, if it already exists, there is a conflict)
%% RETURNS:       - 'true'  (conflict)
%%                - 'false' (no conflict)
%% NEW STATE:     -
%% MISMATCH RESP: 409 (CONFLICT)
%% NOTE:          A conflict may also arise if the port specified in the body is already
%%                taken, which will be processed in the 'loc_put_post_callback' function
%%

% PUT + resource exists -> conflict
%is_conflict(Req=#{method := <<"PUT">>},State=#{exists := true}) ->
% {true,Req,State};
 
% All other cases -> no conflict
%is_conflict(Req,State) ->
% {false,Req,State}.
 
%% ================================================================================================= % 


%% 1) ALLOW_MISSING_POST
%% ---------------------
%% PURPOSE:       Determine whether the POST method is allowed if the resource does not exist
%% RETURNS:       - 'true' (allowed)
%%                - 'false'(not allowed) <-- Not allowed in this instance
%% NEW STATE:     -
%% MISMATCH RESP: 404 (NOT FOUND)
%%
allow_missing_post(Req,State) ->
 {false,Req,State}.


%% 2) CONTENT_TYPES_ACCEPTED
%% -------------------------
%% PURPOSE:       Define the list of MIME types for the request body this
%%                handler can parse, along with their associated AcceptCallbacks
%% RETURNS:       The list of MIME types for the request body this handler
%%                can parse, along with their associated AcceptCallbacks
%% NEW STATE:     -
%% MISMATCH RESP: 415 (UNSUPPORTED MEDIA TYPE)
%%
content_types_accepted(Req,State) ->

 % Define the list of MIME types for the request body this
 % handler can parse, along with their associated AcceptCallbacks
 Content_Types_Accepted = 
  [
   {{<<"application">>, <<"json">>, '*'}, loc_put_post_callback}  % JSON
  ],
 
 % Return the list of MIME types for the request body this
 % handler can parse, along with their associated AcceptCallbacks
 {Content_Types_Accepted,Req,State}.
 
 
%% 3) LOC_PUT_POST_CALLBACK
%% -------------------------------
%% PURPOSE:       Process the PUT or POST request body and execute the
%%                application-specific operation associated with the request
%% RETURNS:       - 'true':  The operation was successful
%%                - 'false': The operation was unsuccessful due
%%                           to invalid parameters in the body 
%% NEW STATE:     -
%% MISMATCH RESP: 400 (BAD REQUEST)
%%

% PUT -> jsim:add_location(Loc_id,Name,User,Port)
loc_put_post_callback(Req=#{method := <<"PUT">>},State) ->
 
 % Retrieve the "Loc_id" from the state map
 #{loc_id := Loc_id} = State,
 
 % Retrieve the request body as a binary
 {ok,BodyBin,Req1} = cowboy_req:read_body(Req),
 
 % Define the list of parameters to be retrieved
 % from the body, each being characterized by:
 % 
 % - The parameter name (atom)
 % - The parameter type ('list' | 'integer')
 % - (list only) whether the parameter is required (true) or not (false)
 % - (integer only): the minimum allowed value for the parameter
 ParamsDefList = [
                  {name,list,optional},  % "Name" parameter (optional)
                  {user,list,optional},  % "User" parameter (optional)
                  {port,integer,30000}   % "Port" parameter (required, >= 30000)
                 ],
 
 % Attempt to retrieve the required
 % parameters from the request body
 Params = try parse_req_body(BodyBin,ParamsDefList)
          catch
		   ExcType:ExcReason ->
		    io:format("Error in parse_req_body (ExcType: ~w, ExcReason: ~w)~n",[ExcType,ExcReason]),
		    {error,ExcReason}
		  end,
		  
 % Check whether the list of parameters was successfully retrieved
 case Params of
 
  {error,ParseParamsError} ->
   parse_error("sim_rest_loc_put",BodyBin,ParseParamsError,Req1,State);
 
  {Name,User,Port} ->
   
   % Attempt to add the location
   case jsim:add_location(Loc_id,Name,User,Port) of
   
    {ok,ok} ->
	 io:format("[sim_rest_loc_put]: Added location {loc_id = ~w, name = ~p, user = ~p, port = ~w)~n",[Loc_id,Name,User,Port]),
     {true,Req1,State};
	 
    {error,OpError} ->
	 parse_error("sim_rest_loc_put",BodyBin,OpError,Req1,State);
	 
	{ok,_} ->
	 parse_error("sim_rest_loc_put",BodyBin,controller_spawn_error,Req1,State) 

   end
 end;


% POST -> jsim:update_loc_name(Loc_id,Name)
loc_put_post_callback(Req=#{method := <<"POST">>},State) ->

 % Retrieve the "Loc_id" from the state map
 #{loc_id := Loc_id} = State,
 
 % Retrieve the request body as a binary
 {ok,BodyBin,Req1} = cowboy_req:read_body(Req),
 
 % Define the list of parameters to be retrieved
 % from the body, each being characterized by:
 % 
 % - The parameter name (atom)
 % - The parameter type ('list' | 'integer')
 % - (list only) whether the parameter is required (true) or not (false)
 % - (integer only): the minimum allowed value for the parameter
 ParamsDefList = [{name,list,required}],  % "Name" parameter (required)
 
 % Attempt to retrieve the required
 % parameters from the request body
 Params = try parse_req_body(BodyBin,ParamsDefList)
          catch
		   ExcType:ExcReason ->
		    io:format("Error in parse_req_body (ExcType: ~w, ExcReason: ~w)~n",[ExcType,ExcReason]),
		    {error,ExcReason}
		  end,

 % Check whether the list of parameters was successfully retrieved
 case Params of
 
  {error,ParseParamsError} ->
   parse_error("sim_rest_loc_put",BodyBin,ParseParamsError,Req1,State);
 
  {Name} ->
   
   % Attempt to update the location's name
   case jsim:update_loc_name(Loc_id,Name) of
   
    ok ->
	 io:format("[sim_rest_loc_put]: Updated location name {loc_id = ~w, name = ~p)~n",[Loc_id,Name]),
     {true,Req1,State};
	 
    {error,OpError} ->
	 parse_error("sim_rest_loc_put",BodyBin,OpError,Req1,State)
   end
 end.


%% ===================================================== DELETE CALLBACKS ====================================================== %%

%% 1) DELETE_RESOURCE
%% -----------------------
%% PURPOSE:       Determine the resource specified in the request
%% RETURNS:       - 'true' (resource deleted)
%%                - 'false'(resource not deleted)
%% NEW STATE:     -
%% MISMATCH RESP: 500 (INTERNAL SERVER ERROR)
%%

% DELETE -> jsim:delete_location(Loc_id)
delete_resource(Req,State) ->

 % Retrieve the "Loc_id" from the state map
 #{loc_id := Loc_id} = State,
 
 % Attempt to delete the location, along with its
 % controller and all its sublocations and devices
 %
 % NOTE: This currently can return only {ok,ok} since
 %       the location was already checked to exist in
 %       the "resource_exists" callback
 case jsim:delete_location(Loc_id) of
    
  {ok,ok} ->
   io:format("[sim_rest_loc_put]: Deleted location (loc_id = ~w)~n",[Loc_id]),
   {true,Req,State};
	 
  {error,OpError} ->
   parse_error("sim_rest_loc_put",<<"(not_read)">>,OpError,Req,State)
 end.


%%====================================================================================================================================
%%                                                    PRIVATE HELPER FUNCTIONS
%%==================================================================================================================================== 

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
parse_req_body(BodyBin,ParamsDefList) ->

 % Interpret the binary body as JSON
 % and attempt to cast it into a map
 BodyMap = try jsone:decode(BodyBin)
           catch
		   
		    % If the body could not be
			% interpreted as JSON, throw an error
		    error:badarg ->
			 throw(body_not_json)
		   end,

 % Attempt to extract the list of expected parameters from the body map
 ParamsList = extract_bodymap_params(ParamsDefList,[],BodyMap),
 
 % Return the ordered list of body parameters as a tuple
 list_to_tuple(ParamsList).
 
 
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

%% Base case
extract_bodymap_params([],ParamsList,_) ->
 
 % Return the list of extracted parameters
 ParamsList;
 
%% List Parameter (required or optional)
extract_bodymap_params([{ParamName,list,Required}|NextParam],ParamsList,BodyMap) ->

 % Convert the expected list parameter name to a binary
 ParamNameBin = atom_to_binary(ParamName),

 % Attempt to retrieve the list parameter from the body map
 BodyParam = case Required of
 
          % If the parameter is required and was not found,
		  % a {badkey,<<"ParamName">>} error is raised
	      required ->
           maps:get(ParamNameBin,BodyMap);
		   
		  % If the parameter is optional and was not
		  % found, use an empty binary list for its value
          optional ->
		   try maps:get(ParamNameBin,BodyMap)
           catch
            error:{badkey,ParamNameBin} ->
	         <<"">>
           end
         end,

 % Cast the parameter to list depending on the type
 % on which it was mapped to by the JSONE library
 Param = cast_to_list(BodyParam),	
	       
 % Append the parameter into the ParamsList
 % and proceed with the next parameter
 extract_bodymap_params(NextParam,ParamsList ++ [Param],BodyMap);

%% Integer parameter (always required, to be checked against a MinValue)
extract_bodymap_params([{ParamName,integer,MinValue}|NextParam],ParamsList,BodyMap) ->
   
  % Convert the expected integer parameter name to a binary
 ParamNameBin = atom_to_binary(ParamName), 
 
 % Attempt to retrieve the integer parameter from the body map,
 % raising a {badkey,<<"ParamName">>} error if it is not found
 Param = maps:get(ParamNameBin,BodyMap),
 
 % Ensure the extracted parameter to be an integer
 case is_integer(Param) of
  false ->
  
   % If it is not, throw an error
   throw({not_an_integer,ParamNameBin});
   
  true ->
  
   % If it is, ensure it to be >= MinValue
   if
    Param < MinValue ->
	 
	 % If it is not, throw an error
     throw({out_of_range,ParamNameBin});

    true ->

     % If it is, append it into the ParamsList
	 % and proceed with the next parameter
     extract_bodymap_params(NextParam,ParamsList ++ [Param],BodyMap)
   end
 end.


%% Casts a term of undefined type to list
%% (extract_bodymap_params(ParamsDefList,ParamsList,BodyMap) helper function)

% binary -> list
cast_to_list(BodyParam) when is_binary(BodyParam) ->
 binary_to_list(BodyParam);

% integer -> list
cast_to_list(BodyParam) when is_integer(BodyParam) ->
 integer_to_list(BodyParam);

% float -> list
cast_to_list(BodyParam) when is_float(BodyParam) ->
 float_to_list(BodyParam);

% tuple -> list
cast_to_list(BodyParam) when is_tuple(BodyParam) ->
 tuple_to_list(BodyParam);

% atom -> list
cast_to_list(BodyParam) when is_atom(BodyParam) ->
 atom_to_list(BodyParam);

% list (native)
cast_to_list(BodyParam) when is_list(BodyParam) ->
 BodyParam;
 
% Unknown Erlang Type (should not happen)
cast_to_list(_)  ->

 % Throw an exception notifying that the parameter was converted to an unknown Erlang type by the JSONE
 % library (not including the parameter itself since would cause a "badarg" in the parse_error() function
 throw(unknown_jsone_type).


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
parse_error(HandlerName,BodyBin,ErrReason,Req,State) ->

 % Convert the binary body of the HTTP request
 % which caused the error to list (string)
 Body = binary_to_list(BodyBin),
 
 % In case of a composite reason ({error,{ErrReason,Info}),
 % convert the additional info to list (string)
 ConvErrReason = case ErrReason of
                 
				  {Reason,BinParam} ->
				   {Reason,binary_to_list(BinParam)};
				   
				  Reason ->
				   Reason
				 end,
 
 % Retrieve the HTTP error code and the
 % error message associated with the error
 {ErrorCode,ErrorMsg} = get_err_code_msg(ConvErrReason),
 
 % Log the error
 io:format("[~s]: ~s (body = ~s, resp = ~w) ~n",[HandlerName,ErrorMsg,Body,ErrorCode]),

 % Reply the client with the error
 ReqErr = cowboy_req:reply(ErrorCode,
                           #{<<"content-type">> => <<"text/plain">>},
						   ErrorMsg,
						   Req),
						  
 % Inform cowboy to abort its REST state machine for this HTTP request
 {stop,ReqErr,State}.


%% Returns the HTTP error code and the error message associated with an error occured during
%% a REST handler (parse_error(HandlerName,BodyBin,ErrReason,Req,State) helper function)
  
%% --------------------------------- Request Body Errors --------------------------------- %

% HTTP request body could not be interpreted as JSON
get_err_code_msg(body_not_json) ->
 {400,"<ERROR> Request body could not be interpreted as JSON"};
 
% Required parameter missing from the body
get_err_code_msg({badkey,Param}) ->
 {400,io_lib:format("<ERROR> Required parameter \"~s\" is missing",[Param])};
 
% Required integer parameter is not an integer
get_err_code_msg({not_an_integer,Param}) ->
 {400,io_lib:format("<ERROR> Parameter \"~s\" is not an integer",[Param])};
 
% Required integer parameter is of invalid value
get_err_code_msg({out_of_range,Param}) ->
 {400,io_lib:format("<ERROR> Invalid value of parameter \"~s\"",[Param])};
 
% A list parameter was converted to an unknown Erlang type by the JSONE library
get_err_code_msg(unknown_jsone_type) ->
 {500,io_lib:format("<SERVER ERROR> Unknown JSONE type")};

%% ----------------------------- Operation Execution Errors ----------------------------- %

% Trying to add a location that already exists
get_err_code_msg(location_already_exists) ->
 {409,"<ERROR> Location already exists in the Simulator database"};
 
% Trying to add a location with a port already taken
get_err_code_msg(port_already_taken) ->
 {412,"<ERROR> Specified \"port\" is already assigned to another controller"};

% Trying to add a location whose port was already assigned by the host OS
get_err_code_msg(host_port_taken) ->
 {412,"<ERROR> Specified \"port\" is already assigned in the host OS"};
 
% Location successfully added, but the controller could not
% be spawned (a restart of the JANET Simulator is required)
get_err_code_msg(controller_spawn_error) ->
 {500,"<SERVER ERROR> The location was added, but its controller could not be spawned"};

%% --------------------------------- Unhandled Error --------------------------------- %
get_err_code_msg(UnhandledError) ->
 {500,io_lib:format("<SERVER ERROR> Unhandled error: {error,~p}",[UnhandledError])}.