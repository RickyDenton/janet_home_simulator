%% This module represents the main logic of the HTTP test server %%

-module(httptest).
-export([init/2]).

%% Cowboy
init(Req,State) ->

 %% ------------ HTTP Request Client Information ------------ %
 
 % Retrieve the HTTP client peer information
 {IPTuple,Port} = cowboy_req:peer(Req),

 % Convert the IPTuple to a string
 IP = inet:ntoa(IPTuple),
 
 
 %% ------------- HTTP Request Path Information ------------- %
 
 % Retrieve the request scheme (HTTP)
 ReqScheme = cowboy_req:scheme(Req),
 
 % Retrieve the request target host
 ReqHost = cowboy_req:host(Req),
 
 % Retrieve the request target port
 ReqPort = cowboy_req:port(Req),
 
 % Retrieve the request path
 ReqPath = cowboy_req:path(Req),
 
 % Retrieve the request query parameters
 ReqQs = cowboy_req:qs(Req),
 
 
 %% ------------ HTTP Request Header Information ------------ %

 % Retrieve the HTTP version
 Version = cowboy_req:version(Req),
 
 % Retrieve the HTTP method
 Method = cowboy_req:method(Req),
 
 % Retrieve the "Content-Type" header
 ContentType = case cowboy_req:header(<<"content-type">>,Req) of
 
                % If it has no "Content-Type"
				% header, use a stub binary value
                undefined ->
				 <<"none">>;
				
			    % If it has, use it
				ContentType_ ->
				 ContentType_
			   end,
 
 %% TODO: Bindings?
 
 
 %% ------------- HTTP Request Body Information ------------- % 
 
 % Check if the HTTP information has a body 
 HasBody = cowboy_req:has_body(Req),
 
 % If it has, attempt to read it as a binary and in JSON format
 {Req1,BinBody,JSONBody} =
 case HasBody of
 
  % It has a body
  true ->
  
   % Read the body
   {ok,BinBody_,Req1_} = cowboy_req:read_body(Req),
   
   % Check if the body is JSON encoded
   JSONBody_ = try jsone:decode(BinBody_)
               catch
		   
		        % If it is not, set to 'not_json'
		        error:badarg ->
			     not_json
		       end,
			  
   % Return the updated request object,
   % the binary body and the JSON body		  
   {Req1_,BinBody_,JSONBody_};
   
  % If it does not have a body, return the
  % same request object and two stub values
  false ->
   {Req,none,none}
 end,
 
 
 %% ------------------ HTTP Request Summary ------------------ %  

 % Print a summary of the HTTP request
 io:format("~n[HTTP Request]~n"),
 io:format("|--[Client]:      ~s:~p~n",[IP,Port]),
 io:format("|--[Path]:        ~s://~s:~w~s~s~n",[binary_to_list(ReqScheme),binary_to_list(ReqHost),ReqPort,binary_to_list(ReqPath),binary_to_list(ReqQs)]),
 io:format("|--[Version]:     ~s~n",[Version]),
 io:format("|--[Method]:      ~s~n",[binary_to_list(Method)]),
 io:format("|--[ContentType]: ~s~n",[binary_to_list(ContentType)]),  
 
 % Depending of whether the request has a body
 case HasBody of
  false ->
  
   % If it has not a body, just report it  
   io:format("|--[Body]:        no~n");
 
  true ->
    
   % If there is, print it
   io:format("|--[Body]:        ~s~n",[binary_to_list(BinBody)]), 
   
   % Depending on whether the body is encoded in JSON
   case JSONBody of
	
	% If it is not, just report it
	not_json ->
     io:format("|--[BodyJSON]:    no~n");

   	% If it is, also print it in JSON
    _ ->
     io:format("|--[BodyJSON]:    ~1000p~n",[JSONBody])
   end
 end,

 % Define the client response
 Resp = cowboy_req:reply(200,Req1),
 
 % Respond to the client
 {ok,Resp,State}.