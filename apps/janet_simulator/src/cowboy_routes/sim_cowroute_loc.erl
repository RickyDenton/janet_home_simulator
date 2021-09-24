%% This module represents the Cowboy REST handler for the "/location/{loc_id}" route

-module(sim_cowroute_loc).

-export([init/2,known_methods/2,allowed_methods/2,malformed_request/2]).   % Cowboy Callback Functions

%%====================================================================================================================================
%%                                              COWBOY REST HANDLER CALLBACK FUNCTIONS ("/location/{loc_id}")                                                        
%%====================================================================================================================================

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
 {Known_Methods, Req, State}.
 
 
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
 {Allowed_Methods, Req, State}.
 
 
%% 3) MALFORMED REQUEST
%% --------------------
%% PURPOSE:       Check if the request is malformed
%%                (in this context, by validating its bindings)
%% RETURNS:       - 'false' (valid request)
%%                - 'true'  (malformed request)
%% NEW STATE:     The parsed "Loc_id" is added into the state map
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