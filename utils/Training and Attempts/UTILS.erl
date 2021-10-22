subtry() ->
  F = fun() ->
       mnesia:match_object(#sublocation{sub_id = {1,'_'}, _ = '_'})
	  end,
  mnesia:transaction(F).



read_location(Loc_id) ->
 F = fun() ->
      mnesia:read({location,Loc_id})
	 end,
 case mnesia:transaction(F) of
  {atomic,[]} ->
   undefined;
  {atomic,[#location{loc_id=Loc_id,name=Name,user=User,port=Port}]} ->
   {Loc_id,Name,User,Port}
  end.
  
  
  
%% Atoms concatenation (old)
concat_atoms(Atom1,Atom2) ->
 list_to_atom(atom_to_list(Atom1) ++ atom_to_list(Atom2)).
 
 
 
%% Stub function for retrieving the manager children of a 'sup_loc' supervisor 
try_fun(Loc_id) ->

 case db:get_record(suploc,Loc_id) of

  % If the node's location supervisor was not found, there is a consistency 
  % error between the 'suploc' and Mgrtable tables, and so return an error
  {error,not_found} ->
   {error,{internal,suploc_not_started}};
   
  % If the node's location supervisor was found	
  {ok,SuplocRecord} -> 
	  
   % Retrieve the supervisor's PID
   Sup_pid = SuplocRecord#suploc.sup_pid,
   
   % Retrieve the sup_loc child specifications
   ChildSpecs = supervisor:which_children(Sup_pid),
   
   SuplocChildren = decode_suploc_children(ChildSpecs,[],[]),
   
   io:format("~w",[SuplocChildren])
   
 end.

     
decode_suploc_children([],CtrNode,DevNodes) ->
 {CtrNode,DevNodes};	 
decode_suploc_children([ChildSpec|Next_ChildSpec],CtrNode,DevNodes) ->
 SupChildID = element(1,ChildSpec),
 case SupChildID of
 
  "loc_devs_init" ->
   decode_suploc_children(Next_ChildSpec,CtrNode,DevNodes);
   
  _ ->
   SupChildType = string:sub_string(SupChildID,1,3),
   case SupChildType of
    "ctr" ->
     Loc_id = string:sub_string(SupChildID,5),
     decode_suploc_children(Next_ChildSpec,list_to_integer(Loc_id),DevNodes);
  
    "dev" ->
     Dev_id = string:sub_string(SupChildID,5),
     decode_suploc_children(Next_ChildSpec,CtrNode,DevNodes ++ [list_to_integer(Dev_id)])
   end
 end.
 
 
 
 
 
 
%% -----------------------------------------------   HANDLE CASTS() Examples ----------------------------------------------- %%

%% SENDER:    The device's dev_server
%% WHEN:      During the dev_server initialization
%% CONTENTS:  The dev_server's PID
%% MATCHES:   If the dev_statem_pid was not yet received during boot or when its running if the dev_server crashes and it is restarted
%% ACTIONS:   None
%% NEW STATE: Update the dev_srv_pid
%%
handle_cast({dev_srv_pid,DevSrvPid},SrvState) when SrvState#devmgrstate.dev_state =/= booting orelse SrvState#devmgrstate.dev_statem_pid =:= none ->

 % Update the dev_srv_pid
 {noreply,SrvState#devmgrstate{dev_srv_pid = DevSrvPid}};


%% SENDER:    The device's dev_statem
%% WHEN:      During the dev_statem initialization
%% CONTENTS:  The dev_statem's PID
%% MATCHES:   If the dev_srv_pid was not yet received during boot or when its running if the dev_statem crashes and it is restarted
%% ACTIONS:   None
%% NEW STATE: Update the dev_statem_pid 
%%
handle_cast({dev_statem_pid,DevStatemPid},SrvState) when SrvState#devmgrstate.dev_state =/= booting orelse SrvState#devmgrstate.dev_srv_pid =:= none ->

 % Update the dev_statem_pid
 {noreply,SrvState#devmgrstate{dev_statem_pid = DevStatemPid}};

 
%% SENDER:    The device's dev_server
%% WHEN:      During the dev_server initialization
%% CONTENTS:  The dev_server's PID
%% MATCHES:   If the device is booting and the the dev_statem_pid was already received
%% ACTIONS:   Update the device's state to "CONNECTING" in the 'devmanager' table
%% NEW STATE: Update the dev_state to 'connecting', update the dev_srv_pid
%%
handle_cast({dev_srv_pid,DevSrvPid},SrvState) when SrvState#devmgrstate.dev_state =:= booting andalso SrvState#devmgrstate.dev_statem_pid =/= none ->

 % Update the device's state to "CONNECTING" in the 'devmanager' table
 {atomic,ok} = mnesia:transaction(fun() -> mnesia:write(#devmanager{dev_id=SrvState#devmgrstate.dev_id,loc_id=SrvState#devmgrstate.loc_id,mgr_pid=self(),status="CONNECTING"}) end),
 
 % Log that the device is now connecting
 io:format("[devmgr-" ++ integer_to_list(SrvState#devmgrstate.dev_id) ++ "]: Device node successfully booted~n"),
 
 % Update the dev_state and the dev_srv_pid
 {noreply,SrvState#devmgrstate{dev_state = connecting, dev_srv_pid = DevSrvPid}};


%% SENDER:    The device's dev_statem
%% WHEN:      During the dev_statem initialization
%% CONTENTS:  The dev_statem's PID
%% MATCHES:   If the device is booting and the the dev_srv_pid was already received
%% ACTIONS:   Update the device's state to "CONNECTING" in the 'devmanager' table
%% NEW STATE: Update the dev_state to 'connecting', update the dev_statem_pid
%%
handle_cast({dev_statem_pid,DevStatemPid},SrvState) when SrvState#devmgrstate.dev_state =:= booting andalso SrvState#devmgrstate.dev_srv_pid =/= none ->

 % Update the device's state to "CONNECTING" in the 'devmanager' table
 {atomic,ok} = mnesia:transaction(fun() -> mnesia:write(#devmanager{dev_id=SrvState#devmgrstate.dev_id,loc_id=SrvState#devmgrstate.loc_id,mgr_pid=self(),status="CONNECTING"}) end),
 
 % Log that the device is now connecting
 io:format("[devmgr-" ++ integer_to_list(SrvState#devmgrstate.dev_id) ++ "]: Device node successfully booted~n"),
 
 % Update the dev_state and the dev_statem_pid
 {noreply,SrvState#devmgrstate{dev_state = connecting, dev_statem_pid = DevStatemPid}}.
 
 
%% -----------------------------------------------  Controller nodes ETS tables initializations ----------------------------------------------- %% 

 %% This was discarded  to prevent transaction problems in the Janet Controller nodes
 
 % Retrieve the 'devalloc' environment variable
 {ok,DevAlloc} = application:get_env(devalloc),

 % Initialize the 'devalloc' ETS table through the DevAlloc variable
 ets:new(devalloc,[set,public,named_table]),
 ets:insert(devalloc,DevAlloc),
 
 % Derive the list of devices in the location via the DevAlloc variable
 DevList = lists:flatten([ SublocDevList || {_,SublocDevList} <- DevAlloc ]),
 
 % Initialize the 'devlist' ETS table with the list of devices in the location
 ets:new(devlist,[set,public,named_table]),
 ets:insert(devlist,{devlist,DevList}),

 % Initialize the 'devregister' ETS table
 ets:new(devregister,[set,public,named_table]),
 
 %% -------------------------------------------------- Dynamic Function/Arguments evaluation -------------------------------------------------- %%
 
 https://erlang.org/doc/man/erlang.html#apply-3

 fafa(Module,Function,Args) ->
  apply(Module,Function,Args).
 
 fafa2(Module,Function,Arg) ->
  Module:Function(Arg).
  
 %% ---------------------------------------------------- OLD print_tree_device() Line 552 ---------------------------------------------------- %%
 
 % Print information on the device
 io:format("~s{~w,~s,~w,~w,~p} - ~s~n",[Indent,Dev#device.dev_id,io_lib:format("~p",[Dev#device.name]),Dev#device.sub_id,Dev#device.type,utils:dev_cfg_to_str(Dev#device.config),DevMgrStatus]),
 print_tree_device(NextDev,Indent).
 
 
  
%% ====================================================== CTR_DB: DELETE_DEVICE HANDLER TERMINATION ====================================================== %% 

%% (while this operation was still in the transaction, which caused a deadlock)

terminate_dev_handler('-') ->

 % If the device was not paired with the controller, return
 ok;
 
terminate_dev_handler(Devhandler_pid) ->

 % If the device was paired with the controller, attempt to
 % terminate its handler via the 'sup_devhandlers' supervisor	
 %		   
 % NOTE: In this particular case the operation can be performed within the transaction context
 %       since it has no further side-effects and the only error that it can raise, which is 
 %       relative to the fact that the handler has already terminated, can be ignored
 supervisor:terminate_child(sup_devhandlers,Devhandler_pid),
 ok.



 
%% ====================================================== CTR_MANAGER: DERIVE_CTR_SUBLOC_TABLE ====================================================== %% 

%% (Before moving most of the 'device' table into the 'ctr_device' table)

%% ---------- In handle_continue() ---------- %%
 
% Retrieve the records of the location's sublocations and use them for preparing the controller's 'ctr_sublocation' table
{atomic,SublocationRecords} = mnesia:transaction(fun() -> mnesia:match_object(#sublocation{sub_id = {Loc_id,'_'}, _ = '_'}) end),
CtrSublocTable = derive_ctr_subloc_table(SublocationRecords,[]),

%% ------------- Helper Function ------------- %%

%% Derives the controller's 'ctr_sublocation' table from the records of sublocations in the location (handle_continue(init,{booting,none,Loc_id}) helper function)
derive_ctr_subloc_table([],CtrSublocTable) ->

 % Return the final controller's 'ctr_sublocation' table
 CtrSublocTable;
 
derive_ctr_subloc_table([Subloc|NextSubloc],CtrSublocTable) ->

 % Retrieve the 'subloc_id' associated with the sublocation
 {_,Subloc_id} = Subloc#sublocation.sub_id,
 
 % Append the 'subloc_id' and the 'devlist' of devices in the location to the "CtrSublocTable"
 NextCtrSublocTable = CtrSublocTable ++ [{Subloc_id,Subloc#sublocation.devlist}],
 
 % Parse the next sublocation record
 derive_ctr_subloc_table(NextSubloc,NextCtrSublocTable).
 
 
%% ====================================================== CTR_DB: INIT_CTR_TABLES ====================================================== %%

%% (Before moving most of the 'device' table into the 'ctr_device' table)


%% Initializes the contents of the controller's 'sublocation' and 'device' tables (init_mnesia(CtrSublocTable) helper function)
%%
%% NOTE: dirty_writes are used for improving performance since at this time
%%       no other process is using the Mnesia database on the controller node
%%
init_ctr_tables([]) ->
 ok;
init_ctr_tables([{Subloc_id,SublocDevList}|Next_SubAlloc]) ->

 % Write the {Subloc_id,SublocDevlist} entry in the 'ctr_sublocation' table
 ok = mnesia:dirty_write(#ctr_sublocation{subloc_id = Subloc_id, devlist = SublocDevList}),
 
 % Initialize the 'ctr_device' records of all devices in the sublocation 
 ok = init_ctr_dev_records(SublocDevList,Subloc_id),
 
 % Initialize the next 'ctr_sublocation' entry
 init_ctr_tables(Next_SubAlloc).

%% Initializes the 'ctr_device' records associated with the list of devices in a sublocation (init_ctr_tables([{Subloc_id,SublocDevList}|Next_SubAlloc]) helper function)
init_ctr_dev_records([],_) ->
 ok;
init_ctr_dev_records([Dev_id|Next_Devid],Subloc_id) ->

 % Write the {Dev_id,Subloc_id,-} entry to the 'ctr_device' table
 ok = mnesia:dirty_write(#ctr_device{dev_id = Dev_id, subloc_id = Subloc_id, handler_pid = '-'}),
 
 % Initialize the next 'ctr_device' entry
 init_ctr_dev_records(Next_Devid,Subloc_id).


%% ============================================ SIMULATOR COWBOY ROUTES (with constraints) ============================================ %%

init(_) ->

 % Trap exit signals so to allow cleanup operations when terminating (terminate(Reason,SrvState) callback function)
 process_flag(trap_exit,true),
 
 % Retrieve the 'rest_port' and 'remote_host' environment variables
 {ok,RESTPort} = application:get_env(rest_port),
 {ok,RemoteHost} = application:get_env(remote_host),
 
 % Start the Ranch TCP acceptor required by the Cowboy REST server
 application:start(ranch),

 Greater0 =
 fun 
  (forward,X) when X > 0 ->
   {ok,X};
  (forward,_) ->
   {error,non_greater_zero};
  (format_error,{non_greater_zero,X})  ->
   io_lib:format("~p is not > 0", [X])
 end,

 GreaterEqual0 =
 fun 
  (forward,X) when X >= 0 ->
   {ok,X};
  (forward,_) ->
   {error,non_greater_equal_zero};
  (format_error,{non_greater_equal_zero,X})  ->
   io_lib:format("~p is not >= 0", [X])
 end,

 % Initialize the list of resource paths accepted by Cowboy as of the JANET Simulator REST interface
 %
 % Paths = [{Path,Constraints,CallbackModule,InitialState}]        
 %
 Paths = [
          % add_location(), update_loc_name(), delete_location()
          {"/location/:loc_id",[{loc_id,[int,Greater0]}],?MODULE,[]},						
			 
	      % update_subloc_name()
		  {"/location/:loc_id/sublocation/:subloc:id",[{loc_id,[int,Greater0]},{subloc_id,[int,GreaterEqual0]}],?MODULE,[]},
			 
		  % update_dev_name()
		  {"/device/:dev_id",[{dev_id,[int,Greater0]}],?MODULE,[]}
	     ],
			 
 % Initialize the list of Cowboy routes by merging the list of resource
 % paths with the list of accepted hosts (the RemoteHost and localhost)
 Routes = [{"localhost",Paths},{RemoteHost,Paths}],
 
 % Compile the Cowboy Routes
 CompiledRoutes = cowboy_router:compile(Routes),
   
 % Cowboy
 %Dispatch = cowboy_router:compile([ {'_', [{"/",sim_resthandler, []}]} ]),
 {ok,_} = cowboy:start_clear(sim_restserver,[{port, RESTPort}],#{env => #{dispatch => CompiledRoutes}}),

 io:format("[sim_resthandler]: Initialized~n"),
 {ok,null}.  	


%% =========================================================== REGEX Example =========================================================== %%
re:run("/location/0","^/location/(\\d+)$").    
 -> {match,[{0,11},{10,1}]}


%% ================================================= handle_req() before reorganization ================================================= %%

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

%% ======================================== CHECK IF CONFIG COMMAND IS EFFECTIVE ======================================== %% 

% In parse_devcommands() (inner)
{empty_cfgcmd,DevType} ->
 throw(#{dev_id => Dev_id, status => 417, errorReason => "The command would have no effect on the device according to its type (" ++ atom_to_list(DevType) ++ ")"}); 
 
 
check_cfgcmd_effective([],Type) ->

 % Throw an error signaling that the resulting
 % configuration command would not have any effect
 throw({empty_cfgcmd,Type}); 
 
check_cfgcmd_effective(['$keep'|NextTraitValue],Type) ->
 
 % If the trait is '$keep', search the next one
 check_cfgcmd_effective(NextTraitValue,Type);
 
check_cfgcmd_effective(_,_) ->
 
 % If the trait is not '$keep', return ok
 ok.


%% ============================================== DEVCOMMANDS SAMPLE QUERY ============================================== %%
%% location: 90
curl -v -d "[{\"dev_id\":100,\"actions\":{\"onOff\":\"off\",\"tempTarget\":49,\"fanSpeed\":99}},{\"dev_id\":104,\"actions\":{\"onOff\":\"off\",\"fanSpeed\":96}},{\"dev_id\":4},{\"dev_id\":103,\"actions\":{\"openClose\":\"close\"}},{\"dev_id\":102,\"actions\":{\"tempTarget\":1}},{\"dev_id\":105,\"actions\":{\"brightness\":21,\"color\":\"FF23A5\"}}]" -H 'Content-Type: application/json' -X PATCH  http://localhost:40000/devcommands


%% ========================================== ISSUE_DEVCOMMANDS GENERAL FAILURE ========================================= %%

%% Before list comprehension (line 1296)

...

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
   io:format("[issue_devcommands~w]: error: ~p~n",[Loc_id,Reason]),
   
   % Convert all valid in failed commands with the same error Reason
   {[],issue_devcommands_failure(ValidCommands,[],Reason)}
 end.
 
 
%% Converts all valid in failed commands with the same error Reason( issue_devcommands(ValidCommands) helper function)
issue_devcommands_failure([],FailedCommands,_) ->

 % Base case (return the list of failed commands)
 FailedCommands;
 
issue_devcommands_failure([ValidCommand|NextValidCommand],FailedCommands,ErrorReason) ->
 
 % Convert the valid command to failed specifying the error Reason
 FailedCommand = #{dev_id => ValidCommand#valdevcmd.dev_id, status => 500, errorReason => list_to_binary(lists:flatten(io_lib:format("Server error in issuing the device commands: ~p",[ErrorReason])))},
 
 % Proceed with the next command
 issue_devcommands_failure(NextValidCommand, FailedCommands ++ [FailedCommand], ErrorReason).


%% ================================================== DEVMAP_TO_CONFIG ================================================== %%

%% Was used to convert a complete device configuration map to its record form before individual trait updates (utils)

%% DESCRIPTION:  Converts a device's configuration encoded in a map (in which
%%               all traits must be present) into its associated #devcfg record
%%
%% ARGUMENTS:    - CfgMap: The device configuration as a map, in which all traits/keys must be present
%%
%% RETURNS:      - #devcfg -> The device configuration record associated with the CfgMap
%% 
%% THROWS:       - {error,invalid_devtype} -> The device type is invalid (or there
%%                                            are missing traits/keys in the map)
%%
% Conditioner
devmap_to_config(#{'onOff' := OnOff, 'tempTarget' := TempTarget, 'tempCurrent' := TempCurrent, 'fanSpeed' := FanSpeed}) ->
 #condcfg{onoff = OnOff, temp_target = TempTarget, temp_current = TempCurrent, fanspeed = FanSpeed};

% Thermostat
devmap_to_config(#{'onOff' := OnOff, 'tempTarget' := TempTarget, 'tempCurrent' := TempCurrent}) ->
 #thermocfg{onoff = OnOff, temp_target = TempTarget, temp_current = TempCurrent};

% Fan
devmap_to_config(#{'onOff' := OnOff, 'fanSpeed' := FanSpeed}) ->
 #fancfg{onoff = OnOff, fanspeed = FanSpeed};

% Light
devmap_to_config(#{'onOff' := OnOff, 'brightness' := Brightness, 'color' := Color}) ->
 #lightcfg{onoff = OnOff, brightness = Brightness, colorsetting = Color};

% Door
devmap_to_config(#{'openClose' := OpenClose, 'lockUnlock' := LockUnlock}) ->
 #doorcfg{openclose = OpenClose, lockunlock = LockUnlock};

% Invalid device type
devmap_to_config(_) ->
 throw({error,invalid_devtype}).

%% =============================================== RUNTIME CONFIGURATION CHANGE =============================================== %%

%% set_env(), application_load() and application_unload() shenanigans (too convoluted to be practical)


%% DESCRIPTION:  Attempts to start the JANET Simulator application by overriding one or
%%               more of its default configuration parameters ("config/sys.config" file)
%%
%% ARGUMENTS:    A map specifying the keys and values of the configuration
%%               parameters to be overridden, with the following being allowed:
%%
%%                 - sim_rest_port => int() >= 30000:      The OS port to be used by the JANET Simulator REST server
%%                                                         (>= 30000 for reducing the chances port allocation conflicts)
%%                 - remote_rest_client => list():         The address of the remote client issuing REST requests
%%                                                         to the JANET Simulator and Controller nodes
%%                 - remote_rest_server_addr => list():    The address of the remote server accepting REST
%%                                                         requests from the JANET Controller nodes
%%                 - remote_rest_server_port => int() > 0: The port of the remote server accepting REST
%%                                                         requests from the JANET Controller nodes (int > 0)
%%
%% RETURNS:      - ok                      -> JANET Simulator succesfully started
%%               - {error,already_running} -> The janet_simulator application is already running on the node
%%               - {mnesia_error,Reason}   -> The Mnesia database is not in a consistent state
%%                                            (probably a wrong or no schema is installed)
%%               - {error,port_conflict}   -> The default or custom port to be used by the JANET
%%                                            Simulator REST server is not available in the host OS 
%%               - {error,Reason}          -> Internal error in starting the application
%%               - {error,badarg}          -> Invalid arguments
%%
run(CfgMap) when is_map(CfgMap) ->

 % Attempt to set the custom configuration passed by the user
 case set_custom_config(CfgMap) of
 
  % If the custom configuration was correctly
  % set, attempt to start the JANET Simulator
  ok ->
   janet_start();
   
  % Otherwise, if there is an error in the custom
  % configuration, return it ({error,badarg})
  CustomConfError ->
   CustomConfError
 end;
   
% A non-map parameter was passed (print help information)
run(_InvalidParam) ->
 io:format("usage: run(#{sim_rest_port => int() >= 30000, remote_rest_client => list(), remote_rest_server_addr => list(), remote_rest_server_port => int() > 0}) (any)~n"),
 io:format("             - sim_rest_port:           The OS port to be used by the JANET Simulator REST server~n"),
 io:format("             - remote_rest_client:      The address of the remote client issuing REST requests to the JANET Simulator and Controller nodes~n"),
 io:format("             - remote_rest_server_addr: The address of the remote server accepting REST requests from the JANET Controller nodes~n"),
 io:format("             - remote_rest_server_port: The port of the remote server accepting REST requests from the JANET Controller nodes~n"), 
 io:format("~n"),
 {error,badarg}.


%% Attempts to set a custom configuration for the JANET Simulator application by overriding a subset of
%% its default configuration with the parameters passed in a map by the user (run(CfgMap) helper function)
set_custom_config(CfgMap) ->   
 
 % Pre-load the JANET Simulator application in the application
 % controller (this is required for setting a custom configuration)
 application:load(janet_simulator),
 
 % Convert the map containing the custom configuration parameters to
 % a list and attempt to override the default ones with their values 
 try override_default_config(maps:to_list(CfgMap))
 catch
 
  %% ---------------- Configuration Parameters Errors ---------------- %%
  
  % Integer parameter of invalid value
  {invalid_cfg,ParamName,out_of_range} ->
   io:format("<ERROR> Parameter \"~w\" is out of range~n",[ParamName]),
   {error,badarg};
 
  % Integer parameter is not an integer
  {invalid_cfg,ParamName,not_an_integer} ->
   io:format("<ERROR> Parameter \"~w\" is not an integer~n",[ParamName]),
   {error,badarg};
   
  % List parameter is not a list
  {invalid_cfg,ParamName,not_a_list} ->
   io:format("<ERROR> Parameter \"~w\" is not a string~n",[ParamName]),
   {error,badarg};

  % Unknown parameter
  {unknown_cfg,UnknownParamName} ->
   io:format("<ERROR> Unknown parameter \"~p\"~n",[UnknownParamName]),
   {error,badarg}
  
 end.


%% Overrides a subset of the JANET Simulator default configuration from a list of custom
%% configuration parameters (run(CfgMap) -> set_custom_config(CfgMap) helper function)
%%
%% NOTE: Host addresses (whether IP or names) are only checked to be lists (or strings)
%%

%% ------------------- All configuration parameters overridden successfully ------------------- %%
override_default_config([]) ->
 ok;

%% --------------------------------- 'sim_rest_port' parameter --------------------------------- %%
% Valid
override_default_config([{sim_rest_port,SimRESTPort}|NextCfgParam]) when is_integer(SimRESTPort), SimRESTPort >= 30000 ->
 application:set_env(janet_simulator,sim_rest_port,SimRESTPort),
 override_default_config(NextCfgParam);
 
% Invalid (integer < 30000)
override_default_config([{sim_rest_port,InvalidSimRESTPort}|_NextCfgParam]) when is_integer(InvalidSimRESTPort) ->
 throw({invalid_cfg,sim_rest_port,out_of_range});
 
% Invalid (not an integer)
override_default_config([{sim_rest_port,_InvalidSimRESTPort}|_NextCfgParam]) ->
 throw({invalid_cfg,sim_rest_port,not_an_integer});
 
%% ------------------------------ 'remote_rest_client' parameter ------------------------------ %%
% Valid
override_default_config([{remote_rest_client,RemoteRESTClient}|NextCfgParam]) when is_list(RemoteRESTClient) ->
 application:set_env(janet_simulator,remote_rest_client,RemoteRESTClient),
 override_default_config(NextCfgParam);
 
% Invalid (not a list)
override_default_config([{remote_rest_client,_InvalidRemoteRESTClient}|_NextCfgParam]) ->
 throw({invalid_cfg,remote_rest_client,not_a_list});

%% ---------------------------- 'remote_rest_server_addr' parameter ---------------------------- %%
% Valid
override_default_config([{remote_rest_server_addr,RemoteRESTServerAddr}|NextCfgParam]) when is_list(RemoteRESTServerAddr) ->
 application:set_env(janet_simulator,remote_rest_server_addr,RemoteRESTServerAddr),
 override_default_config(NextCfgParam);
 
% Invalid (not a list)
override_default_config([{remote_rest_server_addr,_InvalidRemoteRESTServerAddr}|_NextCfgParam]) ->
 throw({invalid_cfg,remote_rest_server_addr,not_a_list});


%% ---------------------------- 'remote_rest_server_port' parameter ---------------------------- %%
% Valid
override_default_config([{remote_rest_server_port,RemoteRESTServerPort}|NextCfgParam]) when is_integer(RemoteRESTServerPort), RemoteRESTServerPort > 0 ->
 application:set_env(janet_simulator,remote_rest_server_port,RemoteRESTServerPort),
 override_default_config(NextCfgParam);
 
% Invalid (integer =< 0)
override_default_config([{remote_rest_server_port,InvalidRemoteRESTServerPort}|_NextCfgParam]) when is_integer(InvalidRemoteRESTServerPort) ->
 throw({invalid_cfg,remote_rest_server_port,out_of_range});

% Invalid (not an integer)
override_default_config([{remote_rest_server_port,_InvalidRemoteRESTServerPort}|_NextCfgParam]) ->
 throw({invalid_cfg,remote_rest_server_port,not_an_integer});
  
%% ------------------------------ Unknown configuration parameter ------------------------------ %%
override_default_config([{UnknownParameter,_Value}|_NextCfgParam]) ->
 throw({unknown_cfg,UnknownParameter}).
 

%% Attempts to stop the JANET Simulator application and clears its Mnesia ram_copies tables (stop(), shutdown() helper function) 
janet_stop() ->

 try 
 
  % Ensure the JANET Simulator to be running
  ok = utils:ensure_jsim_state(running),
  
  % Attempt to stop the JANET Simulator application
  case application:stop(janet_simulator) of
   ok ->
	 
	% If the JANET Simulator was successfully stopped, clear its Mnesia ram_copies tables
    [{atomic,ok},{atomic,ok},{atomic,ok}] = [mnesia:clear_table(suploc),mnesia:clear_table(ctrmanager),mnesia:clear_table(devmanager)],
   
    % Attempt to unload the JANET Simulator application from the application controller
	% (this is a fix for allowing to load a custom configuration in later executions)
	case application:unload(janet_simulator) of
	 ok ->
	 
	  %% [TODO]: This sleep is for output ordering purposes (it will not be necessary once the primary logger level will be set to "warning")
      timer:sleep(5),                            
	
      % If it was successfully unloaded, report that the application has stopped
      io:format("Janet Simulator stopped~n");
	  
	 {error,UnloadReason} ->
	 
	  % If an error occured in unloaded the JANET Simulator, throw it
	  throw({error,{janet_unload,UnloadReason}})
	end;
	 
   {error,StopReason} ->
	
     % If an error occured  in stopping the JANET Simulator, throw it
	 throw({error,{janet_stop,StopReason}})
  end
  
 catch
 
  % If attempting to stop the JANET Simulator while it is not running, return the error
  {error,janet_not_running} ->
   {error,not_running};
 
  % If an error occured in stopping or unloading the JANET Simulator application, return it
  {error,Reason} ->
   {error,Reason}
   
 end.

%% ============================================ SUPPRESS LOGGER 'notice' MESSAGES ============================================ %%

%% E.g. applications stopping, superseded by configuring the 'logger_level' to 'warning' directly in the sys.config gile

logger:set_primary_config(#{level => warning}), %% (hides the == APPLICATION INFO === messages when supervisors stop components, uncomment before release)	

%% ===================================================== PING REMOTE HOST ===================================================== %%
os:cmd("ping -c 1 www.google.it >/dev/null ; echo $?",#{max_size => 1}).

- "0" -> found
- _ -> not_found  

%% ================================================ JANET SIMULATOR SLAVE DEBUG ================================================ %%

1>  net_kernel:start(['janet-simulator@localhost',shortnames]).
{ok,<0.142.0>}
(janet-simulator@localhost)2> erlang:set_cookie(node(),'janet-simulator').
true
(janet-simulator@localhost)3> slave:start_link("Helios","ctr-7","-setcookie janet-simulator").
{error,timeout}
(janet-simulator@localhost)4> slave:start_link("Helios","ctr-7","-setcookie janetsimulator"). 

User switch command
 --> q
(base) rickydenton@LinuxMintVB:~/Scrivania/rebar3slave$ rebar3 shell
===> Verifying dependencies...
===> Analyzing applications...
===> Compiling rebar3slave
Erlang/OTP 24 [erts-12.0] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [jit]

Eshell V12.0  (abort with ^G)
1> ===> Booted rebar3slave
 
1> net_kernel:start(['janet_simulator@localhost',shortnames]).
{ok,<0.142.0>}
(janet_simulator@localhost)2> erlang:set_cookie(node(),'janet_simulator').
true
(janet_simulator@localhost)3>  slave:start_link("Helios","ctr-7","-setcookie janet_simulator")
(janet_simulator@localhost)3> .

User switch command
 --> q
(base) rickydenton@LinuxMintVB:~/Scrivania/rebar3slave$ rebar3 shell
===> Verifying dependencies...
===> Analyzing applications...
===> Compiling rebar3slave
Erlang/OTP 24 [erts-12.0] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [jit]

Eshell V12.0  (abort with ^G)
1> ===> Booted rebar3slave
 
1> net_kernel:start([rebar3slave,shortnames]).
{ok,<0.142.0>}
(rebar3slave@LinuxMintVB)2> erlang:set_cookie(node(),'janet-simulator').
true
(rebar3slave@LinuxMintVB)3> erlang:get_cookie().
'janet-simulator'
(rebar3slave@LinuxMintVB)4> slave:start_link("Helios","ctr-6","-setcookie janet-simulator").
{ok,'ctr-6@Helios'}
(rebar3slave@LinuxMintVB)5>                                                                 
User switch command
 --> q
(base) rickydenton@LinuxMintVB:~/Scrivania/rebar3slave$ rebar3 shell
===> Verifying dependencies...
===> Analyzing applications...
===> Compiling rebar3slave
Erlang/OTP 24 [erts-12.0] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [jit]

Eshell V12.0  (abort with ^G)
1> ===> Booted rebar3slave
           
1> 
1> net_kernel:start(['janet-simulator',shortnames]).
{ok,<0.142.0>}
(janet-simulator@LinuxMintVB)2> erlang:set_cookie(node(),'janet-simulator').
true
(janet-simulator@LinuxMintVB)3> slave:start_link("Helios","ctr-7","-setcookie janet-simulator").


{ok,'ctr-7@Helios'}
(janet-simulator@LinuxMintVB)4> 
User switch command
 --> q
(base) rickydenton@LinuxMintVB:~/Scrivania/rebar3slave$ 


%% ========================================= OLD CONTROLLER AND DEVICE NODES NODEARGS ========================================= %%

%% Before $ERL_LIBS OS environment variable

% Controller
 NodeArgs = "-setcookie " ++ Loc_id_str ++ " -connect_all false -kernel net_ticktime 20 -pa _build/default/lib/janet_controller/ebin/ _build/default/lib/janet_simulator/ebin/ " ++
            "_build/default/lib/cowboy/ebin/ _build/default/lib/cowlib/ebin/ _build/default/lib/ranch/ebin/ _build/default/lib/jsone/ebin/ _build/default/lib/gun/ebin/",

% Device
 NodeArgs = "-setcookie " ++ Loc_id_str ++ " -connect_all false -kernel net_ticktime 20 -pa _build/default/lib/janet_device/ebin/ _build/default/lib/janet_simulator/ebin/",


%% ============================================= OLD START SCRIPT $REBAR3_ERL_ARGS ============================================= %%

REBAR3_ERL_ARGS="-sname janet-simulator -hidden -setcookie janet-simulator -kernel net_ticktime 20 -env ERL_LIBS _build/default/lib/ -run db" rebar3 shell



%% ==================================== MULTI-CONTINUE SLAVE:START_LINK ATTEMPT (ctrmanager) ==================================== %%

-include("janet_node_init.hrl").                % JANET nodes initialization record and constants
%% This record represents the state of a ctr_manager gen_server
-record(ctrmgrstate,    
        {
		 ctr_state,       % The state of the managed controller node ('booting' | 'connecting' | 'online')
		 ctr_node,        % The reference to the managed node
		 ctr_srv_pid,     % The PID of the controller's 'ctr_simserver' process
		 ctr_srv_mon,     % A reference used for monitoring the controller's 'ctr_simserver' process (and consequently the node)
		 loc_id,          % The controller's location ID
		 node_init        % The controller node #nodeinit initialization record ("janet_node_init.hrl")
		}).

%% ======================================================= HANDLE_CONTINUE ======================================================= %%
 
%% NOTE: The division of the managed node starting operations in two handle_continue() callback functions, as well as using a
%%       'node_init' initialization record defined in a separate "janet_node_init.hrl" header file is required to allow to the
%%       manager to receive messages (in particular, exit signals from their 'sup_loc' supervisors) while attempting to start its
%%       managed node, a task that cannot be delegated to a client 'node_starter' process due to the 'slave' library not offering
%%       the possibility of starting and linking a node to a process different than the caller


%% START_CONTROLLER_NODE
%% ---------------------
%% WHEN:      Right after the 'init' and the 'handle_info' CONTROLLER NODE DOWN callbacks
%% PURPOSE:   Start the attempts to create the managed controller node and link it to the manager
%% ACTIONS:   1) Prepare the controller node #nodeinit initialization record
%%            2) Call the START_CONTROLLER_NODE_ATTEMPT handle_continue() callback function
%%               for attempting to start the controller node and link it to the manager
%% NEW STATE: Set the controller node initialization record
%%  
handle_continue(start_controller_node,SrvState=#ctrmgrstate{loc_id=Loc_id}) ->
 
 % Retrieve the name of the nodes host where the controller must be deployed in
 {ok,LocationRecord} = db:get_record(location,Loc_id),
 NodeHost = utils:get_effective_hostname(LocationRecord#location.hostname),
 
 % Define the the controller node name by concatenating
 % its location ID to a the constant "loc-" string
 Loc_id_str = integer_to_list(Loc_id),
 NodeName = "ctr-" ++ Loc_id_str,
 
 % Prepare the controller node VM arguments as follows:
 %
 % - Set its cookie to its location ID
 % - Prevent transitive connections between nodes
 % - Align the net_kernel ticktime to the one used by the JANET Simulator (20 seconds)
 % - Set the $ERL_LIBS OS environment variable to build directory to allow
 %   it to find the 'janet_controller' application resource file and bytecode
 %
 %   NOTE: This applies only to nodes deployed in the localhost, while remote nodes
 %         are supposed to have the application resource files and bytecode placed
 %         in the Erlang installation directory (default: "/usr/lib/erlang/lib")
 %
 NodeArgs = "-setcookie " ++ Loc_id_str ++ " -connect_all false -kernel net_ticktime 20 -env ERL_LIBS _build/default/lib/",
 
 % Set the cookie provided by the JANET Simulator to the controller node to its location ID
 erlang:set_cookie(list_to_atom(NodeName ++ "@" ++ NodeHost),list_to_atom(Loc_id_str)),
 
 % Initialize the map containing information required for starting the JANET Controller application with:
 %
 % - The port to be used by the controller's REST server
 % - The user the location belongs to
 %
 AppInfo = #{ctr_rest_port => LocationRecord#location.port, loc_user => LocationRecord#location.user},
 
 % Build the node initialization record with the previous information
 CtrInitRecord = #nodeinit{node_host = NodeHost, node_name = NodeName, node_args = NodeArgs,
                            app_info = AppInfo, attempts_left = ?Node_start_max_attempts},
 
 io:format("[ctr_mgr-~w]: In start_controller_node~n",[Loc_id]),

 % Set the controller node initialization record in the server state and call the START_CONTROLLER_NODE_ATTEMPT handle
 % handle_continue() callback for performing the first attempt to start the controller node and link it to the manager 
 {noreply,SrvState#ctrmgrstate{node_init = CtrInitRecord},{continue,start_controller_node_attempt}};
 
 
%% START_CONTROLLER_NODE_ATTEMPT
%% -----------------------------
%% WHEN:      Right after the START_CONTROLLER_NODE and between consecutive START_CONTROLLER_NODE_ATTEMPT handle_continue() callbacks
%% PURPOSE:   Attempt to start the controller node and link it with the manager
%% ACTIONS:   1) Attempt to start the controller node and link it with the manager
%%              A) Node successfully started
%%                2) Retrieve the information required for starting the JANET Controller application on the node
%%                3) Launch the JANET Controller application on the controller node
%%              B) Error in starting the node, and there are start attempts left
%%                2) Report the error in starting the node
%%                3) If the error Reason =/= 'timeout, wait for a predefined time before the next attempt
%%                4) Decrement the number of attempts left and call again this callback functionn
%%              C) Error in starting the node and there are NO start attempts left
%%                2) Report the error in starting the node and that the manager is stopping
%%                3) Stop the manager with reason 'normal' for preventing the 'sup_loc' supervisor from attempting to respawn
%%                   it (which would be useless given that in the current situation its managed node cannot be created) 
%% NEW STATE:   A) Set the 'ctr_node' and clear the 'node_init' state variables
%%              B) Decrement the number of attempts left in the controller initialization record
%%              C) - 
%%  
handle_continue(start_controller_node_attempt,SrvState=#ctrmgrstate{loc_id = Loc_id, node_init =
                                              CtrInitRecord =#nodeinit{node_host = NodeHost, node_name = NodeName, node_args = NodeArgs,
											                            app_info = AppInfo, attempts_left = AttemptsLeft}}) ->
						
 io:format("[ctr_mgr-~w]: In start_controller_node_attempt~n",[Loc_id]),
						
 % Attempt to start the controller node and link it to the manager
 StartRes = slave:start_link(NodeHost,NodeName,NodeArgs),
 
 % Depending on the result of the operation
 % and whether there are start attempts left
 case {StartRes,AttemptsLeft} of 
 
  %% ---------------------------------- NODE START SUCCESS ---------------------------------- %%
  {{ok,CtrNode},_} ->
  
   io:format("[ctr_mgr-~w]: NODE START SUCCESS ~n",[Loc_id]),
  
   % If the controller was successfully started, retrieve the information required
   % for starting the JANET Controller application from the 'app_info' map
   #{ctr_rest_port := CtrRESTPort, loc_user := Loc_user} = AppInfo,
   
   % Derive the initial contents of the controller
   % 'ctr_sublocation' and 'ctr_device' tables
   {ok,CtrSublocTable,CtrDeviceTable} = prepare_ctr_tables(Loc_id),
 
   % Retrieve the remote REST server address and port
   {ok,RemoteRESTServerAddr} = application:get_env(remote_rest_server_addr), % The address of the remote server accepting REST requests from the controller (a list)  
   {ok,RemoteRESTServerPort} = application:get_env(remote_rest_server_port), % The port of the remote server accepting REST requests from the controller (int > 0)

   % Attempt to launch the JANET Controller application on the controller node
   ok = rpc:call(CtrNode,jctr,run,[Loc_id,CtrSublocTable,CtrDeviceTable,self(),CtrRESTPort,RemoteRESTServerAddr,RemoteRESTServerPort,Loc_user]),
 
   % Set the 'ctr_node' and clear the 'node_init' state variables and wait for
   % the registration request of the controller's 'ctr_simserver' process
   {noreply,SrvState#ctrmgrstate{ctr_node = CtrNode, node_init = none}};
   
  %% -------------------------- NODE START ERROR (no attempts left) -------------------------- %%
  {{error,Reason},0} ->
  
   io:format("[ctr_mgr-~w]: NODE START ERROR (no attempts left) ~n",[Loc_id]),
   
   % If an error occured in starting the controller node and there are no
   % attempts left, report the error and inform that the manager is stopping
   io:format("[ctr_mgr-~w]: <ERROR> The controller node could not be started (reason = ~p) and the maximum number of start attempts has been reached, the manager will now stop~n",[Loc_id,Reason]),
   
   % Stop the manager with reason 'normal' for preventing the 'sup_loc' supervisor from attempting to respawn
   % it (which would be useless given that in the current situation its managed node cannot be created) 
   {stop,normal,SrvState};
  
  %% ------------------------- NODE START ERROR (with attempts left) ------------------------- %% 
  {{error,Reason},AttemptsLeft} ->
  
   io:format("[ctr_mgr-~w]: NODE START ERROR (~n - 1 attempts left) ~n",[Loc_id,AttemptsLeft]),
  
   % If an error occured in starting the controller node and there are attempts left, report the error
   % and, if its Reason =/= 'timeout', wait for an added delay before performing the next attempt 
   case Reason of
   
    % Node host timeout (retry with no delay)
    timeout ->
	 io:format("[ctr_mgr-~w]: <WARNING> Timeout in connecting with the controller node's host, retrying...~n",[Loc_id]);
	 
	% A previous instance of the controller node (NodeName@NodeHost) is still running (retry with delay)
    {already_running,_CtrNode} ->
	 io:format("[ctr_mgr-~w]: <ERROR> A previous instance of the controller node is still running, retrying in ~w seconds...~n",[Loc_id,?Nodes_start_attempts_delay_sec]),
	 timer:sleep(?Nodes_start_attempts_delay_sec * 1000);
	 
	% No 'ssh' program was found in the remote host (retry with delay)
    no_rsh ->
	 io:format("[ctr_mgr-~w]: <ERROR> No 'ssh' program was found in the controller node's host, retrying in ~w seconds...~n",[Loc_id,?Nodes_start_attempts_delay_sec]),
	 timer:sleep(?Nodes_start_attempts_delay_sec * 1000)
   end,
   
   % Decrement the number of attempts left in the node initialization record and perform the next node start attempt
   {noreply,SrvState#ctrmgrstate{node_init = CtrInitRecord#nodeinit{attempts_left = AttemptsLeft-1}},{continue,start_controller_node_attempt}}
 end.


%% ================================================= JCTR run() arguments debug ================================================= %%
 Cookie = erlang:get_cookie(),
 NetTickTime = net_kernel:get_net_ticktime(),
 
 io:format("~nJCTR~n"),
 io:format("====~n"),
 io:format("Cookie = ~w~n",[Cookie]),
 io:format("net_ticktime = ~w~n",[NetTickTime]),
 io:format("Loc_id = ~w~n",[Loc_id]),
 io:format("CtrSublocTable = ~0p~n",[CtrSublocTable]),
 io:format("CtrDeviceTable = ~0p~n",[CtrDeviceTable]),
 io:format("MgrPid = ~w~n",[MgrPid]),
 io:format("CtrRESTPort = ~w~n",[CtrRESTPort]),
 io:format("RemoteRESTServerAddr = ~s~n",[RemoteRESTServerAddr]),
 io:format("RemoteRESTServerPort = ~w~n",[RemoteRESTServerPort]),
 io:format("Loc_user = ~s~n~n",[Loc_user]),


