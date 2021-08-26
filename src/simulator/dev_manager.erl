%% This module represents a device's manager in the Janet Simulator application %%

-module(dev_manager).
-behaviour(gen_server).

-export([start_link/2,init/1,terminate/2,handle_call/3,handle_cast/2,handle_continue/2]).  % gen_server Behaviour Callback Functions

-include("table_records.hrl").  % Mnesia Table Records Definitions

%% This record represents the state of a dev_manager gen_server
-record(devmgrstate,    
        {
		 dev_state,       % The state of the managed device node
		 dev_node,        % The reference to the managed node
		 dev_srv_pid,     % The PID of the device's dev_server
		 dev_statem_pid,  % The PID of the device's state machine
		 dev_id,          % The device's ID
		 loc_id           % The device location's ID
		}).

%%====================================================================================================================================
%%                                                  GEN_SERVER CALLBACK FUNCTIONS                                                        
%%====================================================================================================================================

%% ============================================================ INIT ============================================================ %%
init({Dev_id,Loc_id}) ->

 % Trap exit signals so to allow cleanup operation at shutdown in the terminate(shutdown,State) callback function
 process_flag(trap_exit,true),
 
 % Register the manager in the 'devmanager' table
 {atomic,ok} = mnesia:transaction(fun() -> mnesia:write(#devmanager{dev_id=Dev_id,loc_id=Loc_id,sup_pid=self(),status="BOOTING"}) end),
 
 % The device node's inizialization will continue in the handle_continue(Continue,State) callback function for parallelization purposes  
 {ok,#devmgrstate{dev_state=booting,dev_node=none,dev_srv_pid=none,dev_statem_pid=none,dev_id=Dev_id,loc_id=Loc_id},{continue,init}}.
 
 
%% ======================================================= HANDLE_CONTINUE ======================================================= %%
  
%% Initializes the device's node (called after the 'init' callback function)
handle_continue(init,SrvState) ->
 
 %% ---------------- Device Node Configuration Parameters Definition ---------------- %%
 
 % Retrieve the Dev_id and the Loc_id and convert them to strings
 Dev_id = SrvState#devmgrstate.dev_id,
 Loc_id = SrvState#devmgrstate.loc_id,
 Dev_id_str = integer_to_list(Dev_id),
 Loc_id_str = integer_to_list(Loc_id),
 
 % Retrieve the device record
 {ok,DeviceRecord} = db:get_record(device,Dev_id),
 
 % Check the record validity
 %% [TODO]: This is probably is not required, since if the record is corrupted the function crashes anyway (and it just checks for it to be in the form {device,...}
 % true = is_record(DeviceRecord,device),
 
 % Retrieve the device's type and configuration
 Type = DeviceRecord#device.type,
 Config = DeviceRecord#device.config,
 
 %% ------------------------------ Device Node Creation ------------------------------ %% 
 
 % Set the cookie for allowing the Janet Simulator to connect with the device's node
 % NOTE: The use of atoms is required by the erlang:set_cookie BIF  
 erlang:set_cookie(utils:str_to_atom("dev-" ++ Dev_id_str ++ "@localhost"),utils:str_to_atom(Loc_id_str)),
 
 % Prepare the Host, Name and Args parameters of the controller's node
 NodeHost = "localhost",
 NodeName = "dev-" ++ Dev_id_str,
 NodeArgs = "-setcookie " ++ Loc_id_str ++ " -connect_all false -pa ebin/",
 
 % Instantiate the controller's node and link it to the manager
 {ok,Node} = slave:start_link(NodeHost,NodeName,NodeArgs),

 % Launch the Janet Device application on the controller node
 ok = rpc:call(Node,jdev,run,[Dev_id,Loc_id,self(),Type,Config]),
 
 % Return with the server initial state, awaiting the registration
 % messages of the dev_server and dev_statem processes
 {noreply,SrvState#devmgrstate{dev_node = Node}}.
  
 
%% ========================================================== TERMINATE ========================================================== %% 

%% Called when the manager is asked to shutdown by its 'sup_loc' supervisor 
terminate(shutdown,SrvState) ->
  
 % Deregister the manager from the 'devmanager' table
 {atomic,ok} = mnesia:transaction(fun() -> mnesia:write(#devmanager{dev_id=SrvState#devmgrstate.dev_id,loc_id=SrvState#devmgrstate.loc_id,sup_pid='-',status="STOPPED"}) end),
 
 % Return (note that the device's node will be automatically terminated since, being
 % it linked with the manager, will receive an exit signal with reason 'shutdown')
 ok.


%% ========================================================= HANDLE_CALL ========================================================= %% 

%% ----------------------------------------------------------- BOOTING ----------------------------------------------------------- %%

%% SENDER:    The device's dev_server
%% WHEN:      During the dev_server initialization (handle_continue(init,_))
%% PURPOSE:   Device Registration Request
%% CONTENTS:  The dev_server and dev_statem PIDs
%% MATCHES:   When the device is booting
%% ACTIONS:   Update the device's state to "CONNECTING" in the 'devmanager' table
%% ANSWER:    'ok' (the device registration was successful)
%% NEW STATE: Set the 'dev_srv_pid' and 'dev_statem_pid' and update the 'dev_state' to 'connecting'
%%
%% NOTE:      This message can be received only once, since if the dev_server process crashes, the entire
%%            device node is shut down by its application master (The Janet Device is a permanent application)
%%
handle_call({dev_reg,DevSrvPid,DevStatemPid},{DevSrvPid,_},SrvState) when SrvState#devmgrstate.dev_state =:= booting andalso node(DevSrvPid) =:= SrvState#devmgrstate.dev_node ->

 % Update the device's state to "CONNECTING" in the 'devmanager' table
 {atomic,ok} = mnesia:transaction(fun() -> mnesia:write(#devmanager{dev_id=SrvState#devmgrstate.dev_id,loc_id=SrvState#devmgrstate.loc_id,sup_pid=self(),status="CONNECTING"}) end),
 
 % Log that the device has successfully booted
 io:format("[devmgr-" ++ integer_to_list(SrvState#devmgrstate.dev_id) ++ "]: Device node successfully booted~n"),
 
 % Confirm the device registration and update the 'dev_srv_pid', 'dev_statem_pid' and the 'dev_state'
 {reply,ok,SrvState#devmgrstate{dev_state = connecting, dev_srv_pid = DevSrvPid, dev_statem_pid = DevStatemPid}}; 


%% --------- STUB
handle_call(Num,_,{Sum,N}) when is_number(Num) ->
 New_Sum = Sum + Num,
 New_N = N+1,
 {reply,New_Sum/New_N,{New_Sum,New_N}}.


%% ========================================================= HANDLE_CAST ========================================================= %% 


%% --------- STUB
handle_cast(reset,State) -> % Resets the server State
 {noreply,State}.


%%====================================================================================================================================
%%                                                         START FUNCTION                                                        
%%====================================================================================================================================

%% Called by its 'sup_loc' location supervisor whenever a new device is started in the location, which may happen:
%%  - At boot time by the location devices' initializer (locs_devs_init)
%%  - Every time a new device is added to the database in such location ([TODO]: Insert function name here)
start_link(Dev_id,Loc_id) ->
 gen_server:start_link(?MODULE,{Dev_id,Loc_id},[]).