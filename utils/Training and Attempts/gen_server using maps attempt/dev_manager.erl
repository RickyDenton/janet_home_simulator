%% This module represents a device's manager in the Janet Simulator application %%

-module(dev_manager).
-behaviour(gen_server).

-export([start_link/2,init/1,terminate/2,handle_call/3,handle_cast/2,handle_continue/2]).  % gen_server Behaviour Callback Functions

-include("sim_mnesia_tables_definitions.hrl").  % Janet Simulator Mnesia Tables Records Definitions

%%====================================================================================================================================
%%                                                  GEN_SERVER CALLBACK FUNCTIONS                                                        
%%====================================================================================================================================

%% ============================================================ INIT ============================================================ %%
init({Dev_id,Loc_id}) ->

 % Trap exit signals so to allow cleanup operation at shutdown in the terminate(shutdown,State) callback function
 process_flag(trap_exit,true),
 
 % Register the manager in the 'devmanager' table
 {atomic,ok} = mnesia:transaction(fun() -> mnesia:write(#devmanager{dev_id=Dev_id,loc_id=Loc_id,mgr_pid=self(),status="BOOTING"}) end),
 
 % The device node's inizialization will continue in the handle_continue(Continue,State) callback function for parallelization purposes 
 % {ok,{booting,{none,none,none},{Dev_id,Loc_id}},{continue,init}}.
 
 {ok,#{devstate => booting, devref => #{devnode => none,devserver => none,devfsm => none}, mgrref => #{dev_id => Dev_id, loc_id => Loc_id}},{continue,init}}.
 
 
%% ======================================================= HANDLE_CONTINUE ======================================================= %%
  
%% Initializes the device's node (called after the 'init' callback function)
% handle_continue(init,{booting,{none,none,none},{Dev_id,Loc_id}}) ->
handle_continue(init,SrvState) ->
 %% ---------------- Device Node Configuration Parameters Definition ---------------- %%
 
 % Convert the Dev_id and the Loc_id to string
 #{mgrref := #{dev_id := Dev_id, loc_id := Loc_id}} = SrvState,
 Dev_id_str = integer_to_list(Dev_id),
 Loc_id_str = integer_to_list(Loc_id),
 
 % Retrieve the device record
 {ok,DeviceRecord} = db:get_record(device,Dev_id),
 
 % Check the record validity
 %% [NOTE]: This is probably is not required, since if the record is corrupted the function crashes anyway (and it just checks for it to be in the form {device,...}
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
 
 
 
 
 % Return the updated manager's state
 %{noreply,{booting,{Node,none,none},{Dev_id,Loc_id}}}. 
 
 {noreply,SrvState#{devref := #{devnode => Node,devserver => none,devfsm => none}}}.
 
 
 
 
%% ========================================================== TERMINATE ========================================================== %% 

%% Called when the manager is asked to shutdown by its 'sup_loc' supervisor 
terminate(shutdown,SrvState) ->
 
 #{mgrref := #{dev_id := Dev_id, loc_id := Loc_id}} = SrvState,
 
 % Deregister the manager from the 'devmanager' table
 {atomic,ok} = mnesia:transaction(fun() -> mnesia:write(#devmanager{dev_id=Dev_id,loc_id=Loc_id,mgr_pid='-',status="STOPPED"}) end),
 
 % Return (note that the device's node will be automatically terminated since, being
 % it linked with the manager, will receive an exit signal with reason 'shutdown')
 ok.


%% ========================================================= HANDLE_CALL ========================================================= %% 

%% --------- STUB
handle_call(Num,_,{Sum,N}) when is_number(Num) ->
 New_Sum = Sum + Num,
 New_N = N+1,
 {reply,New_Sum/New_N,{New_Sum,New_N}}.


%% ========================================================= HANDLE_CAST ========================================================= %% 

handle_cast({dev_server_pid,DevSrvPid},SrvState) ->

%handle_cast({dev_server_pid,DevSrvPid},{MgrState,{Node,_,DevStatemPid},{Dev_id,Loc_id}}) ->
 
 #{devstate := DevState, devref := #{devfsm := DevStatemPid}} = SrvState,
 
 
 case DevState of
  
  booting ->
   
   case DevStatemPid of
   
   
    % We need to wait for the other
    none ->
	 #{devref := OldDevRef} = SrvState,
	 {noreply,SrvState#{devref := OldDevRef#{devserver := DevSrvPid}}};
	 
	% The device has terminated booting, and is now attempting to register within the controller
	_ ->
     % Update the controller status in the 'devmanager' table to 'CONNECTING'
	 #{mgrref := #{dev_id := Dev_id, loc_id := Loc_id}} = SrvState,
     {atomic,ok} = mnesia:transaction(fun() -> mnesia:write(#devmanager{dev_id=Dev_id,loc_id=Loc_id,mgr_pid=self(),status="CONNECTING"}) end),
	 io:format("[devmgr-" ++ integer_to_list(Dev_id) ++ "]: Device node successfully booted (last was the DevSrvPid)~n"),
     
	 #{devref := OldDevRef} = SrvState,
	 
	 NewSrvState = SrvState#{devstate := connecting, devref := OldDevRef#{devserver := DevSrvPid}},
	 
	 io:format("AFTER CONNECTED: NewSrvState = ~w~n",[NewSrvState]),
	 
	 {noreply,NewSrvState}
	 
	 
	 
	% {noreply,{connecting,{Node,DevSrvPid,DevStatemPid},{Dev_id,Loc_id}}}
	 
   end;
 
  % Just a random state update (probably the dev_server crashed and was restarted)
  _ ->
  
  
     #{devref := OldDevRef} = SrvState,
	 {noreply,SrvState#{devref := OldDevRef#{devserver := DevSrvPid}}}
	
   %{noreply,{MgrState,{Node,DevSrvPid,DevStatemPid},{Dev_id,Loc_id}}}
 end;
 
 
%handle_cast({dev_statem_pid,DevStatemPid},{MgrState,{Node,DevSrvPid,_},{Dev_id,Loc_id}}) ->
handle_cast({dev_statem_pid,DevStatemPid},SrvState) ->
 
 
 #{devstate := DevState, devref := #{devserver := DevSrvPid}} = SrvState,
 
 case DevState of
  
  booting ->
   
   case DevSrvPid of
   
    % We need to wait for the other
    none ->
	
     #{devref := OldDevRef} = SrvState,
	 {noreply,SrvState#{devref := OldDevRef#{devfsm := DevStatemPid}}};
	 
	 %{noreply,{booting,{Node,DevSrvPid,DevStatemPid},{Dev_id,Loc_id}}};
	 
	% The device has terminated booting, and is now attempting to register within the controller
	_ ->
     % Update the controller status in the 'devmanager' table to 'CONNECTING'
	 #{mgrref := #{dev_id := Dev_id, loc_id := Loc_id}} = SrvState,
	 
     {atomic,ok} = mnesia:transaction(fun() -> mnesia:write(#devmanager{dev_id=Dev_id,loc_id=Loc_id,mgr_pid=self(),status="CONNECTING"}) end),
	 io:format("[devmgr-" ++ integer_to_list(Dev_id) ++ "]: Device node successfully booted (last was the DevStatemPid)~n"),
    

	 #{devref := OldDevRef} = SrvState,
	 
	 NewSrvState = SrvState#{devstate := connecting, devref := OldDevRef#{devfsm := DevStatemPid}},
	 
	 io:format("AFTER CONNECTED: NewSrvState = ~w~n",[NewSrvState]),
	 
	 {noreply,NewSrvState}

	%{noreply,{connecting,{Node,DevSrvPid,DevStatemPid},{Dev_id,Loc_id}}}
	 
   end;
 
  % Just a random state update (probably the dev_statem crashed and was restarted)
  _ ->
  
   #{devref := OldDevRef} = SrvState,
   {noreply,SrvState#{devref := OldDevRef#{devfsm := DevStatemPid}}}
  
   %{noreply,{MgrState,{Node,DevSrvPid,DevStatemPid},{Dev_id,Loc_id}}}
 end. 
 

%%====================================================================================================================================
%%                                                         START FUNCTION                                                        
%%====================================================================================================================================

%% Called by its 'sup_loc' location supervisor whenever a new device is started in the location, which may happen:
%%  - At boot time by the location devices' initializer (locs_devs_init)
%%  - Every time a new device is added to the database in such location ([TODO]: Insert function name here)
start_link(Dev_id,Loc_id) ->
 gen_server:start_link(?MODULE,{Dev_id,Loc_id},[]).