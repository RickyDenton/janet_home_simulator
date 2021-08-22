%% This module represents a device's manager in the Janet Simulator application %%

-module(dev_manager).
-behaviour(gen_server).

-export([start_link/2,init/1,terminate/2,handle_call/3,handle_cast/2,handle_continue/2]).  % gen_server Behaviour Callback Functions

-include("table_records.hrl").  % Mnesia Table Records Definitions

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
 {ok,{booting,none,Dev_id,Loc_id},{continue,init}}.
 

%% ======================================================= HANDLE_CONTINUE ======================================================= %%
  
%% Initializes the device's node (called after the 'init' callback function)
handle_continue(init,{booting,none,Dev_id,Loc_id}) ->
 
 %% ---------------- Device Node Configuration Parameters Definition ---------------- %%
 
 % Convert the Dev_id and the Loc_id to string
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
 
 % Update the controller status in the 'devmanager' table to 'CONNECTING'
 {atomic,ok} = mnesia:transaction(fun() -> mnesia:write(#devmanager{dev_id=Dev_id,loc_id=Loc_id,sup_pid=self(),status="CONNECTING"}) end),
 
 % Return the updated manager's state
 {noreply,{online,Node,{Dev_id,Loc_id}}}. 
 
 
%% ========================================================== TERMINATE ========================================================== %% 

%% Called when the manager is asked to shutdown by its 'sup_loc' supervisor 
terminate(shutdown,{_,_,{Dev_id,Loc_id}}) ->
 
 % Deregister the manager from the 'devmanager' table
 {atomic,ok} = mnesia:transaction(fun() -> mnesia:write(#devmanager{dev_id=Dev_id,loc_id=Loc_id,sup_pid='-',status="STOPPED"}) end),
 
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