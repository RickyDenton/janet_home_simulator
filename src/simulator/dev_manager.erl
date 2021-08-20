-module(dev_manager).
-behaviour(gen_server).
-export([start_link/1,init/1,terminate/2,handle_call/3,handle_cast/2,handle_continue/2]).

-include("table_records.hrl").   % Mnesia table records definition


%% ================================================ GEN_SERVER CALLBACK FUNCTIONS ================================================ %%
 

init(DeviceRecord) ->

 % Trap Exit signals
 process_flag(trap_exit,true),
 
 % Register the device's manager in the devmanager table
 {atomic,ok} = mnesia:transaction(fun() -> mnesia:write(#devmanager{dev_id=DeviceRecord#device.dev_id,sup_pid=self(),status="BOOTING"}) end),
 
 % The device node initialization will continue in the handle_continue(Continue,State) callback for parallelization purposes 
 {ok,{booting,none,DeviceRecord},{continue,init}}.
 
  
%% Continues with the initialization
handle_continue(init,{booting,none,DeviceRecord}) ->
 
 % Retrieve the Dev_id and the Loc_id, both as a number and as a string
 {Loc_id,_} = DeviceRecord#device.sub_id,
 Loc_id_str = integer_to_list(Loc_id),
 Dev_id =  DeviceRecord#device.dev_id,
 Dev_id_str = integer_to_list(Dev_id),
 
 % Retrieve the device's type and configuration
 Type = DeviceRecord#device.type,
 Config = DeviceRecord#device.config,
 
 % Set the cookie for connecting to the target node (NOTE: the use of atoms is required by the erlang:set_cookie BIF) 
 erlang:set_cookie(utils:str_to_atom("dev-" ++ Dev_id_str ++ "@localhost"),utils:str_to_atom(Loc_id_str)),
 
 % Prepare the controller's node Name, Host and VM arguments
 NodeHost = "localhost",
 NodeName = "dev-" ++ Dev_id_str,
 NodeArgs = "-setcookie " ++ Loc_id_str ++ " -connect_all false -pa ebin/",
 
 % Instantiate and link to the slave device node
 {ok,Node} = slave:start_link(NodeHost,NodeName,NodeArgs),

 % Launch the device application on the slave node
 ok = rpc:call(Node,jdev,run,[Dev_id,Loc_id,self(),Type,Config]),
 
 % Set the device's status in the ctrmanager table to "CONNECTING"
 {atomic,ok} = mnesia:transaction(fun() -> mnesia:write(#devmanager{dev_id=Dev_id,sup_pid=self(),status="CONNECTING"}) end),
 
 io:format("[dev_manager ~w]: Device node successfully initialized~n",[Dev_id]),
 
 {noreply,{online,Node,{Dev_id,Loc_id}}}. 
 
 
 
 
 
 
 


%% --------- STUB
terminate(normal,_) ->
 io:format("[dev_manager ]: Terminated").

%% --------- STUB
handle_call(Num,_,{Sum,N}) when is_number(Num) ->
 New_Sum = Sum + Num,
 New_N = N+1,
 {reply,New_Sum/New_N,{New_Sum,New_N}}.

%% --------- STUB
handle_cast(reset,State) -> % Resets the server State
 {noreply,State}.


%% ======================================================== START FUNCTION ======================================================== %%

%% Called by the associated "loc_sup" when starting the device's manager  
start_link(DeviceRecord) ->
 gen_server:start_link(?MODULE,DeviceRecord,[]).