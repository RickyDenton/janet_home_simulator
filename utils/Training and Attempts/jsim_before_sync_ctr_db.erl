%% This is the callback module of the JANET Simulator (janet_simulator) application %%

-module(jsim).
-behaviour(application). 

%% ---------------------------------- JANET SIMULATOR RUN AND STOP ---------------------------------- %%
-export([run/0,run/2,stop/0,shutdown/0]).

%% ---------------------------------- DATABASE INTERFACE FUNCTIONS ---------------------------------- %%
-export([add_location/4,add_sublocation/2,add_device/4]).            % Create
-export([print_table/0,print_table/1,print_tree/0,                   % Read
         print_tree/1,print_tree/2,get_record/2]).
-export([update_dev_subloc/2,update_loc_name/2,                      % Update
         update_subloc_name/2,update_dev_name/2]).                                
-export([delete_location/1,delete_sublocation/1,delete_device/1]).   % Delete

%% ---------------------------------- JANET NODES STOP AND RESTART ---------------------------------- %%
-export([stop_node/2,restart_node/2]).                               % Per-node stop/restart
-export([stop_subloc/1,restart_subloc/1]).                           % Per-sublocation stop/restart
-export([stop_loc/1,restart_loc/1]).                                 % Per-location stop/restart
-export([stop_all_nodes/0,restart_all_nodes/0]).                     % All-nodes stop/restart

%% ---------------------------------- SIMULATION UTILITY FUNCTIONS ---------------------------------- %%
-export([print_nodes/0,print_nodes/1]).                              % Running and stopped nodes info
-export([print_ctr_table/1,print_ctr_table/2,                        % Controller Nodes interaction
         print_ctr_tree/1,ctr_command/4]).   
-export([dev_config_change/2,dev_command/4]).                        % Device Nodes interaction
	  
%% ---------------------------- APPLICATION BEHAVIOUR CALLBACK FUNCTIONS ---------------------------- %%
-export([start/2,stop/1]). 		    

-include("sim_mnesia_tables_definitions.hrl").  % Janet Simulator Mnesia Tables Records Definitions

%%====================================================================================================================================
%%                                                  JANET SIMULATOR RUN AND STOP                                                       
%%====================================================================================================================================

%% DESCRIPTION:  Prepares the configuration parameters and starts the JANET Simulator application
%%
%% ARGUMENTS:    - RestPort:   The port that will be used by the JANET Simulator for binding its REST server on the host OS
%%                             (must be >=30000 for preventing port allocation conflicts)
%%               - RemoteHost: The IP address of the host where JANET controllers will forward state updates
%%               - ():         A default RestPort and RemoteHost are used (testing purposes olny) 
%%
%% RETURNS:      - ok                      -> JANET Simulator succesfully started
%%               - {error,already_running} -> The janet_simulator application is already running on the node
%%               - {error,Reason}          -> Internal error in starting the application
%%               - {error,badarg}          -> Invalid arguments
%%
run(RestPort,RemoteHost) when is_number(RestPort), RestPort>=30000 ->

 % Check if the JANET Simulator is already running
 case utils:is_running(janet_simulator) of
  true ->
  
   % If it is, return an error
   {error,already_running};
   
  false ->
  
   % Otherwise, initialize the JANET Simulator configuration parameters as for the arguments
   application:set_env(janet_simulator,rest_port,RestPort),
   application:set_env(janet_simulator,remotehost,RemoteHost),
   
   % Ensure the Mnesia database to be running
   case db:start_mnesia() of
 
    ok ->
     % If Mnesia is running, start the JANET Simulator and its tables
     %% [TODO]: logger:set_primary_config(#{level => warning}),  (hides the == APPLICATION INFO === messages when supervisors stop components, uncomment before release)	
     application:start(janet_simulator);
	 
	{error,_} ->
     % Otherwise notify that the JANET Simulator cannot be started
     io:format("Mnesia is required for running the JANET Simulator~n")
	 
   end
 end;

%% Invalid function invocations (print help messages)
run(RestPort,_) when is_number(RestPort) ->
 io:format("Please use a port of value >= 30000 for the Simulator rest server for preventing port allocation conflicts on the host OS~n"),
 {error,badarg};
run(_,_) ->
 io:format("usage: run(RestPort,RemoteHost) (Port >= 30000)~n"),
 {error,badarg}.
 
%% Start the JANET Simulator with the default configuration (testing purposes olny) 
run() ->
 run(55555,"somehost.com:1240").


%% DESCRIPTION:  Stops the JANET Simulator
%%
%% ARGUMENTS:    none 
%%
%% RETURNS:      - ok                  -> JANET Simulator succesfully stopped
%%               - {error,not_running} -> The JANET Simulator is not running on the node
%%               - {error,Reason}      -> Internal error in stopping the application
%%
stop() ->

 % Check if the JANET Simulator is running
 case utils:is_running(janet_simulator) of
  false ->
  
   % If it is not, return an error
   {error,not_running};
  
  true ->
  
   % Otherwise, attempt to stop the JANET Simulator
   StopSimStatus = application:stop(janet_simulator),
   case StopSimStatus of
    ok ->
	 
	 % If stopped, clear all Mnesia ram_copies tables and report the operation
     [{atomic,ok},{atomic,ok},{atomic,ok}] = 
	  [mnesia:clear_table(suploc),mnesia:clear_table(ctrmanager),mnesia:clear_table(devmanager)],
	 timer:sleep(5),                            %% [TODO]: This sleep is for output ordering purposes (it will not be necessary once the primary logger level will be set to "warning")
     io:format("Janet Simulator stopped~n");
	 
	{error,Reason} ->
	 
	 % Otherwise, notify the error
     io:format("Error in stopping the Janet Simulator (reason = ~w)~n",[Reason])
   end,
   StopSimStatus
 end.


%% DESCRIPTION:  Stops the Janet Simulator and Mnesia application, as well as the erlang node
%%
%% ARGUMENTS:    none 
%%
%% RETURNS:      - ok -> JANET Simulator node succesfully stopped
%% 
shutdown() ->
 
 % Attempt to stop both the Janet Simulator and Mnesia applications
 stop(),
 
 % Shut down the node
 init:stop("shutdown").


%%====================================================================================================================================
%%                                                  DATABASE INTERFACE FUNCTIONS
%%====================================================================================================================================

%% ========================================================== CREATE ===============================================================%%

%% DESCRIPTION:  Adds an empty location to the database with its (default) sublocation {Loc_id,0}, and starts up its controller
%%
%% ARGUMENTS:    - Loc_id: The ID of the location to add, which must not already exist and be >0
%%               - Name:   The name of the location (optional)
%%               - User:   The username of the location's owner (optional)
%%               - Port:   The port by which the location's controller listens for REST requests, which must not be already taken and be >=30000
%%
%% RETURNS:      - {ok,ok}                         -> The location was successfully added and its controller node was started
%%               - {ok,Error}                      -> The location was successfully added, but starting its controller returned an Error
%%               - ok                              -> The location was successfully added (but the controller node
%%                                                    was not started since the JANET Simulator is not running)
%%               - {error,location_already_exists} -> The loc_id already exists in the "location" table 
%%               - {error,port_already_taken}      -> The port is already used by another controller
%%               - {error,badarg}                  -> Invalid arguments
%%

% This is just an interface function, no further operation is required by the Simulator
add_location(Loc_id,Name,User,Port) ->
 db:add_location(Loc_id,Name,User,Port).
 
 
%% DESCRIPTION:  Adds an empty sublocation to the database and, if running, attempts to inform its controller node of it
%%
%% ARGUMENTS:    - {Loc_id,Subloc_id}: The sub_id of the sublocation, which must not already exist and be >0
%%               - Name:               The name of the sublocation (optional)
%%
%% RETURNS:      - {ok,ok}                            -> The sublocation was successfully added and
%%                                                       its controller node was informed of it
%%               - {ok,Error}                         -> The sublocation was successfully added, but
%%                                                       informing its controller raised an Error
%%               - ok                                 -> The sublocation was successfully added (informing
%%                                                       its controller was not necessary since either the
%%                                                       JANET Simulator or the controller node are stopped)
%%               - {error,location_not_exists}        -> The location 'Loc_id' does not exist
%%               - {error,sublocation_already_exists} -> A sublocation with such 'sub_id' already exists
%%               - {error,badarg}                     -> Invalid arguments
%%
add_sublocation({Loc_id,Subloc_id},Name) -> 
 
 % Attempt to add the new sublocation to the database
 case db:add_sublocation({Loc_id,Subloc_id},Name) of
  {error,DBError} ->
  
   % If the operation raised an error, return it
   {error,DBError};
   
  ok ->
  
   % If the sublocation was successfully added, attempt to inform the controller node of the new
   % sublocation, taking into account the situations where it or the JANET Simulator are stopped
   %
   % NOTE: the following 'try' block is structured so to return:
   %        - ok          -> The result of the previous database operation only (if the
   %                         JANET  Simulator or the controller node are not running)
   %        - {ok,CtrRes} -> The result of the database operation plus the result of informing
   %                         the controller of the new sublocation (if the JANET Simulator and
   %                         the controller node are running)
   try gen_ctr_command(Loc_id,ctr_db,add_sublocation,[Subloc_id]) of	
    
	% Result of the database operation + the result of informing the controller of the new sublocation
    Res ->
	 {ok,Res}
   catch
   
    % If the JANET Simulator is not running, just return the result of the database operation
    {error,janet_not_running} ->
     ok; 
 
    % If the controller node is not running, just return the result of the database operation
    {error,node_stopped} ->
     ok;

    % All other throws consist in functional errors, and should
	% be returned after the result of the database operation
    Error ->
     {ok,Error}
   end
 end.
 
 
%% DESCRIPTION:  Adds a new device with a default configuration to the database, and, if the JANET Simulator
%%               is running, also starts its device node and informs its controller of the new device
%%
%% ARGUMENTS:    - Dev_id:             The ID of the device, which must not already exist and be >0
%%               - Name:               The device's name (optional)
%%               - {Loc_id,Subloc_id}: The device's sub_id, which must exist and with Subloc_id >=0
%%               - Type:               The device's type, which must belong to the set of valid device types
%%
%% RETURNS:      - {ok,ok,ok}                     -> The device was successfully added, its device node was
%%                                                   started and its controller was informed of the new device
%%               - {ok,ok,Error}                  -> The device was successfully added, its device node was started,
%%                                                   but an Error occured in informing its controller of the new device
%%               - {ok,ok}                        -> The device was successfully added and its device node was started
%%                                                   (its controller was not informed since it is stopped)
%%               - {ok,Error}                     -> The device was successfully added, but an Error occured in starting its node
%%                                                   (its controlled was not informed since it would not have had sense)
%%               - ok                             -> The device was successfully added (but the device node
%%                                                   was not started since the JANET Simulator is not running)
%%               - {error,invalid_devtype}        -> The device type is invalid
%%               - {error,device_already_exists}  -> A device with such 'dev_id' already exists 
%%               - {error,sublocation_not_exists} -> The 'sub_id' sublocation doesn't exist
%%               - {error,badarg}                 -> Invalid arguments
%%
add_device(Dev_id,Name,{Loc_id,Subloc_id},Type) ->

 % Attempt to add the new device to the database and start its device node
 case db:add_device(Dev_id,Name,{Loc_id,Subloc_id},Type) of
  {ok,ok} ->

   % If the device was added successfully added AND its device node
   % was started, attempt to inform its controller of the new device
   %
   % NOTE: the following 'try' block is structured so to return:
   %        - {ok,ok}        -> The result of the previous two operations (if the controller node is not running)
   %        - {ok,ok,CtrRes} -> The result of the previous two operations plus the result of informing 
   %                            the controller of the new device (if the controller is running)
   try gen_ctr_command(Loc_id,ctr_db,add_device,[Dev_id,Subloc_id,Type]) of	
    
	% The result of the previous two operations plus the result of informing the controller of the new device
    Res ->
	 {ok,ok,Res}
   catch

    % If the controller node is not running, just return the result of the previous two operations
    {error,node_stopped} ->
     {ok,ok};

    % All other throws consist in functional errors, and should
	% be returned after the result of the previous two operations
    Error ->
     {ok,ok,Error}
   end;
   
  AddStartNode -> 
  
   % In all other cases, just return the results of the previous two operations
   AddStartNode
 end.


%% =========================================================== READ ================================================================%%  

%% DESCRIPTION:  Prints the contents of all or a specific table in the database
%%
%% ARGUMENTS:    - (Tabletype): The table to print, also considering shorthand forms
%%               - (),(all):    Print all tables in the database   
%%
%% RETURNS:      - ok                    -> The table(s) contents were printed
%%               - {error,unknown_table} -> Unknown table
%%               - {error,badarg}        -> Invalid arguments
%%

% These are just all interface functions, no further operation is required by the Simulator
print_table(all) ->
 db:print_table(all);
print_table(Tabletype) ->
 db:print_table(Tabletype).
print_table() ->
 db:print_table(all).


%% DESCRIPTION:  Prints the database contents associated to one or more users, a location or a sublocation indented as a tree
%%
%% ARGUMENTS:    - (),(all):             Prints the tree of locations, sublocations and devices of all users
%%               - (user,Username):      Prints the tree of locations, sublocations and devices of a specific user
%%               - (location,Loc_id):    Prints the tree of sublocations and devices in a specific location
%%               - (sublocation,Sub_id): Prints the tree of devices in a specific sublocation
%%
%% RETURNS:      - ok                     -> Required contents were printed
%%               - user_not_exists        -> The specified user doesn't exist    (print_tree(user,Username))
%%               - location_not_exists    -> Location "Loc_id" does not exist    (print_tree(location,Loc_id))
%%               - sublocation_not_exists -> Sublocation "Sub_id" does not exist (print_tree(sublocation,Sub_id)
%%               - {error,unknown_table}  -> Unknown table                       (print_tree(Tabletype,id))
%%               - {error,unsupported}    -> Operation not supported             (print_tree(device,Id))
%%               - {error,badarg}         -> Invalid arguments
%%

% These are just all interface functions, no further operation is required by the Simulator
print_tree(all) ->
 db:print_tree(all).
print_tree() ->
 db:print_tree(all).
print_tree(user,Username) ->
 db:print_tree(user,Username);
print_tree(Tabletype,Id) ->
 db:print_tree(Tabletype,Id).


%% DESCRIPTION:  Returns a table record by key, if it exists
%%
%% ARGUMENTS:    - Tabletype: The table where to search the record, also considering shorthand forms
%%               - Key:       The record key (>=0)
%%
%% RETURNS:      - {ok,Record}             -> The record with key "Key" in table "Tabletype"
%%               - {error,not_found}       -> The record with key "Key" was not found in table "Tabletype"
%%               - {error,unknown_table} -> Unknown table
%%               - {error,badarg}        -> Invalid arguments
%%
get_record(Tabletype,Key) ->

 % Attempt to retrieve the record of key "Key" in table "Tabletype"
 case db:get_record(Tabletype,Key) of
  {ok,Record} ->
  
   % If the record was successfully retrieved, print it
   io:format("~n~p~n~n",[Record]);
   
  {error,Reason} ->
   
   % If the table and/or record do not exist, return the error
   io:format("~n~p~n~n",[Reason])
 end.   

 
%% ========================================================== UPDATE ===============================================================%% 

%% DESCRIPTION:  Updates a device's sublocation and, if the controller node is running, informs it of such change
%%
%% ARGUMENTS:    - Dev_id:             The ID of the device to change sublocation, which must exist and be >0
%%               - {Loc_id,Subloc_id}: The 'sub_id' of the sublocation where to put the device, where "Loc_id" must be >0
%%                                     and coincide with the current device's location, and the "Subloc_id" must be >=0
%%
%% RETURNS:      - {ok,ok}                        -> The device's sublocation was successfully updated
%%                                                   and its controller node informed of such change
%%               - {ok,Error}                     -> The device's sublocation was successfully updated, but
%%                                                   informing its controller of such change returned an Error
%%               - ok                             -> Device sublocation successfully updated (the controller was not
%%                                                   informed of the change since it or the JANET Simulator are stopped)
%%               - {error,device_not_exists}      -> The device 'Dev_id' does not exist
%%               - {error,sublocation_not_exists} -> The sublocation 'sub_id' does not exist
%%               - {error,different_locations}    -> The specified and the current sublocations are in different locations
%%               - {error,badarg}                 -> Invalid arguments
%%
update_dev_subloc(Dev_id,{Loc_id,Subloc_id}) ->
 
 % Attempt to change the device's sublocation
 case db:update_dev_subloc(Dev_id,{Loc_id,Subloc_id}) of
  {error,DBError} ->
  
   % If the operation raised an error, return it
   {error,DBError};
   
  ok ->
  
   % If the device's sublocation was successfully changed, attempt to inform its controller of such
   % change, taking into account the situations where the it or the JANET Simulator are stopped
   %
   % NOTE: the following 'try' block is structured so to return:
   %        - ok          -> The result of the previous database operation only (if the
   %                         JANET  Simulator or the controller node are not running)
   %        - {ok,CtrRes} -> The result of the database operation plus the result of informing
   %                         the controller of such change (if the JANET Simulator and the
   %                         controller node are running)
   try gen_ctr_command(Loc_id,ctr_db,update_dev_sub,[Dev_id,Subloc_id]) of	
    
	% Result of the database operation + the result of informing the controller of the change
    Res ->
	 {ok,Res}
   catch
   
    % If the JANET Simulator is not running, just return the result of the database operation
    {error,janet_not_running} ->
     ok; 
 
    % If the controller node is not running, just return the result of the database operation
    {error,node_stopped} ->
     ok;

    % All other throws consist in functional errors, and should
	% be returned after the result of the database operation
    Error ->
     {ok,Error}
   end
 end.


%% DESCRIPTION:  Updates a location's name
%%
%% ARGUMENTS:    - Loc_id: The ID of the location to update the name, which must exist and be >0
%%               - Name:   The updated location name
%%
%% RETURNS:      - ok                          -> Location name successfully updated
%%               - {error,location_not_exists} -> The location 'Loc_id' does not exist
%%               - {error,badarg}              -> Invalid arguments
%%

% This is just an interface function, no further operation is required by the Simulator
update_loc_name(Loc_id,Name) ->
 db:update_loc_name(Loc_id,Name).


%% DESCRIPTION:  Updates a sublocation's name
%%
%% ARGUMENTS:    - {Loc_id,Subloc_id}: The sub_id of the sublocation to update
%%                                     the name, which must exist and be {>0,>0}
%%               - Name:               The updated sublocation name
%%
%% RETURNS:      - ok                             -> Sublocation name successfully updated
%%               - {error,sublocation_not_exists} -> The sublocation 'Sub_id' does not exist
%%               - {error,badarg}                 -> Invalid arguments
%%
%% NOTE:         Currently the name of the "(default)" sublocation of a location cannot be changed (Subloc_id >0)
%%

% This is just an interface function, no further operation is required by the Simulator
update_subloc_name({Loc_id,Subloc_id},Name) -> 
 db:update_subloc_name({Loc_id,Subloc_id},Name).
 
 
%% DESCRIPTION:  Updates a device's name
%%
%% ARGUMENTS:    - Dev_id: The dev_id of the device to update the name, which must exist and be >0
%%               - Name:   The updated device name
%%
%% RETURNS:      - ok                        -> Device name successfully updated
%%               - {error,device_not_exists} -> The device 'Dev_id' does not exist
%%               - {error,badarg}            -> Invalid arguments
%%

% This is just an interface function, no further operation is required by the Simulator
update_dev_name(Dev_id,Name) ->
 db:update_dev_name(Dev_id,Name).


%% ========================================================== DELETE ===============================================================%% 

%% DESCRIPTION:  Deletes a location, along with all its sublocations and devices, from the
%%               database, also stopping the associated controller' and devices' nodes
%%
%% ARGUMENTS:    - Loc_id: The ID of the location to delete from the database
%%
%% RETURNS:      - {ok,ok}                     -> The location and all its sublocations and devices were
%%                                                successfully deleted, and the associated nodes were stopped
%%               - ok                          -> The location and all its sublocations and devices were successfully
%%                                                deleted (not their nodes since the JANET Simulator is not running)
%%               - {error,location_not_exists} -> The location 'Loc_id' does not exist
%%               - {error,badarg}              -> Invalid arguments
%%
%% NOTE:         Use with caution, for deleted locations, sublocations and devices CANNOT be recovered
%%

% This is just an interface function, no further operation is required by the Simulator
delete_location(Loc_id) ->
 db:delete_location(Loc_id).


%% DESCRIPTION:  Deletes a sublocation from the database, moving all its devices to its location's default 
%%               sublocation {Loc_id,0} and, if it is running, informs the location controller of such changes
%%
%% ARGUMENTS:    - {Loc_id,Subloc_id}: The sub_id of the sublocation to delete, with both elements >0
%%
%% RETURNS:      - {ok,ok}                        -> The sublocation was successfully deleted, its devices were
%%                                                   moved to the location's default sublocation {Loc_id,0},
%%                                                   and the location controller was informed of such change
%%               - ok                             -> The sublocation was successfully deleted and its devices were moved
%%                                                   to the location's default sublocation {Loc_id,0} (the controller
%%                                                   was not inform since it or the JANET Simulator are not running)
%%               - {error,sublocation_not_exists} -> The sublocation 'Sub_id' does not exist
%%               - {error,badarg}                 -> Invalid arguments
%%
%% NOTE:         Default sublocations cannot be removed (Subloc_id > 0)
%%
delete_sublocation({Loc_id,Subloc_id}) ->

 % Attempt to remove the sublocation from the database
 case db:delete_sublocation({Loc_id,Subloc_id}) of
  {error,DBError} ->
  
   % If the operation raised an error, return it
   {error,DBError};
   
  ok ->
  
   % If the sublocation was successfully removed, attempt to inform the controller node of the change,
   % change, taking into account the situations where it or the JANET Simulator are stopped
   %
   % NOTE: the following 'try' block is structured so to return:
   %        - ok          -> The result of the previous database operation only (if the
   %                         JANET  Simulator or the controller node are not running)
   %        - {ok,CtrRes} -> The result of the database operation plus the result of informing
   %                         the controller of the change (if the JANET Simulator and
   %                         the controller node are running)
   try gen_ctr_command(Loc_id,ctr_db,delete_sublocation,[Subloc_id]) of	
    
	% Result of the database operation + the result of informing the controller of the new sublocation
    Res ->
	 {ok,Res}
   catch
   
    % If the JANET Simulator is not running, just return the result of the database operation
    {error,janet_not_running} ->
     ok; 
 
    % If the controller node is not running, just return the result of the database operation
    {error,node_stopped} ->
     ok;

    % All other throws consist in functional errors, and should
	% be returned after the result of the database operation
    Error ->
     {ok,Error}
   end
 end.


%% DESCRIPTION:  Deletes a device from the database and if they are running, the
%%               device node is stopped and its controller is informed of such change
%%
%% ARGUMENTS:    - Dev_id: The 'dev_id' of the device to delete, which must exist and be >0
%%
%% RETURNS:      - {ok,ok,ok}                -> The device was deleted from the database, its node was
%%                                              stopped, and its controller was informed of such change
%%               - {ok,ok,ErrorCtr}          -> The device was deleted from the database and its node was stopped,
%%                                              but an "ErrorCtr" was raised in informing its controller of such change
%%               - {ok,ErrorDev,ok}          -> The device was deleted from the database and its controller was informed
%%                                              of such change, but an "ErrorDev" was raised in stopping the device's node
%%               - {ok,ErrorDev,ErrorCtr}    -> The device was deleted from the database, but two "ErrorDev" and "ErrorCtr" were
%%                                              raised respectively in stopping its node and informing its controller of such change
%%               - {ok,ok}                   -> The device was deleted from the database and its node was stopped
%%                                              (its controller was not informed since it is not running)
%%               - {ok,ErrorDev}             -> The device was deleted from the database, but an "ErrorDev" was raised in
%%                                              stopping its node (the controller was not informed since it is not running)
%%               - ok                        -> The device was deleted from the database (its node was not terminated and
%%                                              the controller was not informed since the JANET Simulator is not running)
%%               - {error,device_not_exists} -> The device 'Dev_id' does not exist
%%               - {error,badarg}            -> Invalid arguments
%%
delete_device(Dev_id) when is_number(Dev_id), Dev_id >0 ->

 % Attempt to retrieve the device's location ID (this must be done
 % done before attempting to delete the device from the database)
 case db:get_record(device,Dev_id) of
  {error,not_found} ->
  
   % If the device was not found in the database, return the error
   {error,device_not_exists};
	
  {ok,DevRecord} ->
  
   % If the device was found in the database, attempt
   % to delete it from it and to stop its node
   case db:delete_device(Dev_id) of
    {ok,DevStopRes} ->

     % If the device was successfully removed from the database, whether
     % its node was stopped or not, inform the controller of such change
     %
     % NOTE: the following 'try' block is structured so to return:
     %        - {ok,DevStopRes}        -> The result of the previous two operations (if the controller is not running)
     %        - {ok,DevStopRes,CtrRes} -> The result of the previous two operations plus the result of informing 
     %                                    the controller of the change (if the controller is running)
     try gen_ctr_command(element(1,DevRecord#device.sub_id),ctr_db,delete_device,[Dev_id]) of	
    
	  % The result of the previous two operations plus the result of informing the controller of the change
      CtrRes ->
  	   {ok,DevStopRes,CtrRes}
     catch

      % If the controller node is not running, just return the result of the previous two operations
      {error,node_stopped} ->
       {ok,DevStopRes};

      % All other throws consist in functional errors, and should
	  % be returned after the result of the previous two operations
      Error ->
       {ok,DevStopRes,Error}
     end;
   
    DeleteStopNode -> 
  
     % In all other cases, just return the results of the previous two operations
     DeleteStopNode
   end
 end;

delete_device(_) ->
 {error,badarg}.


%%====================================================================================================================================
%%                                                  JANET NODES STOP AND RESTART
%%====================================================================================================================================

%% ==================================================== PER-NODE STOP/RESTART ==================================================== %%

%% DESCRIPTION:  Stops a running controller or device node, shutting down its manager and so VM
%%
%% ARGUMENTS:    - NodeTypeShorthand: An atom indicating the type of node to be stopped, also taking
%%                                    into account shorthand forms, with the following being allowed:
%%                                     - controller,ctr,contr -> controller node
%%                                     - device, dev          -> device node
%%
%%               - Node_id: The ID of the node to stop, which must consist in the 'loc_id'
%%                          for controller nodes and in the 'dev_id' for device nodes (>0) 
%%
%% RETURNS:      - ok                          -> Node successfully stopped
%%               - {error,not_running}         -> The node is already stopped
%%               - {error,janet_not_running}   -> The Janet Simulator is not running
%%               - {error,location_not_exists} -> The location associated with the specified
%%                                                controller node does not exist
%%               - {error,device_not_exists}   -> The specified device does not exist
%%               - {error,unknown_nodetype}    -> Unknown node type
%%               - {error,badarg}              -> Invalid arguments
%%               - {error,{internal,Reason}}   -> Internal error (should not happen)
%%
stop_node(NodeTypeShortHand,Node_id) when is_atom(NodeTypeShortHand), is_number(Node_id), Node_id>0 ->
 catch(change_node_status(NodeTypeShortHand,Node_id,stop));
 
stop_node(_,_) ->
 io:format("usage: stop_node(ctr|dev,loc_id|dev_id)~n"),
 {error,badarg}. 
 
 
%% DESCRIPTION:  Restarts a stopped controller or device node, reinstantiating its manager and so VM
%%
%% ARGUMENTS:    - NodeTypeShorthand: An atom indicating the type of node to be restarted, also taking
%%                                    into account shorthand forms, with the following being allowed:
%%                                     - controller,ctr,contr -> controller node
%%                                     - device, dev          -> device node
%%
%%               - Node_id: The ID of the node to restart, which must consist in the 'loc_id'
%%                          for controller nodes and in the 'dev_id' for device nodes (>0) 
%%
%% RETURNS:      - ok                              -> Node successfully restarted
%%               - {error,already_running}         -> The node is already running
%%               - {error,janet_not_running}       -> The Janet Simulator is not running
%%               - {error,location_not_exists}     -> The location associated with the specified
%%                                                    controller node does not exist
%%               - {error,device_not_exists}       -> The specified device does not exist
%%               - {error,unknown_nodetype}        -> Unknown node type
%%               - {error,node_already_restarting} -> The node is already restarting
%%               - {error,badarg}                  -> Invalid arguments
%%               - {error,{internal,Reason}}       -> Internal error (should not happen)
%% 
restart_node(NodeTypeShortHand,Node_id) when is_atom(NodeTypeShortHand), is_number(Node_id), Node_id>0 ->
 catch(change_node_status(NodeTypeShortHand,Node_id,restart));
 
restart_node(_,_) ->
 io:format("usage: restart_node(ctr|dev,loc_id|dev_id)~n"),
 {error,badarg}. 


%% ================================================ PER-SUBLOCATION STOP/RESTART ================================================ %%

%% DESCRIPTION:  Stops all device nodes in a sublocation, shutting down their managers and so VMs
%%
%% ARGUMENTS:    - {Loc_id,Subloc_id}: The ID of the sublocation whose devices are to be stopped
%%
%% RETURNS:      - ok                             -> Information on the devices nodes that were stopped,
%%                                                   were already stopped and failed to stop is reported.
%%                                                   This also includes the cases where:
%%                                                    - The sublocation is empty
%%                                                    - All sublocation devices were already stopped
%%               - {error,janet_not_running}      -> The Janet Simulator is not running
%%               - {error,sublocation_not_exists} -> The specified sublocation does not exist
%%               - {error,badarg}                 -> Invalid arguments
%%
stop_subloc({Loc_id,Subloc_id}) when is_number(Loc_id), Loc_id>0, is_number(Subloc_id), Subloc_id>=0 ->

 % The operation is enclosed in a try expression for catching throws not associated with functional errors
 try change_subloc_status({Loc_id,Subloc_id},stop)
 catch
 
  % If the sublocation is empty
  subloc_empty ->
   io:format("The sublocation is empty~n");
   
  % If all devices in the sublocation are already stopped
  all_devs_stopped ->
   io:format("All devices in the sublocation are already stopped~n");
   
  % All other throws consist in functional errors {error,Error}, and should be returned as they are
  Error ->
   Error
 end;   
   
stop_subloc(_) ->
 io:format("usage: stop_subloc({loc_id,subloc_id})~n"),
 {error,badarg}. 


%% DESCRIPTION:  Restarts all device nodes in a sublocation, reinstantiating their managers and so VMs
%%
%% ARGUMENTS:    - {Loc_id,Subloc_id}: The ID of the sublocation whose devices are to be restarted
%%
%% RETURNS:      - ok                             -> Information on the devices nodes that were restarted,
%%                                                   were already running and failed to restart is reported.
%%                                                   This also includes the cases where:
%%                                                    - The sublocation is empty
%%                                                    - All sublocation devices were already running
%%               - {error,janet_not_running}      -> The Janet Simulator is not running
%%               - {error,sublocation_not_exists} -> The specified sublocation does not exist
%%               - {error,badarg}                 -> Invalid arguments
%%
restart_subloc({Loc_id,Subloc_id}) when is_number(Loc_id), Loc_id>0, is_number(Subloc_id), Subloc_id>=0 ->

 % The operation is enclosed in a try expression for catching throws not associated with functional errors
 try change_subloc_status({Loc_id,Subloc_id},restart)
 catch
 
  % If the sublocation is empty
  subloc_empty ->
   io:format("The sublocation is empty~n");
  
  % If all devices in the sublocation are already stopped  
  all_devs_running ->
   io:format("All devices in the sublocation are already running~n");
   
  % All other throws consist in functional errors {error,Error}, and should be returned as they are
  Error ->
   Error
 end;   
 
restart_subloc(_) ->
 io:format("usage: restart_subloc({loc_id,subloc_id})~n"),
 {error,badarg}. 


%% ================================================== PER-LOCATION STOP/RESTART ================================================== %%

%% DESCRIPTION:  Stops the controller and all device nodes in a location, shutting down their managers and so VMs
%%
%% ARGUMENTS:    - Loc_id: The ID of the location whose controller and devices are to be stopped
%%
%% RETURNS:      - ok                          -> Information on the controller and devices nodes that
%%                                                were already stopped and failed to stop is reported.
%%                                                This also includes the cases where:
%%                                                 - The location contains no devices
%%                                                 - The controller and all devices in
%%                                                   the location were already stopped
%%               - {error,janet_not_running}   -> The Janet Simulator is not running
%%               - {error,location_not_exists} -> The specified location does not exist
%%               - {error,badarg}              -> Invalid arguments
%%
stop_loc(Loc_id) when is_number(Loc_id), Loc_id>0 ->
 catch(change_loc_status(Loc_id,stop));
 
stop_loc(_) ->
 io:format("usage: stop_loc(loc_id)~n"),
 {error,badarg}. 
 

%% DESCRIPTION:  Restarts the controller and all device nodes in a location, reinstantiating their managers and so VMs
%%
%% ARGUMENTS:    - Loc_id: The ID of the location whose controller and devices are to be restarted
%%
%% RETURNS:      - ok                          -> Information on the controller and devices nodes that
%%                                                were already running and failed to restart is reported.
%%                                                This also includes the cases where:
%%                                                 - The location contains no devices
%%                                                 - The controller and all devices in
%%                                                   the location were already running
%%               - {error,janet_not_running}   -> The Janet Simulator is not running
%%               - {error,location_not_exists} -> The specified location does not exist
%%               - {error,badarg}              -> Invalid arguments
%% 
restart_loc(Loc_id) when is_number(Loc_id), Loc_id>0 ->
 catch(change_loc_status(Loc_id,restart));
 
restart_loc(_) ->
 io:format("usage: restart_loc(loc_id)~n"),
 {error,badarg}. 


%% =================================================== ALL-NODES STOP/RESTART =================================================== %%

%% DESCRIPTION:  Stops all controller and device nodes in the JANET simulator, shutting down their managers and so VMs
%%
%% ARGUMENTS:    - none
%%
%% RETURNS:      - ok                        -> A summary of the operation is reported, which 
%%                                              includes nodes that raised an error in their stopping
%%               - {error,janet_not_running} -> The Janet Simulator is not running
stop_all_nodes() ->
 catch(change_all_nodes_statuses(stop)).


%% DESCRIPTION:  Stops all controller and device nodes in the JANET simulator, shutting down their managers and so VMs
%%
%% ARGUMENTS:    - none
%%
%% RETURNS:      - ok                        -> A summary of the operation is reported, which 
%%                                              includes nodes that raised an error in their stopping
%%               - {error,janet_not_running} -> The Janet Simulator is not running 
restart_all_nodes() ->
 catch(change_all_nodes_statuses(restart)).


%%====================================================================================================================================
%%                                                   SIMULATION UTILITY FUNCTIONS                                                        
%%====================================================================================================================================

%% =============================================== RUNNING AND STOPPED NODES INFO =============================================== %%

%% DESCRIPTION:  Prints the IDs of all stopped and/or running JANET nodes
%%
%% ARGUMENTS:    - (), (all): Print the IDs of all stopped and running nodes
%%               - (stopped): Print the IDs of all stopped nodes
%%               - (running): Print the IDs of all running nodes
%%
%% RETURNS:      - ok                        -> The IDs of all stopped and/or running JANET nodes was printed
%%               - {error,janet_not_running} -> The Janet Simulator is not running
%%               - {error,badarg}            -> Invalid arguments
%% 
print_nodes() ->
 catch(print_managers(all)).

print_nodes(all) ->
 catch(print_managers(all));
 
print_nodes(stopped) ->
 catch(print_managers(stopped));
 
print_nodes(running) ->
 catch(print_managers(running));

print_nodes(_) ->
 io:format("usage: print_nodes(stopped|running|all)~n"),
 {error,badarg}. 
 
 
%% ================================================ CONTROLLER NODES INTERACTION  ================================================ %%

%% DESCRIPTION:  Prints a specific or all Mnesia RAM Tables in a running controller node
%%
%% ARGUMENTS:    - (Loc_id), (Loc_id,all): Print all Mnesia RAM Tables in the controller node of location "Loc_id"
%%               - (Loc_id,TableType):     Prints the TableType Mnesia RAM table in the controller node of location "Loc_id",
%%                                         with the following table types being allowed, also considering shorthand forms
%%
%%                                          - sub,subloc,sublocation,ctr_sub,ctr_subloc,ctr_sublocation -> ctr_sublocation
%%                                          - dev,device,ctr_dev,ctr_device                             -> ctr_device
%%
%% RETURNS:      - ok                          -> The specified tables were printed
%%               - {error,janet_not_running}   -> The Janet Simulator is not running
%%               - {error,location_not_exists} -> The location associated with the specified controller node does not exist 
%%               - {error,node_stopped}        -> The controller node is currently stopped
%%               - {error,unknown_table}       -> Unknown TableType
%%               - {error,request_timeout}     -> Request timeout (either on the controller node or in its manager)
%%               - {error,ctr_timeout}         -> Controller node timeout
%%               - {error,badarg}              -> Invalid arguments
%%
%% NOTE:         This function relies on the fact that all I/O in the controller (slave)
%%               node is automatically reidirected to the simulator (master) node
%%
print_ctr_table(Loc_id) when is_number(Loc_id), Loc_id>0 ->
 catch(gen_ctr_command(Loc_id,ctr_db,print_table,[]));
 
print_ctr_table(_) -> 
 io:format("usage: print_ctr_table(Loc_id,|all|Ctr_Table)~n"),
 {error,badarg}. 
 
print_ctr_table(Loc_id,TableType) when is_number(Loc_id), Loc_id>0, is_atom(TableType) ->
 catch(gen_ctr_command(Loc_id,ctr_db,print_table,[TableType]));

print_ctr_table(_,_) -> 
 io:format("usage: print_ctr_table(Loc_id,|all|Ctr_Table)~n"),
 {error,badarg}. 


%% DESCRIPTION:  Prints all sublocations and their devices in a controller node as a tree
%%
%% ARGUMENTS:    - Loc_id: The location ID of the controller node
%%
%% RETURNS:      - ok                          -> The devices sublocations allocation in the controller was printed
%%               - {error,janet_not_running}   -> The Janet Simulator is not running
%%               - {error,location_not_exists} -> The location associated with the specified controller node does not exist 
%%               - {error,node_stopped}        -> The controller node is currently stopped
%%               - {error,request_timeout}     -> Request timeout (either on the controller node or in its manager)
%%               - {error,ctr_timeout}         -> Controller node timeout
%%               - {error,badarg}              -> Invalid arguments
%%
%% NOTE:         This function relies on the fact that all I/O in the controller (slave)
%%               node is automatically reidirected to the simulator (master) node
%%
print_ctr_tree(Loc_id) when is_number(Loc_id), Loc_id>0 ->
 catch(gen_ctr_command(Loc_id,ctr_db,print_tree,[]));
 
print_ctr_tree(_) -> 
 io:format("usage: print_ctr_tree(Loc_id)~n"),
 {error,badarg}.


%% DESCRIPTION:  Executes a custom command on a running controller node
%%
%% ARGUMENTS:    - Loc_id:   The location ID of the controller node
%%               - Module:   The Erlang module where the function to be executed is defined
%%               - Function: The function to be executed
%%               - ArgsList: The [LIST] of function arguments
%%
%% RETURNS:      - ok                          -> The devices sublocations allocation in the controller was printed
%%               - {error,janet_not_running}   -> The Janet Simulator is not running
%%               - {error,location_not_exists} -> The location associated with the specified controller node does not exist 
%%               - {error,node_stopped}        -> The controller node is currently stopped
%%               - {error,request_timeout}     -> Request timeout (either on the controller node or in its manager)
%%               - {error,ctr_timeout}         -> Controller node timeout
%%               - {error,badarg}              -> Invalid arguments
%%
%% NOTE:         This function is for DEBUGGING PURPOSES ONLY, and will crash the controller node in case of errors
%%
ctr_command(Loc_id,Module,Function,ArgsList) when is_number(Loc_id), Loc_id>0 ->
 catch(gen_ctr_command(Loc_id,Module,Function,ArgsList));
 
ctr_command(_,_,_,_) ->
 io:format("usage: ctr_command(Loc_id,Module,Function,ArgsList)~n"),
 {error,badarg}.


%% ================================================== DEVICE NODES INTERACTION  ================================================== %%

%% DESCRIPTION:  Changes the configuration of a running device node
%%
%% ARGUMENTS:    - Dev_id:   The node's device ID
%%               - {Config}: A tuple of type-specific variables representing a
%%                           device's configuration, with the following  being
%%                           allowed (see the "devtypes_configurations_definitions.hrl"
%%                           header file for more information):
%%                            - fan:         {OnOff,FanSpeed}
%%                            - light:       {OnOff,Brightness,ColorSetting}
%%                            - door:        {OpenClose,LockUnlock}
%%                            - thermostat:  {OnOff,TempTarget,TempCurrent}
%%                            - conditioner: {OnOff,TempTarget,TempCurrent,FanSpeed}
%%                           The '$keep' wildcard can also be used in any field for the
%%                           preserving its current value in the current device configuration
%%
%% RETURNS:      - {ok,{UpdatedCfg,Time}}    -> The device's configuration was updated as
%%                                              requested at time "Time" on the device node
%%               - {error,invalid_devconfig} -> The passed {Config} arguments represent an
%%                                              invalid configuration for the device Type
%%               - {error,janet_not_running} -> The Janet Simulator is not running
%%               - {error,device_not_exists} -> The specified device does not exist
%%               - {error,node_stopped}      -> The device node is currently stopped
%%               - {error,dev_booting}       -> The device node is still booting
%%               - {error,request_timeout}   -> Request timeout (either on the device node or in its manager)
%%               - {error,dev_timeout}       -> Device node timeout
%%               - {error,badarg}            -> Invalid arguments
%%
%% NOTE:         If the operation is successful the updated device configuration along with its
%%               timestamp is automatically pushed in the 'device' table in the Mnesia database
%%               as well as forwarded to its device handler in the controller node (or in any
%%               case buffered is the device is not currently registered with it)
%%
dev_config_change(Dev_id,Config) when is_number(Dev_id), Dev_id>0 ->
 catch(gen_dev_config_change(Dev_id,Config));
 
dev_config_change(_,_) ->
 io:format("usage: dev_config_change(Dev_id,Config) (Config = device-dependent)~n"),
 {error,badarg}.


%% DESCRIPTION:  Executes a custom command on a running device node
%%
%% ARGUMENTS:    - Dev_id:   The node's device ID
%%               - Module:   The Erlang module where the function to be executed is defined
%%               - Function: The function to be executed
%%               - ArgsList: The [LIST] of function arguments
%%
%% RETURNS:      - ok                        -> The devices sublocations allocation in the controller was printed
%%               - {error,janet_not_running} -> The Janet Simulator is not running
%%               - {error,device_not_exists} -> The specified device does not exist
%%               - {error,node_stopped}      -> The device node is currently stopped
%%               - {error,dev_booting}       -> The device node is still booting
%%               - {error,request_timeout}   -> Request timeout (either on the device node or in its manager)
%%               - {error,dev_timeout}       -> Device node timeout
%%               - {error,badarg}            -> Invalid arguments
%%
%% NOTE:         This function is for DEBUGGING PURPOSES ONLY, and will crash the device node in case of errors
%%
dev_command(Dev_id,Module,Function,ArgsList) when is_number(Dev_id), Dev_id>0 ->
 catch(gen_dev_command(Dev_id,Module,Function,ArgsList));
 
dev_command(_,_,_,_) ->
 io:format("usage: dev_command(Dev_id,Module,Function,ArgsList)~n"),
 {error,badarg}.


%%====================================================================================================================================
%%                                        PRIVATE JANET NODES STOP AND RESTART HELPER FUNCTIONS
%%====================================================================================================================================

%% =========================================== PER-NODE STOP/RESTART HELPER FUNCTIONS =========================================== %%

%% Changes a node status by halting or restarting its manager process 
%% (stop_node(NodeTypeShortHand,Node_id), restart_node(NodeTypeShortHand,Node_id) helper function)
change_node_status(NodeTypeShortHand,Node_id,Mode) -> 
 
 % Ensure the JANET Simulator to be running
 ok = utils:ensure_janet_started(),

 % If it is running, determine the passed node type, taking shorthand forms into account
 NodeType = utils:resolve_nodetype_shorthand(NodeTypeShortHand),
   
 % Retrieve the node's manager location ID and status
 {Loc_id,_,MgrStatus} = db:get_manager_info(NodeType,Node_id),
   
 % Verify the node's status change to be valid (i.e. not attempting
 % to stop an already stopped or restart an already running node)
 ok = verify_node_status_change(MgrStatus,Mode),
   
 % Retrieve the PID of the node manager's 'sup_loc' supervisor
 Sup_pid = db:get_suploc_pid(Loc_id),
  
 % Attempt to change the node's manager status via its 'sup_loc' supervisor as of "Mode"
 ok = change_manager_status(Sup_pid,NodeType,Node_id,Mode).
	
	
%% ================================================ PER-SUBLOCATION STOP/RESTART ================================================ %%

%% Attempts to change the statuses of all device managers in a sublocation
%% (stop_subloc({Loc_id,Subloc_id}),restart_subloc({Loc_id,Subloc_id}) helper function)
change_subloc_status({Loc_id,Subloc_id},Mode) ->

 % Ensure the JANET Simulator to be running
 ok = utils:ensure_janet_started(),
 
 % Retrieve the list of devices in the sublocation
 DevIdList = get_subloc_devs_throw({Loc_id,Subloc_id}),
   
 % Retrieve the PID of the managers' 'sup_loc' supervisor
 Sup_pid = db:get_suploc_pid(Loc_id),
   
 % Attempt to change the statuses of all devices in the sublocation
 DevicesStatusesChange = change_devices_statuses(DevIdList,Sup_pid,Mode),
   
 % Print a summary of the operation
 print_devs_statuses_change_summary(DevicesStatusesChange,Mode).


%% Retrieves the list of devices in a sublocation, raising throws in case the sublocation does not exist or is empty 
%% (change_subloc_status({Loc_id,Subloc_id},Mode) helper function)
get_subloc_devs_throw({Loc_id,Subloc_id}) ->
    
 % Retrieve the list of devices in the sublocation
 case db:get_subloc_devs({Loc_id,Subloc_id}) of

    % Empty sublocation
    [] ->
	 throw(subloc_empty);
	 
    % If the sublocation does not exist, return an error
    {error,sublocation_not_exists} ->
     throw({error,sublocation_not_exists});
	 
	% If the list of devices was successfully retrieved
    DevIdList ->
     DevIdList
 end.


%% ================================================== PER-LOCATION STOP/RESTART ================================================== %%

%% Attempts to change the statuses of the controller and all devices managers in a location
%% (stop_loc(Loc_id),restart_loc(Loc_id) helper function)
change_loc_status(Loc_id,Mode) ->

 % Ensure the JANET Simulator to be running
 ok = utils:ensure_janet_started(),
 
 % Retrieve the PID of location managers' 'sup_loc' supervisor
 Sup_pid = db:get_suploc_pid(Loc_id),
   
 % Attempt to change the status of the location's controller manager as of "Mode"
 CtrMgrStatus = catch(change_ctr_status(Loc_id,Sup_pid,Mode)),
   
 % Retrieve the list of devices in the sublocation
 %
 % NOTE: Differently from "change_subloc_status()" and its "get_subloc_devs_throw()" function in
 %       this case an empty list can be returned, meaning that the location contains no devices  
 DevIdList = db:get_loc_devs(Loc_id),
   
 % Attempt to change the statuses of all device managers in the location, obtaining in return:
 %
 %  - If a non-empty "DevIdList" was passed, the {StoppedMgrs,RunningMgrs,ChangedMgrsSuccesses,AllMgrsFails}
 %    lists (see print_loc_status_change_summary())
 %  - If an empty "DevIdList" was passed, the tuple {[],[],[],[]}
 %  - If attempting to stop or restart a list of devices that are already all
 %    stopped or running, the 'all_devs_stopped' or 'all_devs_running' atoms
 % 
 DevicesStatusesChange = catch(change_devices_statuses(DevIdList,Sup_pid,Mode)),
   
 % Print a summary of the operation
 print_loc_status_change_summary(Loc_id,CtrMgrStatus,DevicesStatusesChange,Mode).
 

%% Prints a summary of the status change operation of the controller and devices in a location
%% (change_loc_status(Loc_id,Mode) helper function)

% Location controller devices were all already stopped
print_loc_status_change_summary(_,{error,not_running},all_devs_stopped,stop) ->
 io:format("The controller and all location devices are already stopped~n");
 
% Location controller devices were all already running  
print_loc_status_change_summary(_,{error,already_running},all_devs_running,restart) ->
 io:format("The controller and all location devices are already running~n");
 
% Hybrid case 
print_loc_status_change_summary(Loc_id,CtrMgrStatus,DevicesStatusesChange,Mode) ->

 % Print a summary of the updated status of the location controller
 print_ctr_status_change_summary(utils:prefix_node_id(controller,Loc_id),CtrMgrStatus,Mode),
   
 % Print a summary of updated statuses of location devices
 print_loc_devs_statuses_change_summary(DevicesStatusesChange,Mode).  
   
  
%% Prints a summary of the updated status of a location's controller
%% (print_loc_status_change_summary(Loc_id,CtrMgrStatus,DevicesStatusesChange,Mode) helper function)

% The controller is already stopped
print_ctr_status_change_summary(Pre_Ctr_id,{error,not_running},stop) ->
 io:format("The controller ~p is already stopped~n",[Pre_Ctr_id]);

% The controller is already running
print_ctr_status_change_summary(Pre_Ctr_id,{error,already_running},restart) ->
 io:format("The controller ~p is already running~n",[Pre_Ctr_id]);

% The controller successfully stopped
print_ctr_status_change_summary(Pre_Ctr_id,{ok,stop},stop) ->
 io:format("The controller ~p was successfully stopped~n",[Pre_Ctr_id]);

% The controller successfully restarted
print_ctr_status_change_summary(Pre_Ctr_id,{ok,restart},restart) ->
 io:format("The controller ~p was successfully restarted~n",[Pre_Ctr_id]);

% Error in stopping the controller
print_ctr_status_change_summary(Pre_Ctr_id,{error,Reason},stop) ->
 io:format("The controller ~p raised an error in its stopping: {error,~p}~n",[Pre_Ctr_id,Reason]);

% Error in restarting the controller
print_ctr_status_change_summary(Pre_Ctr_id,{error,Reason},restart) ->
 io:format("The controller ~p raised an error in its restarting: {error,~p}~n",[Pre_Ctr_id,Reason]).
 
 
%% Prints a summary of the updated statuses of location devices
%% (print_loc_status_change_summary(Loc_id,CtrMgrStatus,DevicesStatusesChange,Mode) helper function) 

% If the location contained no devices, just notify it
print_loc_devs_statuses_change_summary({[],[],_,_},_) ->
 io:format("The location contains no devices~n");
 
% Otherwise print a summary of their statuses change
% via the "print_devs_statuses_change_summary()" function
print_loc_devs_statuses_change_summary(DevicesStatusesChange,Mode) ->
 print_devs_statuses_change_summary(DevicesStatusesChange,Mode).


%% =================================================== ALL-NODES STOP/RESTART =================================================== %%

%% Attempts to change the statuses of all nodes in the JANET simulator
%% (stop_all_nodes(), restart_all_nodes() helper function)
change_all_nodes_statuses(Mode) ->

 % Ensure the JANET Simulator to be running
 ok = utils:ensure_janet_started(),
  
 % If it is, retrieve all locations' IDs from the database
 LocIdsList = db:get_table_keys(location),
   
 case LocIdsList of
  [] ->
	
   % If the database is empty, just return
   io:format("The JANET simulator has no nodes~n");
	
  _ ->
	 
   % Otherwise attempt to change the statuses
   % of nodes in all locations as of "Mode"
   change_locs_statuses(LocIdsList,[],Mode)
 end.

   
%% Attempts to change the statuses of all nodes in a list of locations
%% (change_all_nodes_statuses(Mode) helper function)   

% If there are no more locations, print a summary of the operation
change_locs_statuses([],MgrsErrors,Mode) ->
 print_all_nodes_statuses_change_summary(MgrsErrors,Mode);  
 
% Change the statuses of all nodes in location "Loc_id" as of "Mode",
% also accumulating status changes errors in the "MgrsErrors" variable
change_locs_statuses([Loc_id|Next_LocId],MgrsErrors,Mode) ->
   
 % Retrieve the PID of location managers' 'sup_loc' supervisor
 Sup_pid = db:get_suploc_pid(Loc_id),  
   
 % Attempt to change the status of the location's controller manager as of "Mode"
 CtrMgrStatus = catch(change_ctr_status(Loc_id,Sup_pid,Mode)),  
   
 % If changing the controller's manager status resulted in an error, append it into "MgrsErrors"
 case CtrMgrStatus of
 
  {error,{internal,_}} ->
   MgrsErrorsWithCtr = lists:append(MgrsErrors,[{CtrMgrStatus,utils:prefix_node_id(controller,Loc_id)}]);
   
  _ ->
   MgrsErrorsWithCtr = MgrsErrors
 end,
  
 % Retrieve the list of devices in the location
 %
 % NOTE: Differently from "change_subloc_status()" and its "get_subloc_devs_throw()" function in
 %       this case an empty list can be returned, meaning that the location contains no devices  
 DevIdList = db:get_loc_devs(Loc_id), 
   
 % Attempt to change the statuses of all device managers in the location, obtaining in return:
 %
 %  - If a non-empty "DevIdList" was passed, the {StoppedMgrs,RunningMgrs,ChangedMgrsSuccesses,AllMgrsFails}
 %    lists (see print_loc_status_change_summary())
 %  - If an empty "DevIdList" was passed, the tuple {[],[],[],[]}
 %  - If attempting to stop or restart a list of devices that are already all
 %    stopped or running, the 'all_devs_stopped' or 'all_devs_running' atoms
 % 
 DevicesStatusesChange = catch(change_devices_statuses(DevIdList,Sup_pid,Mode)),  
   
 % If changing the location devices's managers statuses returned any error, append it into "MgrsErrorsWithCtr"
 case DevicesStatusesChange of
    
  {_,_,_,DevMgrsErrors} ->
   DevMgrsErrorsPrefixed = [ {FailReason,utils:prefix_node_id(device,Dev_id)} || {FailReason,Dev_id} <- DevMgrsErrors ],
   MgrsErrorsWithDevs = lists:append(MgrsErrorsWithCtr,DevMgrsErrorsPrefixed);
   
  _ ->
   MgrsErrorsWithDevs = MgrsErrorsWithCtr
 end,
 
 % Continue with the next location, accumulating any error in the "MgrsErrorsWithDevs" variable
 change_locs_statuses(Next_LocId,MgrsErrorsWithDevs,Mode).


%% Prints a summary of changing the statuses of all JANET nodes in the application
%% (change_locs_statuses([],MgrsErrors,Mode) helper function) 

% All stopped, no errors
print_all_nodes_statuses_change_summary([],stop) ->
 io:format("All JANET nodes are now stopped~n");

% All restarted, no errors
print_all_nodes_statuses_change_summary([],restart) ->
 io:format("All JANET nodes are now running~n");

% Some errors, the rest is stopped
print_all_nodes_statuses_change_summary(MgrsErrors,stop) ->
 io:format("The following nodes raised an error in stopping:~n"),
 print_failed_mgrs_changes(MgrsErrors),
 io:format("The remaining nodes are now stopped~n");

% Some errors, the rest is restarted
print_all_nodes_statuses_change_summary(MgrsErrors,restart) ->
 io:format("The following nodes raised an error in restarting:~n"),
 print_failed_mgrs_changes(MgrsErrors),
 io:format("The remaining nodes are now running~n").


%% ============================================ SHARED STOP/RESTART HELPER FUNCTIONS ============================================ %%
 
%% Verifies that a node's status change is valid, i.e. does not consist in stopping an already stopped or restarting an already running node
%% (change_node_status(NodeTypeShortHand,Node_id,Mode),change_ctr_status(Loc_id,Sup_pid,Mode) helper function)		
verify_node_status_change(MgrStatus,Mode) ->
 if
 
  % If attempting to stop an already stopped node, throw an error
  MgrStatus =:= "STOPPED" andalso Mode =:= stop ->
   throw({error,not_running});
	
  % If attempting to restart an already running node, throw an error
  MgrStatus =/= "STOPPED" andalso Mode =:= restart ->
   throw({error,already_running});
	 
  % Otherwise the node status change is valid
  true ->
   ok
 end.


%% Attempts to stop or restart a node by terminating or restarting its associated manager via its 'sup_loc' supervisor
%% (change_node_status(NodeTypeShortHand,Node_id,Mode),change_managers_statuses(Sup_pid,_,RunningMgrs,Mode),
%% change_ctr_status(Loc_id,Sup_pid,Mode) helper function)
change_manager_status(Sup_pid,NodeType,Node_id,stop) ->
 
 % Retrieve the node's manager prefix, being it used as ChildID under its 'sup_loc' supervisor
 ChildID = utils:prefix_node_id(NodeType,Node_id),

 % Attempt to terminate the node's manager via its 'sup_loc' supervisor
 case supervisor:terminate_child(Sup_pid,ChildID) of
 
  % If the node's manager was succesfully terminated
  ok ->
   ok;
  
  % If the 'sup_loc' supervisor doesn't recognize the manager child, there is a consistency 
  % error between its internal list of children and the 'ctrmanager' or 'devmanager' table
  {error,not_found} ->
   throw({error,{internal,unknown_suploc_child}})
 end;
	
change_manager_status(Sup_pid,NodeType,Node_id,restart) ->
 
 % Retrieve the node's manager prefix, being it used as ChildID under its 'sup_loc' supervisor
 ChildID = utils:prefix_node_id(NodeType,Node_id),
 
 % Attempt to restart the node's manager via its 'sup_loc' supervisor
 case supervisor:restart_child(Sup_pid,ChildID) of
 
  % If the node's manager was succesfully restarted
  {ok,_MgrPid} ->
   ok;
 
  % If the 'sup_loc' supervisor is already attempting to restart
  % the manager child, return the error (probable deadlock condition)
  {error,restarting} ->
   throw({error,{node_already_restarting}});
  
  % If the 'sup_loc' supervisor doesn't recognize the manager child, there is a consistency 
  % error between its internal list of children and the 'ctrmanager' or 'devmanager' table
  {error,not_found} ->
   throw({error,{internal,unknown_suploc_child}});
  
  % Other supervisor internal error
  {error,Error} ->
   throw({error,{internal,Error}})
 end. 
 
 
%% Attempts to change the statuses of a list of device nodes via their 'sup_loc' supervisor
%% (change_subloc_status({Loc_id,Subloc_id},Mode),change_loc_status(Loc_id,Mode),
%% change_locs_statuses([Loc_id|Next_LocId],MgrsErrors,Mode) helper function)
change_devices_statuses([],_,_) ->
 {[],[],[],[]};
change_devices_statuses(DevIdList,Sup_pid,Mode) ->

 % Retrieve the statuses of all device managers associated with the 'dev_id's in the "DevIdList"
 MgrsStatuses = [ {element(3,catch(db:get_manager_info(device,Dev_id))),Dev_id} || Dev_id <- DevIdList ],
	
 % Filter possible errors occured while retrieving the device managers' statuses
 MgrsStatusesFails = [ {MgrStatusFail,Dev_id} || {MgrStatusFail,Dev_id} <- MgrsStatuses, MgrStatusFail =:= 'error' ],
 MgrsStatusesFailsError = [ {{error,{internal,devmanager_missing}},Dev_id} || {_,Dev_id} <- MgrsStatusesFails],
 MgrsStatusesSuccesses = lists:subtract(MgrsStatuses,MgrsStatusesFails),

 % Derive the lists of running and stopped device managers from the "MgrsStatusesSuccesses" list
 StoppedMgrs = [ {MgrStatus,Dev_id} || {MgrStatus,Dev_id} <- MgrsStatusesSuccesses, MgrStatus=="STOPPED" ],
 RunningMgrs = lists:subtract(MgrsStatusesSuccesses,StoppedMgrs),
 
 % Verify that at least one device in the list can be stopped or restarted as of "Mode"
 ok = verify_nodes_statuses_change(StoppedMgrs,RunningMgrs,Mode),
   
 % Attempt to stop the RunningMgrs or restart the StoppedMgrs as of "Mode"
 ChangedMgrs = change_managers_statuses(Sup_pid,StoppedMgrs,RunningMgrs,Mode),
   
 % Filter possible errors occured while changing the device managers' statuses
 ChangedMgrsFails = [ {ChangeStatus,Dev_id} || {ChangeStatus,Dev_id} <- ChangedMgrs, ChangeStatus =/= 'ok' ],
 ChangedMgrsSuccesses = [ Dev_id || {_,Dev_id}<-lists:subtract(ChangedMgrs,ChangedMgrsFails) ],   
 
 % Derive the list of managers which raised an error in the operation (either in retrieving or changing their status)
 AllMgrsFails = lists:append(MgrsStatusesFailsError,ChangedMgrsFails),
 
 % Return the following lists:
 %
 %  - StoppedMgrs:          The list of device managers in the "DevIdList" that were already stopped
 %  - RunningMgrs:          The list of device managers in the "DevIdList" that were running
 %  - ChangedMgrsSuccesses: The list of device managers in the "DevIdList" that were successfully restarted or stopped as of mode
 %  - AllMgrsFails:          The list of device managers in the "devIdList" that raised errors in retrieving or changing their status  
 {StoppedMgrs,RunningMgrs,ChangedMgrsSuccesses,AllMgrsFails}.  
   
  
%% Prints a summary of the statuses change operation of multiple devices 
%% (change_subloc_status({Loc_id,Subloc_id},Mode),print_loc_devs_statuses_change_summary(DevicesStatusesChange,Mode) helper function)
print_devs_statuses_change_summary({AlreadyStoppedMgrs,_,SuccessStoppedMgrs,FailedStoppedMgrs},stop) ->
 if
 
  % If one or more devices were successfully stopped
  length(SuccessStoppedMgrs) > 0 ->
   
   % Prefix all devices that were successfully stopped and print them
   SuccessStoppedMgrsStr = [ utils:prefix_node_id(device,Dev_id) || Dev_id <- SuccessStoppedMgrs ],
   io:format("The following devices were successfully stopped: ~p~n",[SuccessStoppedMgrsStr]);
 
  true ->
   ok
 end,
 
 if
  
  % If one or more devices were already stopped
  length(AlreadyStoppedMgrs) > 0 ->
  
   % Prefix all devices that were already stopped and print them
   AlreadyStoppedMgrsStr = [ utils:prefix_node_id(device,Dev_id) || {_,Dev_id} <- AlreadyStoppedMgrs ],
   io:format("The following devices were already stopped: ~p~n",[AlreadyStoppedMgrsStr]);
   
  true ->
   ok
 end,
 
 if
 
  % If one or more devices raised an error while stopping
  length(FailedStoppedMgrs) > 0 ->
  
   % Prefix all devices that raised an error while stopping and print them via the print_failed_mgrs_changes() function
   FailedStoppedMgrsStr = [ {FailReason,utils:prefix_node_id(device,Dev_id)} || {FailReason,Dev_id} <- FailedStoppedMgrs ],
   io:format("The following devices raised an error in stopping:~n"),
   print_failed_mgrs_changes(FailedStoppedMgrsStr);
   
  true ->
   ok
 end;
 
print_devs_statuses_change_summary({_,AlreadyRunningMgrs,SuccessRestartMgrs,FailedRestartMgrs},restart) ->
 if
 
  % If one or more devices were successfully restarted
  length(SuccessRestartMgrs) > 0 ->
  
   % Prefix all devices that were successfully restarted and print them
   SuccessRestartMgrsStr = [ utils:prefix_node_id(device,Dev_id) || Dev_id <- SuccessRestartMgrs ],
   io:format("The following devices were successfully restarted: ~p~n",[SuccessRestartMgrsStr]);
   
  true ->
   ok
 end,
 
 if
 
  % If one or more devices were already running
  length(AlreadyRunningMgrs) > 0 ->
  
   % Prefix all devices that were already running and print them
   AlreadyRunningMgrsStr = [ utils:prefix_node_id(device,Dev_id) || {_,Dev_id} <- AlreadyRunningMgrs ],
   io:format("The following devices were already running: ~p~n",[AlreadyRunningMgrsStr]);
   
  true ->
   ok
 end,
 
 if
 
  % If one or more devices raised an error while stopping
  length(FailedRestartMgrs) > 0 ->
  
   % Prefix all devices that raised an error while stopping and print them via the print_failed_mgrs_changes() function
   FailedRestartMgrsStr = [ {FailReason,utils:prefix_node_id(device,Dev_id)} || {FailReason,Dev_id} <- FailedRestartMgrs ],
   io:format("The following devices raised an error in restarting:~n"),
   print_failed_mgrs_changes(FailedRestartMgrsStr);
   
  true ->
   ok
 end.
 
 
%% Prints the prefixed "Node_id" and reason of each of a list of manager status change failures
%% (print_devs_statuses_change_summary(StoppedMgrs,RunningMgrs,ChangedMgrsSuccesses,AllMgrsFails,Mode),
%% print_all_nodes_statuses_change_summary(MgrsErrors,stop) helper function)
print_failed_mgrs_changes([]) ->
 ok; 
print_failed_mgrs_changes([{Reason,Node_id}|NextFailedMgr]) ->
 io:format(" - ~p: ~p~n",[Node_id,Reason]),
 print_failed_mgrs_changes(NextFailedMgr).
 
 
%% Attempts to change the statuses of a set of device managers via their 'sup_loc' supervisor
%% (change_devices_statuses(DevIdList,Sup_pid,Mode) helper function)
change_managers_statuses(Sup_pid,_,RunningMgrs,stop) ->
 [ {catch(change_manager_status(Sup_pid,device,Dev_id,stop)),Dev_id} || {_,Dev_id} <- RunningMgrs ];
change_managers_statuses(Sup_pid,StoppedMgrs,_,restart) ->
 [ {catch(change_manager_status(Sup_pid,device,Dev_id,restart)),Dev_id} || {_,Dev_id} <- StoppedMgrs ].
 

%% Verifies that at least one node in the list can be stopped or restarted depending on "Mode"
%% (change_devices_statuses(DevIdList,Sup_pid,Mode) helper function)
verify_nodes_statuses_change(_,RunningMgrs,stop) ->
 if 
 
  % If attempting to stop a list of nodes that are all already stopped
  length(RunningMgrs) =:= 0 ->
   throw(all_devs_stopped);
   
  % Otherwise the nodes' statuses change operation can continue
  true ->
   ok
 end;
 
verify_nodes_statuses_change(StoppedMgrs,_,restart) ->
 if 
 
  % If attempting to restart a list of nodes that are all already running
  length(StoppedMgrs) =:= 0 ->
   throw(all_devs_running);
   
  % Otherwise the nodes' statuses change operation can continue
  true ->
   ok
 end. 
 
 
%% Attempts to change the status of a controller manager (change_loc_status(Loc_id,Mode),
%% change_locs_statuses([Loc_id|Next_LocId],MgrsErrors,Mode) helper function)
change_ctr_status(Loc_id,Sup_pid,Mode) ->

 % Retrieve the controller manager status
 {_,_,MgrStatus} = db:get_manager_info(controller,Loc_id),

 % Verify the controller manager status change to be valid (i.e. not attempting
 % to stop an already stopped or restart an already running controller)
 ok = verify_node_status_change(MgrStatus,Mode),

 % Attempt to change the manager's status via its 'sup_loc' supervisor as of "Mode"
 ok = change_manager_status(Sup_pid,controller,Loc_id,Mode),

 % Return that the operation was successful and the updated controller manager status
 {ok,Mode}. 
 

%%====================================================================================================================================
%%                                            SIMULATION UTILITY PRIVATE HELPER FUNCTIONS                                                        
%%====================================================================================================================================

%% =============================================== RUNNING AND STOPPED NODES INFO =============================================== %%

%% Prints the lists of IDs of all stopped and/or running node managers
%% (print_nodes(),print_nodes(all),print_nodes(Status) helper function)
print_managers(all) ->

 % Ensure the JANET Simulator to be running
 ok = utils:ensure_janet_started(),
  
 % If it is, retrieve all records from the 'ctrmanager' and 'devmanager' tables
 CtrMgrRecords = db:get_table_records(ctrmanager),
 DevMgrRecords = db:get_table_records(devmanager),
   
 % Determine the sorted IDs of all stopped and running controller and device managers
 StoppedCtrManagers = lists:sort([ Loc_id || {_,Loc_id,_,MgrStatus} <- CtrMgrRecords, MgrStatus =:= "STOPPED" ]),
 StoppedDevManagers = lists:sort([ Dev_id || {_,Dev_id,_,_,MgrStatus} <- DevMgrRecords, MgrStatus =:= "STOPPED" ]),
 RunningCtrManagers = lists:sort([ Loc_id || {_,Loc_id,_,MgrStatus} <- CtrMgrRecords, MgrStatus =/= "STOPPED" ]),
 RunningDevManagers = lists:sort([ Dev_id || {_,Dev_id,_,_,MgrStatus} <- DevMgrRecords, MgrStatus =/= "STOPPED" ]),
   
 % Print the IDs of all stopped and running controller and device managers
 io:format("~nSTOPPED NODES~n============="),
 print_mgrs_list("Controllers:",StoppedCtrManagers),
 print_mgrs_list("Devices:    ",StoppedDevManagers),
 io:format("~n"),
 io:format("~nRUNNING NODES~n============="),
 print_mgrs_list("Controllers:",RunningCtrManagers),
 print_mgrs_list("Devices:    ",RunningDevManagers),
 io:format("~n~n");

print_managers(Status) ->

 % Ensure the JANET Simulator to be running
 ok = utils:ensure_janet_started(),
  
 % If it is, retrieve all records from the 'ctrmanager' and 'devmanager' tables
 CtrMgrRecords = db:get_table_records(ctrmanager),
 DevMgrRecords = db:get_table_records(devmanager),
   
 % Depending on the specified Status of node managers
 case Status of
  stopped ->
	
   % Determine the sorted IDs of all stopped controller and device managers
   StatusCtrManagers = lists:sort([ Loc_id || {_,Loc_id,_,MgrStatus} <- CtrMgrRecords, MgrStatus =:= "STOPPED" ]),
   StatusDevManagers = lists:sort([ Dev_id || {_,Dev_id,_,_,MgrStatus} <- DevMgrRecords, MgrStatus =:= "STOPPED" ]),
	 
   % Print results header
   io:format("~nSTOPPED NODES~n=============");
	 
  running ->

   % Determine the sorted IDs of all running controller and device managers
   StatusCtrManagers = lists:sort([ Loc_id || {_,Loc_id,_,MgrStatus} <- CtrMgrRecords, MgrStatus =/= "STOPPED" ]),
   StatusDevManagers = lists:sort([ Dev_id || {_,Dev_id,_,_,MgrStatus} <- DevMgrRecords, MgrStatus =/= "STOPPED" ]),
	 
   % Print results header
   io:format("~nRUNNING NODES~n=============")
 end,
   
 % Print the IDs of all controller and device managers in the specified Status
 print_mgrs_list("Controllers:",StatusCtrManagers),
 print_mgrs_list("Devices:    ",StatusDevManagers),
 io:format("~n~n").
 


%% Prints a list of node managers' statuses (print_managers(all), print_managers(Status) helper function)
print_mgrs_list(StrHeader,MgrsList) ->
 io:format("~n~s ",[StrHeader]),
 if
  length(MgrsList) > 0 ->
   io:format("~p",[MgrsList]);
  true ->
   io:format("(none)")
 end.
 
 
%% ================================================ CONTROLLER NODES INTERACTION  ================================================ %%

%% Attempts to synchronously execute a command on a running controller node by forwarding the
%% request to its manager, returning the result of the operation (print_ctr_table(Loc_id,Table),
%% print_ctr_tree(Loc_id), ctr_command(Loc_id,Module,Function,ArgsList) helper function)
gen_ctr_command(Loc_id,Module,Function,ArgsList) ->

 % Ensure the JANET Simulator to be running
 ok = utils:ensure_janet_started(),
  
 % Retrieve the controller's manager PID and status
 {_,CtrMgrPid,CtrMgrStatus} = db:get_manager_info(controller,Loc_id),
 case CtrMgrStatus of
  "STOPPED" ->
	
   % If the manager and thus the controller node
   % is stopped, the command cannot be forwarded
   throw({error,node_stopped});
	 
  _ ->
	
   % Otherwise forward the command to the controller node's
   % manager and wait for a response up to a predefined timeout
   try gen_server:call(CtrMgrPid,{ctr_command,Module,Function,ArgsList},5000)
   catch
    exit:{timeout,_} ->
	   
     % Command timeout
	 throw({error,request_timeout})
   end	   
 end.


%% ================================================== DEVICE NODES INTERACTION  ================================================== %%

%% Attempts to synchronously change the configuration of a running device node by forwarding the request to 
%% its manager, returning the result of the operation (dev_config_change(Dev_id,Config) helper function)
gen_dev_config_change(Dev_id,Config) ->

 % Ensure the JANET Simulator to be running
 ok = utils:ensure_janet_started(),

 % Retrieve the device's manager PID and status
 {_,DevMgrPid,DevMgrStatus} = db:get_manager_info(device,Dev_id),
 case DevMgrStatus of
  "STOPPED" ->
	
   % If the manager and thus the device node
   % is stopped, the command cannot be forwarded
   throw({error,node_stopped});
	 
  _ ->
	
   % Otherwise retrieve the device's record from the 'device' table
   %
   % NOTE: The device exists for sure at this point, since otherwise the
   %       db:get_manager_info(device,Dev_id) function would have raised a throw
   {ok,DevRecord} = db:get_record(device,Dev_id),
	 
   % Attempt to build the new configurationto be
   % applied to the device depending on its type
   DevCfg = utils:build_dev_config_wildcard(Config,DevRecord#device.type),
	 
   % Forward the configuration change command to the device node's
   % manager and wait for a response up to a predefined timeout
   CfgChangeRes = try gen_server:call(DevMgrPid,{dev_config_change,DevCfg},5000)
   catch
	exit:{timeout,_} ->
	   
	% Command timeout
	throw({error,request_timeout})
   end,
	 
   % Depending on the result of the operation
   case CfgChangeRes of
	  
   % If the operation was successful, format the received
   % Timestamp as a date before returning it to the user
   {ok,{UpdatedCfg,Timestamp}} ->
	{ok,{UpdatedCfg,string:slice(calendar:system_time_to_rfc3339(Timestamp,[{time_designator,$\s}]),0,19)}};
	   
   % Otherwise if an error was raised, simply return it to the user
   _ ->
	CfgChangeRes
  end
 end.


%% Attempts to synchronously execute a command on a running device node by forwarding the request to its
%% manager, returning the result of the operation (dev_command(Loc_id,Module,Function,ArgsList) helper function)
gen_dev_command(Dev_id,Module,Function,ArgsList) ->

 % Ensure the JANET Simulator to be running
 ok = utils:ensure_janet_started(),
  
 % Retrieve the device's manager PID and status
 {_,DevMgrPid,DevMgrStatus} = db:get_manager_info(device,Dev_id),
 case DevMgrStatus of
  "STOPPED" ->
	
   % If the manager and thus the device node
   % is stopped, the command cannot be forwarded
   throw({error,node_stopped});
	 
  _ ->
	
   % Otherwise forward the command to the device node's manager
   % and wait for a response up to a predefined timeout
   try gen_server:call(DevMgrPid,{dev_command,Module,Function,ArgsList},5000)
   catch
    exit:{timeout,_} ->
	   
	 % Command timeout
	 throw({error,request_timeout})
   end	   
 end.

 
%%====================================================================================================================================
%%                                             APPLICATION BEHAVIOUR CALLBACK FUNCTIONS                                                        
%%====================================================================================================================================

%% Starts the JANET Simulator
start(normal,_Args) ->
 sup_jsim:start_link().
 
%% Called once the JANET Simulator has been stopped
stop(_State) ->
 ok.