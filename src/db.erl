-module(db).
-export([reset/0,info/0]).

%%-- Mnesia Records/Tables Definitions --- %% 
% -record(location,{id,port,status,name=[],user=[],devlist=[],sublocations=[]}).
% -record(device,{id,type,locationID,sublocationIDX,config,name=[]}).

-record(location,     	% A location, consisting in a controller and a list of sublocations (DISCOPIES)
        {
		 locid,       	% The location/controller's ID (must be unique)
		 name,        	% The location's name          (optional)
		 user,        	% The location's user
		 port,        	% The location controller's port for REST operations (must be unique)
		 sublocs=[]   	% The location's sublocations list (at least the default sublocation '0' must be present)
		}).

-record(sublocation, 	% A sublocation, containing a set of devices (DISCOPIES)
        {
		 sublocid,   	% The sublocation's ID (0 = default sublocation)
		 name,       	% The sublocation's name
		 locid,         % The ID of the location the sublocation belongs to (redundancy)
		 devices=[]  	% The list of devices in the sublocation
		}).
		
-record(device,      	% A device, characterized by its type, status, and the sublocation/location it is deployed in (DISCOPIES)
        {
		 devid,      	% The device's ID
		 type,       	% The device's type (light, fan, door, thermostat, heater)
		 locid,	     	% The ID of the location the device is deployed in (redundancy)
		 sublocid,	 	% The ID of the sublocation the device is deployed in (redundancy)
		 status      	% The device's status (type-specific)
		}).

-record(devstate,    	% A device's current state (controller included) (ETS)
        {
		 devid,      	% The device's ID
		 state       	% The device's state (initializing, online, offline)
		}).
		
-record(loctable,  	  	% A table holding the PIDs of the location supervisor and controller manager of each location (ETS)
        {
		 locid,     	% The location's ID
         sup_loc_pid,	% The PID of the location's supervisor
		 ctrl_mgr_pid	% The PID of the controller's manager
        }).

%%-- Other Utility Functions --- %% 

% Resets the Mnesia database (WARNING!)
reset() ->

 % Ensure the JANET simulator to be stopped
 case jsim:is_running(janet_simulator) of
  true ->
   io:format("Please stop the JANET Simulator first~n");
  false ->
  
   % Ask user confirmation
   {ok,[Ans]} = io:fread("Resetting the local Mnesia database may cause inconsistencies with the remote database. Are you sure you want to proceed? (y/N): ","~s"),
   if
   
    % If positive, reset the Mnesia database
    Ans =:= "y"; Ans =:= "Y" ->
     application:stop(mnesia),
	 application:set_env(mnesia,dir,"priv/mnesia.db"),
	 mnesia:delete_schema([node()]),
	 mnesia:create_schema([node()]),
	 application:start(mnesia),
	 mnesia:create_table(location,
                     [{attributes, record_info(fields, location)},
                     {index, [#location.user]},
                     {disc_copies, [node()]}]),				 
     mnesia:create_table(sublocation,
                     [{attributes, record_info(fields, sublocation)},
                     {index, [#sublocation.locid]},
                     {disc_copies, [node()]}]),		 
     mnesia:create_table(device,
                     [{attributes, record_info(fields, device)},
                     {index, [#device.locid,#device.sublocid]},
                     {disc_copies, [node()]}]),	 
     mnesia:create_table(devstate,
                     [{attributes, record_info(fields, devstate)},
                     {ram_copies, [node()]}]),
     mnesia:create_table(loctable,
                     [{attributes, record_info(fields, loctable)},
                     {ram_copies, [node()]}]),
	 application:stop(mnesia),
	 io:format("Mnesia database successully reset~n");
	 
	% Otherwise, abort the operation
	true ->
	 aborted
  end
 end.
	 

% Database info
info() ->
 PrevStatus = jsim:is_running(mnesia),
 if
  PrevStatus =:= false ->
   jsim:start_mnesia();
  true ->
   ok
 end,
  timer:sleep(1),      % This sleep is required to solve a race condition in Mnesia loading the sublocation scheme (even if wait_for_tables/2 is used...)
  LocationKeysNum = get_table_keys_num(location),
  SublocationKeysNum = get_table_keys_num(sublocation),
  DeviceKeysNum = get_table_keys_num(device),
 if
  PrevStatus =:= false ->
   jsim:stop_mnesia();
  true ->
   ok
 end,
 io:format("~nDATABASE CONTENTS~n=================~n- ~p location(s)~n- ~p sublocation(s)~n- ~p device(s)~n",[LocationKeysNum,SublocationKeysNum,DeviceKeysNum]).
 

%%-- Keys Utility Functions --- %% 

get_table_keys(Table) ->
 F = fun() -> {mnesia:all_keys(Table)} end,
 {atomic,{Keys}} = mnesia:transaction(F),
 Keys.

get_table_keys_num(Table) ->
 Keys = get_table_keys(Table),
 length(Keys).
 

% Retrieve the keys of all or a specific table
%get_table_keys(all) ->
% F = fun() -> {mnesia:all_keys(location),mnesia:all_keys(device)} end,
% {atomic,{LocationKeys,DeviceKeys}} = mnesia:transaction(F),
% {LocationKeys,DeviceKeys};
%get_table_keys(Table) ->
% F = fun() -> {mnesia:all_keys(Table)} end,
% {atomic,Keys} = mnesia:transaction(F),
% Keys.

% Returns the number of keys of all or a specific table
%get_table_keys_num(all) ->
% {LocationKeys,DeviceKeys} = get_table_keys(all),
% {length(LocationKeys),length(DeviceKeys)};
%get_table_keys_num(Table) ->
% Keys = get_table_keys(Table),
% length(Keys).








%%-- CRUD Functions --- %%  

% -----------------------------------  TODO!!!!!!!!!!!!!

%addlocation(Id,Port,Name,User) ->
% F = fun() ->
%	 mnesia:write(#location{id=Id,port=Port,status=offline,name=Name,user=User,devlist=[],sublocations=[]})
%	 end,
% mnesia:transaction(F).
 
%findlocation(Id) ->
% F = fun() -> mnesia:read({location,Id}) end,
% case mnesia:transaction(F) of
%  {atomic,[]} ->
%   undefined;
%  {atomic,[#location{id=I,port=P,status=ST,name=N,user=U,sublocations=SU}]} ->
%   {I,P,ST,N,U,SU}
%  end.