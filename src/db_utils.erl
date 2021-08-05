-module(db_utils).
-export([reset/0,info/0,addlocation/4,findlocation/1]).

%%-- Mnesia Records/Tables Definitions --- %% 
-record(location,{id,port,status,name=[],user=[],devlist=[],sublocations=[]}).
-record(device,{id,type,locationID,sublocationIDX,config,name=[]}).


%%-- Other Utility Functions --- %% 

% Resets the Mnesia database (WARNING!)
reset() ->
 case jsim:is_running(janet_simulator) of
  true ->
   io:format("Please stop the JANET Simulator first~n");
  false ->
   
   % Read the number of records in the Location and the Device table so to warn the user
   jsim:start_mnesia(),
   {N_Locations,N_Devices} = get_table_keys_num(all),
   Reset = if
            N_Locations + N_Devices > 0 ->
	         io:format("WARNING: ~p location(s) and ~p device(s) are currently stored in the Mnesia database, deleting them may generate inconsistencies with the remote database.~n",[N_Locations,N_Devices]),
	         {ok,[Ans]} = io:fread("Are you sure you want to proceed? (y/N): ","~s"),
	         if
			  Ans =:= "y"; Ans =:= "Y" ->
			   reset;
	          true ->
	           not_reset
	         end;
           true ->
            reset
          end,
   case Reset of
    reset ->                             % NOTE: The schema in fact cannot be dropped using the mnesia:delete_schema([node()]) and/or mnesia:create_schema([node()]), thus only tables are reset
     mnesia:delete_table(location),
	 mnesia:delete_table(device),
	 mnesia:create_table(location,
                     [{attributes, record_info(fields,location)},
					  {index, [#location.user]},
					  {disc_copies,[node()]}]),
     mnesia:create_table(device,
                     [{attributes, record_info(fields,device)},
					  {disc_copies,[node()]}]),
     application:stop(mnesia),
	 io:format("Mnesia database successully reset~n");
	_ ->
	 aborted
   end
 end.

info() ->
 PrevStatus = jsim:is_running(mnesia),
 if
  PrevStatus =:= false ->
   jsim:start_mnesia();
  true ->
   ok
 end,
 {LocationKeys,DeviceKeys} = get_table_keys(all),
 if
  PrevStatus =:= false ->
   jsim:stop_mnesia();
  true ->
   ok
 end,
 io:format("~p location(s) and ~p device(s) are currently stored in the database~n",[length(LocationKeys),length(DeviceKeys)]).
 

%%-- Keys Utility Functions --- %% 

% Retrieve the keys of all or a specific table
get_table_keys(all) ->
 F = fun() -> {mnesia:all_keys(location),mnesia:all_keys(device)} end,
 {atomic,{LocationKeys,DeviceKeys}} = mnesia:transaction(F),
 {LocationKeys,DeviceKeys};
get_table_keys(Table) ->
 F = fun() -> {mnesia:all_keys(Table)} end,
 {atomic,Keys} = mnesia:transaction(F),
 Keys.

% Returns the number of keys of all or a specific table
get_table_keys_num(all) ->
 {LocationKeys,DeviceKeys} = get_table_keys(all),
 {length(LocationKeys),length(DeviceKeys)};
get_table_keys_num(Table) ->
 Keys = get_table_keys(Table),
 length(Keys).








%%-- CRUD Functions --- %%  

% -----------------------------------  TODO!!!!!!!!!!!!!

addlocation(Id,Port,Name,User) ->
 F = fun() ->
	 mnesia:write(#location{id=Id,port=Port,status=offline,name=Name,user=User,devlist=[],sublocations=[]})
	 end,
 mnesia:transaction(F).
 
findlocation(Id) ->
 F = fun() -> mnesia:read({location,Id}) end,
 case mnesia:transaction(F) of
  {atomic,[]} ->
   undefined;
  {atomic,[#location{id=I,port=P,status=ST,name=N,user=U,sublocations=SU}]} ->
   {I,P,ST,N,U,SU}
  end.