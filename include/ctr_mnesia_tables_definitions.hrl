%%====================================================================================================================================%%
%%                                   JANET CONTROLLER MNESIA TABLES RECORDS DEFINITIONS (ram_copies)                                  %%
%%====================================================================================================================================%%

%% The list of devices belonging to a sublocation in the location
%%
%% NOTE: The additional 'all' key is used for storing the complete list of location devices
%%
-record(devalloc,
        {
		 subloc_id,     % The subloc_id of a sublocation in the location
         devlist=[]     % The list of 'dev_id's of devices belonging to such sublocation
        }).		 
		
%% Information on a device registered within the controller	
-record(devregister,
        {
		 dev_id,         % The device's 'dev_id'
		 handler_pid     % The PID of the device handler in the Janet Controller
		}).