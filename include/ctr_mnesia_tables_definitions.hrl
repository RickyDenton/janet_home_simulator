%%====================================================================================================================================%%
%%                                   JANET CONTROLLER MNESIA TABLES RECORDS DEFINITIONS (ram_copies)                                  %%
%%====================================================================================================================================%%

%% Information on a location's sublocation
-record(ctr_sublocation,
        {
		 subloc_id,     % The ID of the sublocation within the location (sub_id = {loc_id,subloc_id}) 
         devlist=[]     % The list of 'dev_id's of devices belonging to the sublocation
        }).		 
		
%% Information on a location's device
-record(ctr_device,
        {
		 dev_id,         % The device's 'dev_id'
		 subloc_id,      % The ID of the sublocation within the location the device belongs to
		 type,           % The device's type (fan|light|door|thermostat|conditioner)
		 config,         % % The device's current configuration (or state) (type-specific)
		 lastupdate,     % The timestamp of when the device's configuration was last updated (UNIX time)
		 handler_pid     % The PID of the 'dev_handler' process assigned to the device node, if any
		}).