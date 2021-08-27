%%====================================================================================================================================%%
%%                                         JANET SIMULATOR MNESIA TABLES RECORDS DEFINITIONS                                          %%
%%====================================================================================================================================%%

%% ===================================================== DISC_COPIES TABLES ===================================================== %% 

%% Location Information
-record(location,     	
        {
		 loc_id,       	% The location ID                            (primary key)
		 name,        	% The location's name                        (optional)
		 user,        	% The location's user                        (optional)
		 port        	% The location controller's REST server port (must be unique)
		}).

%% Sublocation Information
-record(sublocation,
        {
		 sub_id,     	% The sublocation's full ID {loc_id,subloc_id} ({X,0} = default sublocation of location X)
		 name,       	% The sublocation's name                       (optional)
		 devlist=[]     % The list of devices in the sublocation
		}).

%% Device Information		
-record(device,
        {
		 dev_id,      	% The device's ID
		 name,          % The device's name                                        (optional)
		 sub_id,        % The full ID of the sublocation the device is deployed in ({loc_id,subloc_id})
		 type,       	% The device's type                                        (currently supported: [light,fan,door,thermostat,heater])
		 config      	% The device's configuration                               (type-specific)
		}).


%% ====================================================== RAM_COPIES TABLES ====================================================== %% 

%% The PID of a location's 'sup_loc' supervisor
-record(suploc,
        {
		 loc_id,        % The location ID
         sup_pid        % The PID of the location's 'sup_loc' supervisor
        }).		 
		
%% Controller node manager information		
-record(ctrmanager,
        {
		 loc_id,        % The location ID
		 mgr_pid,       % The PID of the controller node's manager
		 status         % The controller node's status ("BOOTING" | "ONLINE" | "STOPPED")
		}).

%% Device node manager information
-record(devmanager,
        {
		 dev_id,        % The device ID
		 loc_id,        % The ID of the location the device belongs to
		 mgr_pid,       % The PID of the device node's manager
		 status         % The device node's status ("BOOTING" | "CONNNECTING" | "ONLINE" | "STOPPED")
		}).    