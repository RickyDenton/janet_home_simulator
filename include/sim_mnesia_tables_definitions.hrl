%%====================================================================================================================================%%
%%                                         JANET SIMULATOR MNESIA TABLES RECORDS DEFINITIONS                                          %%
%%====================================================================================================================================%%

%% ===================================================== DISC_COPIES TABLES ===================================================== %% 

%% Location Information
-record(location,     	
        {
		 loc_id,       	% The location ID                                         (integer, primary key)
		 name,        	% The location's name                                     (optional)
		 user,        	% The location's user                                     (optional)
		 port,        	% The location controller's REST server port              (>= 30000, must be unique)
		 hostname       % The name of the host where to spawn the controller node (a list, must belong to the 'allowed_node_hosts' environment variable)
		}).

%% Sublocation Information
-record(sublocation,
        {
		 sub_id,     	% The sublocation's full ID {loc_id,subloc_id} ({integer,integer}, primary key)
		 name,       	% The sublocation's name                       (optional)
		 devlist=[]     % The list of devices in the sublocation
		}).

%% Device Information		
-record(device,
        {
		 dev_id,      	% The device's ID                                                   (integer, primary key)
		 name,          % The device's name                                                 (optional)
		 sub_id,        % The full ID of the sublocation the device is deployed in          ({loc_id,subloc_id})
		 type,       	% The device's type                                                 (light|fan|door|thermostat|conditioner)
		 config,      	% The device's current configuration (or state)                     (type-specific)
		 lastupdate,    % The timestamp of when the device's configuration was last updated (UNIX time)
		 hostname       % The name of the host where to spawn the device node               (a list, must belong to the 'allowed_node_hosts' environment variable)
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