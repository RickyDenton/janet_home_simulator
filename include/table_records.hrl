%%====================================================================================================================================%%
%%                                               MNESIA TABLES RECORDS DEFINITIONS                                                    %%
%%====================================================================================================================================%%

%% --- disc_copies tables --- %%

-record(location,     	% A location
        {
		 loc_id,       	% The location/controller's ID (must be unique)
		 name,        	% The location's name (optional)
		 user,        	% The location's user
		 port        	% The location controller's port for REST operations (must be unique)
		}).

-record(sublocation, 	% A sublocation
        {
		 sub_id,     	% The sublocation's full ID {loc_id,subloc_id} ({X,0} = default sublocation of location X)
		 name,       	% The sublocation's name (optional)
		 devlist=[]     % The list of devices in the sublocation
		}).
		
-record(device,      	% A device
        {
		 dev_id,      	% The device's ID
		 name,          % The device's name (optional)
		 sub_id,        % The full ID {loc_id,subloc_id} of the sublocation the device is deployed in
		 type,       	% The device's type (light, fan, door, thermostat, heater)
		 config      	% The device's configuration (type-specific)
		}).

%% --- ram_copies tables --- %%

-record(suploc,         % The PID of a location sup_loc supervisor
        {
		 loc_id,        % The location's ID
         sup_pid        % The PID of the location's sup_loc supervisor
        }).		 
		
-record(ctrmanager,     % A location controller's manager
        {
		 loc_id,        % The location's ID
		 sup_pid,       % The PID of the location controller's manager
		 status         % The controller's status (on,off)
		}).

-record(devmanager,     % A device's manager
        {
		 dev_id,        % The device's ID
		 loc_id,        % The device's location ID
		 sup_pid,       % The PID of the device's manager
		 status         % The device's status (on,off)
		}).    