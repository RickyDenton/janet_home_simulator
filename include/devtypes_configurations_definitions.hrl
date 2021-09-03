%%====================================================================================================================================%%
%%                                              DEVICE TYPES CONFIGURATIONS DEFINITIONS                                               %%
%%====================================================================================================================================%%

%% Fan ('fan')
-record(fancfg,    
        {
		 onoff,         % 'onoff' trait ('on'|'off')
		 fanspeed       % 'fanspeed' trait (0 < fanspeed <= 100)
		}).

%% Light ('light')
-record(lightcfg,    
        {
		 onoff,         % 'onoff' trait ('on'|'off')
         brightness,    % 'brightness' trait (0 < brightness <= 100)
         colorsetting   % 'colorsetting' trait (any)
		}).

%% Door ('door')
-record(doorcfg,    
        {
		 openclose,     % 'openclose' trait
         lockunlock     % 'lockunlock' trait
		}).             % ({'open' && 'unlock'} | {'close' && ('lock' || 'unlock'))

%% Thermostat ('thermostat')
-record(thermocfg,    
        {
		 onoff,         % 'onoff' trait ('on'|'off')
		 temp_target,   % 'temp_target' trait (0 <= temp_target <= 50)	
		 temp_current   % 'temp_current' trait (NOTE: the interval range is NOT checked)
		}).
		
%% Conditioner ('conditioner')
-record(condcfg,    
        {
		 onoff,         % 'onoff' trait ('on'|'off')
		 temp_target,   % 'temp_target' trait (0 <= temp_target <= 50)
		 temp_current,  % 'temp_current' trait (NOTE: the interval range is NOT checked)
         fanspeed       % 'fanspeed' trait (0 < fanspeed <= 100)
		}).