%% This module represents the top-level supervisor of the Janet Device application %%

-module(sup_jdev).
-behaviour(supervisor).

-export([init/1]).        % Supervisor Behaviour Callback Function
-export([start_link/0]).  % Start Function

%%====================================================================================================================================
%%                                                SUPERVISOR INIT CALLBACK FUNCTION                                                        
%%====================================================================================================================================
init(_) ->

 % Retrieve the 'type' and 'config' environment variables
 {ok,DevType} = application:get_env(type),
 {ok,Config} = application:get_env(config),

 % Ensure the device configuration to be valid according to its type
 ok = utils:validate_dev_config(Config,DevType),

 % Determine the Erlang module to be used for instantiating the device 'dev_statem' from its Type
 Dev_statem_module = get_statem_module(DevType),
 
 % Set the child specification of the device's 'dev_statem' module
 Dev_statem_specs = {
                     dev_statem,		                        % ChildID
                     {Dev_statem_module,start_link,[Config]},   % Child Start Function
 	                 permanent,                                 % Child Restart Policy 
	                 2000,                                      % Child Sub-tree Max Shutdown Time
	                 worker,                                    % Child Type
	                 [Dev_statem_module]                        % Child Modules (For Release Handling Purposes)
                    },

 % Return the supervisor flags and the list of children specifications
 {ok,
  {
   %% ==================================================== SUPERVISOR FLAGS ==================================================== %%
   {
    %% [TODO]: Currently In the devices if something goes wrong the entire node must be shut down and recreated, since the environment variables may be outdated (think if this should be the case)
	one_for_all,  % RestartStrategy
	0,            % MaxRestarts
	1             % TimePeriod for MaxRestarts
   },
   
   %% =========================================== SUPERVISOR CHILDREN SPECIFICATIONS =========================================== %%
   [
    %% --- The gen_statem relative to the device type (jfan,jlight,jdoor,jheater or jthermostat) --- %%
    Dev_statem_specs,
   
    %% ------------------- The device's communication server (dev_server) ------------------- %%
    {
     dev_server,		             % ChildID
     {dev_server,start_link,[]},     % Child Start Function
 	 permanent,                      % Child Restart Policy
	 4500,                           % Child Sub-tree Max Shutdown Time
	 worker,                  	     % Child Type
	 [dev_server]                    % Child Modules (For Release Handling Purposes)
    }
   ] 
  }
 }.


%% Returns the name of the 'dev_statem' module associated with a device type (init(_) helper function)
get_statem_module(DevType) ->
 case DevType of
 
  % Return the name of the 'dev_statem' module associated with the device type
  fan ->
   jfan;
  light ->
   jlight;
  door ->
   jdoor;
  heater ->
   jheater;
  thermostat ->
   jthermostat;
   
  % If unknown device type, raise a throw exception (this must not happen at this point)
  _ ->
   throw(unknown_device)
 end.   


%%====================================================================================================================================
%%                                                         START FUNCTION                                                        
%%====================================================================================================================================

%% Called by the application_master at boot time
start_link() ->
 supervisor:start_link(?MODULE,[]).