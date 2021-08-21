%% This module represents the top-level supervisor of the Janet Device application %%

-module(sup_jdev).
-behaviour(supervisor).

-export([init/1]).        % Supervisor Behaviour Callback Function
-export([start_link/0]).  % Start Function

%%====================================================================================================================================
%%                                                SUPERVISOR INIT CALLBACK FUNCTION                                                        
%%====================================================================================================================================
init(_) ->

 % Retrieve the 'type' environment variable and use it for determing the FSM module to be created
 {ok,DevType} = application:get_env(type),
 FSM_Module = get_fsm_module(DevType),
 
 % Set the specifications of the FSM child using the FSM module
 FSM_Specs = {
              dev_fsm,		                  % ChildID
              {FSM_Module,start_link,[]},     % Child Start Function
 	          permanent,                      % Child Restart Policy 
	          2000,                           % Child Sub-tree Max Shutdown Time
	          worker,                         % Child Type
	          [FSM_Module]                    % Child Modules (For Release Handling Purposes)
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
    %% --- The FSM relative to the device type (jfan,jlight,jdoor,jheater or jthermostat) --- %%
    FSM_Specs,
   
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

%% Returns the name of the FSM module associated with a device type (init(_) helper function)
get_fsm_module(DevType) ->
 case DevType of
 
  % Return the name of the FSM module associated with the device type
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