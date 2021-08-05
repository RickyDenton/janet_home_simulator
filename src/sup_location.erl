-module(sup_location).
-export([init/1,start_link/2]).
-behaviour(supervisor).

%% --- Supervisor Callback Functions --- %%

%% Sets its children' general specifications (called by sup_jsim at initialization)
init({ControllerID,DevicesList}) ->
{ok,
 {{one_for_one,5,60},     % {RestartStrategy, MaxRestarts, Time Period}
  [
   % -- Location Controller Handler -- %
   {
    utils:str_to_atom(ControllerID),             		% ChildID
    {loc_ctrl,start_link,[ControllerID,DevicesList]},   % Child Start Function
	temporary,                        				    % Child Restart Policy
	10000,                                              % Sub-tree Max Shutdown Time
	worker,                                             % Child Type
	[loc_ctrl]                                          % Child Modules (For Release Handling Purposes)
   }
   
   % -- TODO: Add controller node spawn here?
  ]
}}.

%% --- Exported API --- %%

start_link(ControllerID,DevicesList) ->
 supervisor:start_link({local,utils:concat_atoms(sup_loc_,ControllerID)},?MODULE,[ControllerID,DevicesList]).