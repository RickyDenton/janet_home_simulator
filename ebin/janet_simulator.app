%% JANET Simulator application specification %%

{application, janet_simulator,				% Name
 [
  {description,"JANET Erlang Simulator"},	% Description
  {vsn,"0.0.1"},							% Version
  {modules,[db_utils,loc_ctrl,jsim,         % Application Modules
            sim_restserver,sup_jsim,
			sup_location,sup_locations,
			utils]},
  {registered,[sup_locs]},                  % Registered Names
  {applications,[mnesia,stdlib,kernel]},    % Application Dependencies
  {mod,{jsim,[]}}                           % Application Callback Module
 ]
}.