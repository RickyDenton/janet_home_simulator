%% This is the resource file of the JANET Simulator application %%

{application,
 janet_simulator,				            % Application Name
 [
  {description,"JANET Erlang Simulator"},	% Application Description
  {vsn,"0.0.1"},							% Application Version
  {modules,[jsim,sup_jsim,sim_restserver,   % Application-specific Modules
            db,utils,sup_locs,sup_loc,
			locs_init,loc_devs_init,
			ctr_manager,dev_manager]},
  {registered,[sup_locs]},                  % Application Registered Names
  {applications,[mnesia,stdlib,kernel]},    % Application Dependencies
  {mod,{jsim,[]}}                           % Application Callback Module and Arguments([] = 'normal')
 ]
}.