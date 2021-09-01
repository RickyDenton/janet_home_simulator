%% This is the resource file of the JANET Device application %%

{application,
 janet_device, 				                         % Application Name
 [
  {description,"JANET Erlang Device"},	             % Application Description
  {vsn,"0.0.1"},							         % Application Version
  {modules,[jdev,sup_jdev,dev_server,dev_statem]},   % Application-specific Modules                            
  {registered,[dev_server,dev_statem]},              % Application Registered Names
  {applications,[stdlib,kernel]},                    % Application Dependencies
  {mod,{jdev,[]}}                                    % Application Callback Module and Arguments ([] = 'normal')
 ]
}.