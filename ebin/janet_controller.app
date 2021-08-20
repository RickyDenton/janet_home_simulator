%% This is the resource file of the JANET Controller application %%

{application,
 janet_controller,				                     % Application Name
 [
  {description,"JANET Erlang Controller"},	         % Application Description
  {vsn,"0.0.1"},							         % Application Version
  {modules,[jctr,sup_jctr,sup_devs,ctr_regserver,    % Application-specific Modules
            ctr_devserver,ctr_restserver]},                 
  {registered,[sup_devs,ctr_regserver]},             % Application Registered Names
  {applications,[stdlib,kernel]},                    % Application Dependencies
  {mod,{jctr,[]}}                                    % Application Callback Module and Arguments([] = 'normal')
 ]
}.