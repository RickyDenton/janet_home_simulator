%% JANET Controller application specification %%

{application, janet_controller,				         % Name
 [
  {description,"JANET Controller"},	                 % Description
  {vsn,"0.0.1"},							         % Version
  {modules,[jctr,sup_jctr,sup_devs,ctr_devserver,    % Application Modules
            dev_reg,ctr_restserver]},                 
  {registered,[sup_devs]},                           % Registered Names
  {applications,[stdlib,kernel]},                    % Application Dependencies
  {mod,{jctr,[]}}                                    % Application Callback Module
 ]
}.