%% JANET Device application specification %%

{application, janet_device, 				         % Name
 [
  {description,"JANET Device"},	                     % Description
  {vsn,"0.0.1"},							         % Version
  {modules,[jdev,sup_jdev,dev_server]},              % Application Modules              
  {registered,[dev_server,dev_fsm]},                 % Registered Names
  {applications,[stdlib,kernel]},                    % Application Dependencies
  {mod,{jdev,[]}}                                    % Application Callback Module
 ]
}.