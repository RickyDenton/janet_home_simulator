{application,janet_simulator,
             [{description,"JANET Erlang Simulator"},
              {vsn,"0.0.1"},
              {modules,[ctr_manager,db,dev_manager,jsim,loc_devs_init,
                        locs_init,sim_restserver,sup_jsim,sup_loc,sup_locs,
                        utils]},
              {registered,[sup_locs]},
              {applications,[mnesia,stdlib,kernel]},
              {mod,{jsim,[]}},
              {licenses,["Apache 2.0"]},
              {links,[]}]}.