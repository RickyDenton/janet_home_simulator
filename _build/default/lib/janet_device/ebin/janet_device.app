{application,janet_device,
             [{description,"JANET Erlang Device"},
              {vsn,"0.0.1"},
              {modules,[dev_server,dev_statem,jdev,sup_jdev]},
              {registered,[dev_server,dev_statem]},
              {applications,[stdlib,kernel]},
              {mod,{jdev,[]}},
              {licenses,["Apache 2.0"]},
              {links,[]}]}.
