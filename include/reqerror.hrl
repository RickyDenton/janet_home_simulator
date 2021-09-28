%% This record describes an error occured during the handling of an HTTP request
-record(reqerror,    
        {
		 handler,                    % The name of the resource or operation handler where the error was raised
		 error,                      % The error that was raised, which may consist in either an atom 'errorType' or a tuple {'errorType',errorInfo}
		 req,                        % The cowboy 'req' map when the error occured
		 binbody = <<"<not_read>">>  % A reference used for monitoring the device's 'dev_server' process (and consequently the node)
		}).