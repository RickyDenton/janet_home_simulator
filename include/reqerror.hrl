%% This record describes an error occured during the handling of an HTTP request
-record(reqerror,    
        {
		 handlername,  % The name of the resource or operation handler where the error was raised
		 error,        % The error that was raised, which may consist in either
		               % an atom 'errorType' or a tuple {'errorType',errorInfo}
		 errormod,     % The name of the module where to retrieve the HTTP code and the plain text message
		               % associated with the error (which in the handling logic is set to 'rest_utils' if
					   % the error occured during the resource handler or in retrieving the body parameters,
					   % or the custom handler if the error occured during a operation handler)
		 req,          % The map associated to the HTTP request used by Cowboy when the error occured
		 binbody       % The body of the HTTP request as a binary (if any)
		}).