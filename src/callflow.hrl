-record(cf_call,{
	  cnode = freeswitch@mindcoder :: atom() %% Freeswitch node for this call request
	  ,controller_pid = 'undefined' :: pid() | 'undefined' %% Pid cof the controller process that allocates and deallocates channels or calls?
	  ,cf_pid = 'undefined' :: pid() | 'undefined' %% Pid of the cf executioner that the callflow module needs to respond to
	  ,aleg_uuid = 'undefined' :: string() | 'undefined'  %% Call id for the channel making this request
	  ,bleg_uuid = 'undefined' :: string() | 'undefined'  %% Call id for this request
	  ,flow_id = 'undefined' :: atom() | 'undefined' %% ID of the call flow that was initially executed
	  ,cid_name = 'undefined' :: string() | 'undefined' %% The CID name provided to the call route request
	  ,cid_number = 'undefined' :: string() | 'undefined' %% The CID number provided to the call route request
	  ,destination_number = 'undefined' :: string() | 'undefined' %% The CID number provided to the call route request
          ,no_match = false :: boolean()  %% Boolean flag, set when the no_match callflow is used
	  ,last_action = 'undefined' :: 'undefined' | atom()          %% Previous action
}).
