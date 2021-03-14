-module(proc_sieve).
-export([generate/1]).
-export([gen_print/1]).
-export([sieve/0]).


% Get prime numbers list
generate(MaxN) ->
	Proc_id = spawn(proc_sieve, sieve, []),  % spawn process and save id
	generate_helper(2, MaxN, Proc_id).
generate_helper(CurrNum, MaxN, Proc_id) when CurrNum =< MaxN ->
	Proc_id ! CurrNum,  % send message to process
	generate_helper(CurrNum + 1, MaxN, Proc_id);
generate_helper(_, _, Proc_id) ->
	Proc_id ! {done, self()},  % send done to process
	receive  % got list of prime numbers
		{result, List} -> List;
		_ -> {error, "Error"}
	end.

% Process function
sieve() ->
	receive
		N -> sieve_helper(N, undefined, undefined)
	end.
sieve_helper(N, NextId, PrevIdSaved) ->
    receive
        % if we got done
        {done, PrevId} -> 
            if
                % in case we got Next process id undefined we start to go back in processes
                NextId == undefined -> PrevId ! {result, [N]};
                % else we send done to proc and call helper again with next proc undefined
                true -> NextId ! {done, self()},
                sieve_helper(N, undefined, PrevId)
            end;
        % when we get result - we collect everything in list
        {result, List} ->
            if
                PrevIdSaved =/= undefined -> PrevIdSaved ! {result, [N | List]};
                true -> undefined
            end;
        CurrNum when (CurrNum rem N) =/= 0 ->  % If we got num and num can not be divided by N
            if
                NextId == undefined ->  % first run!
                    Proc_id = spawn(proc_sieve, sieve, []),  % spawn child process
			        Proc_id ! CurrNum,  % send CurrNum to it
				    sieve_helper(N, Proc_id, PrevIdSaved);  % run helper with new NextId
                true -> 
                    NextId ! CurrNum,
                    sieve_helper(N, NextId, PrevIdSaved)
            end;
        _ ->  sieve_helper(N, NextId, PrevIdSaved)  % else just run function again
    end.


gen_print(MaxN) ->
    Prime_list = generate(MaxN),
	lists:foreach(
        fun (X) -> io:format("~w\n", [X]) end, 
        Prime_list
    ).
