-module(mobius).
-export([is_prime/1]).
-export([prime_factors/1]).
-export([next_prime_number/1]).
-export([is_square_multiple/1]).
-export([find_square_multiples/2]).
-export([test_time/0]).
-import(io, [format/1]).


% check if number is prime
is_prime(N) ->
    is_prime_helper(N, 2).
is_prime_helper(N, M) when M * M > N ->
    true;
is_prime_helper(N, M) ->
    IsPrime = N rem M,
    if
        IsPrime == 0 -> false;
        true -> is_prime_helper(N, M + 1)
    end.

% finds next prime number - used in prime_factors func
next_prime_number(N) ->
    next_prime_number_helper(N + 1).
next_prime_number_helper(N) ->
    IsPrime = is_prime(N),
    if
        IsPrime == true -> N;
        true -> next_prime_number_helper(N + 1)
    end.

% get list of prime factors of number
prime_factors(N) ->
    prime_factors_helper(N, [1], 2).
prime_factors_helper(N, List, Div) when Div * Div > N ->
    [N|List];
prime_factors_helper(N, List, Div) when (N rem Div) =:= 0 ->
    prime_factors_helper(N div Div, [Div|List], Div);
prime_factors_helper(N, List, Div) ->
    prime_factors_helper(N, List, next_prime_number(Div)).

% check if list of factors contains duplicate values - if so, just compare :) 
is_square_multiple(N) ->
    FactorsList = prime_factors(N),
    erlang:length(FactorsList) =/= sets:size(sets:from_list(FactorsList)).


% finds sequence of square multiples with length of Count and with numbers less than MaxN 
find_square_multiples(Count, MaxN) ->
	find_square_multiples_helper(0, 0, 0, Count, MaxN).
find_square_multiples_helper(CurrNum, FirstNum, FoundCount, Count, MaxN) when CurrNum =< MaxN, FoundCount < Count ->
	IsSquareMultiple = is_square_multiple(CurrNum),
	if
		IsSquareMultiple == true ->
			% if it is square multiple
            if
                % Case it is first square multiple met, pass it as first num in sequence
				FirstNum == 0 -> find_square_multiples_helper(CurrNum + 1, CurrNum, FoundCount + 1, Count, MaxN);
				% Else first num is not changed
                true -> find_square_multiples_helper(CurrNum + 1, FirstNum, FoundCount + 1, Count, MaxN)
			end;
		true ->
            % If not - pass First num as 0 again
			find_square_multiples_helper(CurrNum + 1, 0, 0, Count, MaxN)
	end;
find_square_multiples_helper(_, FirstNum, FoundCount, Count, _) when FoundCount == Count ->
	FirstNum;
find_square_multiples_helper(_, _, _, _, _) ->
	fail.

test_time() ->
    {Time, _} = timer:tc(mobius,find_square_multiples,[4, 30000]),
    io:fwrite("Time for 4: ~w~n", [Time]),
    {Time2, _} = timer:tc(mobius,find_square_multiples,[5, 30000]),
    io:fwrite("Time for 5: ~w~n", [Time2]),
    {Time3, _} = timer:tc(mobius,find_square_multiples,[6, 30000]),
    io:fwrite("Time for 6: ~w~n", [Time3]).