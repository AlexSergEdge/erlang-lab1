-module(fib).
-export([fib_p/1]).
-export([fib_g/1]).
-export([tail_fib/1]).
-export([test_fib_p/2]).
-export([test_tail_fib/0]).
-import(io, [format/1]).

% if-else non-tail
fib_p(N) ->
    if
        N == 0 -> 0;
        N == 1 -> 1;
        true -> fib_p(N-1) + fib_p(N-2)
    end.

% when guard non-tail
fib_g(N) when N >= 2 ->
    fib_g(N-1) + fib_g(N-2);
fib_g(1) ->
    1;
fib_g(0) ->
    0.

% tail recursion
tail_fib(N) -> 
    tail_fib_helper(N, 0, 1).
tail_fib_helper(0, PrevRes, _) -> 
    PrevRes;
tail_fib_helper(N, PrevRes, CurrRes) ->
    tail_fib_helper(N-1, CurrRes, CurrRes + PrevRes).
    


test_fib_p(Start, Inc) ->
    test_fib_p_helper(Start, Inc, 1).
test_fib_p_helper(Curr, _, Last_time) when Last_time > 5000000 ->
    Curr;
test_fib_p_helper(Curr, Inc, _) ->
    {New_last_time,_} = timer:tc(fib,fib_p,[Curr]),
    io:fwrite("Number: ~w Time: ~w~n", [Curr, New_last_time]),
    test_fib_p_helper(Curr + Inc, Inc, New_last_time).
    

test_tail_fib()->
    {Time, _} = timer:tc(fib,tail_fib,[10000]),
    io:fwrite("Time: ~w~n", [Time]).


%> c(fib.erl).
%> fib:test_fib_p(20, 5).
%> fib:test_tail_fib().
%> fib:fib_p(10).
%> fib:fib_d(10).
%> fib:fib_tail(10)