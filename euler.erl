-module(euler).
-export([
    main/1,
    p1/0, p1/1, 
    p2/0, p2/1,
    p3/0, p3/1, is_prime/1,
    p4/0, p4/1, reverse_number/1,
    p5/0, p5/1, sieve/1, max_power/2
]).

% 1. Find the sum of all the multiples of 3 or 5 below 1000.
p1() -> p1(1000).
p1(N) -> p1(N, 0).
p1(0, Sum) -> Sum;
p1(N, Sum) when (N rem 3 == 0) or (N rem 5 == 0) -> p1(N-1, Sum+N);
p1(N, Sum) -> p1(N-1, Sum).

% 2. Find the sum of all the even-valued terms in the Fibonacci sequence which do not exceed four million.

p2() -> p2(4000000).
p2(Max) -> p2(1, 2, Max, 0).
p2(X, _, Max, Sum) when X > Max -> Sum;
p2(X, Y, Max, Sum) when X rem 2 == 0 -> p2(Y, X+Y, Max, Sum+X);
p2(X, Y, Max, Sum) -> p2(Y, X+Y, Max, Sum).

% 3. What is the largest prime factor of the number 600851475143 ?

p3() -> p3(600851475143).
p3(N) -> p3(1, N-1, N, 1).
p3(X, _, N, FactorSofar) when X * X > N -> FactorSofar;
p3(X, Y, N, FactorSofar) -> 
    TestX = (N rem X == 0) andalso is_prime(X),
    TestY = (N rem Y == 0) andalso is_prime(Y),
    if
        TestY -> Y;
        TestX -> p3(X+1, N div (X+1), N, X);
        true -> p3(X+1, N div (X+1), N, FactorSofar)
    end.

is_prime(N) -> is_prime(2, N).
is_prime(X, N) when X*X > N -> true;
is_prime(X, N) when N rem X == 0 -> false;
is_prime(X, N) -> is_prime(X+1, N).
    
% 4. Find the largest palindrome made from the product of two 3-digit numbers.

p4() -> p4(999).
p4(Max) -> lists:max([X*Y || X <- lists:seq(1, Max), Y <- lists:seq(X, Max), is_palindrome(X*Y)]).

is_palindrome(N) -> N == reverse_number(N).

reverse_number(N) -> reverse_number(N, 0).
reverse_number(0, Reverse) -> Reverse;
reverse_number(N, Reverse) -> reverse_number(N div 10, Reverse*10 + N rem 10).

% 5. What is the smallest number that is evenly divisible by all of the numbers from 1 to 20?

p5() -> p5(20).
p5(Max) -> product(lists:map(fun(X) -> max_power(X, Max) end, sieve(Max))).

product(List) -> lists:foldl(fun(X, Product) -> X * Product end, 1, List).

% Maximum X^N such that X^N < Max.
max_power(X, Max) -> max_power(X, X, Max).
max_power(X, Power, Max) when Power*X > Max -> Power; 
max_power(X, Power, Max) -> max_power(X, Power*X, Max).

sieve(N) -> sieve(lists:seq(2, N), []).
sieve([], Primes) -> Primes;
sieve([X|Numbers], Primes) -> sieve([N || N <- Numbers, N rem X /= 0], [X|Primes]).

run(Name) ->
    {Time, Answer} = timer:tc(euler, Name, []),
    io:format("~w\t~w\t~g~n", [Name, Answer, Time/1000000.0]).

main(_) ->
    lists:foreach(fun(Name)-> run(Name) end, [p1, p2, p3, p4, p5]).

