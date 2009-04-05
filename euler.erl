-module(euler).
-export([
    main/1, start/0,
    p1/0, p1/1, 
    p2/0, p2/1,
    p3/0, p3/1, is_prime/1,
    p4/0, p4/1, reverse_number/1,
    p5/0, p5/1, sieve/1, max_power/2,
    p6/0, p6/1,
    p7/0, p7/1,
    p8/0,
    p9/0, p9/1,
    p10/0, p10/1,
    p11/0, adjacent_product/3,
    p12/0, p12/1, divisors/1,
    p13/0, rzip/1, largesum/1,
    p14/0, p14/1, list_index/2, maxValueIndex/1, collatzNumbers/1, collatzNumber/1
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

% 6. Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
p6() -> p6(100).
p6(N) -> 
    Numbers = lists:seq(1, N), 
    SquareSum = lists:sum([X*X || X <- Numbers]), 
    Sum = lists:sum(Numbers),
    Sum*Sum - SquareSum.

% 7. What is the 10001st prime number?
p7() -> p7(10001).
p7(N) -> p7(N, 1, []).
p7(N, Current, Primes) -> 
    Numbers = [X || X <- lists:seq(Current+1, 2*Current), lists:all(fun(P) -> (X rem P) /= 0 end, lists:reverse(Primes))],
    Primes2 = sieve(Numbers, Primes),
    Test = (length(Primes2) > N),
    if
        Test -> lists:nth(length(Primes2)-N+1, Primes2);
        true -> p7(N, 2*Current, Primes2)
    end.

% 8. Find the greatest product of five consecutive digits in the 1000-digit number. (number stored in data/p008.txt)

p8() -> p8("data/p008.txt").
p8(Filename) -> 
    {ok, Data} = file:read_file(Filename),
    p8([C-$0 || C <- binary_to_list(Data), C /= $\n], 0).
p8([C1, C2, C3, C4, C5|Rest], ProductSoFar) -> p8([C2, C3, C4, C5|Rest], lists:max([ProductSoFar, C1*C2*C3*C4*C5]));
p8(_, ProductSoFar) -> ProductSoFar.

% 9. There exists exactly one Pythagorean triplet for which a + b + c = 1000. Find the product abc.

head([H|_]) -> H.

p9() -> p9(1000).
p9(N) -> head([X*Y*Z || X <- lists:seq(1, N div 3), Y <- lists:seq(X, (1000-X) div 2), Z <- [1000-(X+Y)], X*X + Y*Y == Z*Z]).

% 10. Find the sum of all the primes below two million.
p10() -> p10(20000).
p10(N) -> lists:sum(sieve(N)).

% 11. What is the greatest product of four adjacent numbers in any direction 
%     (up, down, left, right, or diagonally) in the 20x20 grid? (see data/p011.txt)

p11() -> p11("data/p011.txt", 20, 4).
p11(Filename, Size, Ndigits) -> 
    {ok, Data} = file:read_file(Filename),
    Text = binary_to_list(Data),
    Tokens = string:tokens(Text, " \n"),
    Numbers = lists:map(fun([D1, D2]) -> (D1-$0) * 10 + D2-$0 end, Tokens),
    lists:max([
        lists:max(adjacent_product(Numbers, 1, Ndigits)), 
        lists:max(adjacent_product(Numbers, Size, Ndigits)),
        lists:max(adjacent_product(Numbers, Size-1, Ndigits)),
        lists:max(adjacent_product(Numbers, Size+1, Ndigits))]).

zip_product(_, []) -> [];
zip_product([], _) -> [];
zip_product([X|XList], [Y|YList]) -> [X*Y|zip_product(XList, YList)].

list_skip(List, 0) -> List;
list_skip([_|List], N) -> list_skip(List, N-1).

% Returns [.., X_i + X_i+D + X_i+2*D + ... X_i+(N-1)*D, ...]
adjacent_product(List, Distance, N) -> adjacent_product(List, Distance, N-1, List).
adjacent_product(_, _, 0, Result) -> Result;
adjacent_product(List, Distance, N, Result) -> 
    NextList = list_skip(List, Distance),
    adjacent_product(NextList, Distance, N-1, zip_product(Result, NextList)).

% 12. What is the value of the first triangle number to have over five hundred divisors?

divisors(N) -> divisors(N, 1, []).
divisors(N, X, Result) when X*X > N -> Result;
divisors(N, X, Result) when N rem X == 0 
    -> divisors(N, X+1, [X|[N div X|Result]]);
divisors(N, X, Result) -> divisors(N, X+1, Result).

divisor_count(N) -> divisor_count(N, 1, 0).
divisor_count(N, X, Count) when X*X > N -> Count;
divisor_count(N, X, Count) when N rem X == 0
    -> divisor_count(N, X+1, Count+2);
divisor_count(N, X, Count) -> divisor_count(N, X+1, Count).

p12() -> p12(500).
p12(N) -> p12(1, 1, N).
p12(Index, TriangularNumber, Count) -> 
    DivisorCount = divisor_count(TriangularNumber),
    if 
        DivisorCount > Count -> TriangularNumber;
        true -> p12(Index+1, TriangularNumber+Index+1, Count)
    end.

% 13. Work out the first ten digits of the sum of the following one-hundred 50-digit numbers.

p13() -> p13("data/p013.txt").
p13(Filename) ->
    {ok, Data} = file:read_file(Filename),
    Text = binary_to_list(Data),
    Numbers = lists:map(fun string_to_digits/1, string:tokens(Text, "\n")),
    Sum = largesum(Numbers),
    string:sub_string(Sum, 1, 10).

string_to_digits(N) -> [X - $0 || X <- N].

% zips all lists but each result list will be in reverse order.
rzip(Lists) -> rzip(Lists, []).
rzip([[]|_], Result) -> Result;
rzip(Lists, Result) ->
    rzip([T || [_|T] <- Lists], [[H || [H|_] <- Lists]|Result]).

largesum(Numbers) ->
    % convert Numbers into List of digits in each position, Least Significant first.
    Digitwise = rzip(Numbers),
    largesum(Digitwise, 0, []).

largesum([], 0, Result) -> Result;
largesum([], Carry, Result) -> largesum([], Carry div 10, [Carry rem 10|Result]);
largesum([Digits|Digitwise], Carry, Result) ->
    Sum = lists:sum([Carry|Digits]),
    largesum(Digitwise, Sum div 10, [Sum rem 10|Result]).
    
% 14. The following iterative sequence is defined for the set of positive integers:
%       n = n/2 (n is even)
%       n = 3n + 1 (n is odd)
%     Which starting number, under one million, produces the longest chain?

list_index(L, X) -> list_index(L, X, 0).
list_index([], _, _) -> -1;
list_index([Head|Rest], X, Index) -> 
    case Head == X of
        true -> Index;
        false -> list_index(Rest, X, Index+1)
    end.

memoize_map(Fun, Values) -> lists:reverse(memoize_map(Fun, Values, [], array:new())).

memoize_map(_, [], Results, _) -> Results;
memoize_map(Fun, [Head|Rest], Results, Cache) ->
    case array:get(Head, Cache) of
        undefined -> 
            Value = Fun(Head),
            memoize_map(Fun, Rest, [Value|Results], array:set(Head, Value, Cache));
        Value -> memoize_map(Fun, Rest, [Value|Results], Cache)
    end. 
    

maxValueIndex(L) -> list_index(L, lists:max(L)).

collatzNumber(1) -> 1;
collatzNumber(N) -> 
    case N rem 2 of
        1 -> 1 + collatzNumber(3*N+1);
        0 -> 1 + collatzNumber(N div 2)
    end.

collatzNumbers(N) -> memoize_map(fun collatzNumber/1, lists:seq(1, N)).

p14() -> p14(1000000).
p14(N) -> maxValueIndex(collatzNumbers(N)).

run(Name) ->
    {Time, Answer} = timer:tc(euler, Name, []),
    io:format("~w\t~w\t~g~n", [Name, Answer, Time/1000000.0]).

main(_) -> start().

start() ->
    %lists:foreach(fun(Name)-> run(Name) end, [p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13]).
    lists:foreach(fun(Name)-> run(Name) end, [p11]).

