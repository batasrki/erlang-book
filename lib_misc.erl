-module(lib_misc).
-export([for/3, qsort/1, pythagorean_triplets/1, permutations/1, odds_and_evens/1, odds_and_evens_acc/1]).

for(Max, Max, Func) ->
    [Func(Max)];
for(I, Max, Func) ->
    [Func(I) | for(I+1, Max, Func)].

qsort([]) ->
    [];
qsort([Pivot|Tail]) ->
    qsort([X || X <- Tail,
                 X < Pivot])
        ++ [Pivot] ++
    qsort([X || X <- Tail,
                X >= Pivot]).

pythagorean_triplets(Number) ->
    [ {A, B, C} ||
        A <- lists:seq(1, Number),
        B <- lists:seq(1, Number),
        C <- lists:seq(1, Number),
        A+B+C =< Number,
        A*A + B*B =:= C*C
    ].

permutations([]) ->

    [[]];
permutations(List) ->
    [[Head|Tail] || Head <- List,
                     Tail <- permutations(List -- [Head])].

odds_and_evens(List) ->
    Odds = [X || X <- List,
                 (X rem 2) =:= 1],
    Evens = [X || X <- List,
                  (X rem 2) =:= 0],
    {Odds, Evens}.

odds_and_evens_acc(List) ->
    accumulate(List, [], []).

accumulate([Head|Tail], Odds, Evens) ->
    case(Head rem 2) of
        1 ->
            accumulate(Tail, [Head|Odds], Evens);
        0 ->
            accumulate(Tail, Odds, [Head|Evens])
    end;
accumulate([], Odds, Evens) ->
    {lists:reverse(Odds), lists:reverse(Evens)}.