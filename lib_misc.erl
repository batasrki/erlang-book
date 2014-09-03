-module (lib_misc).
-export ([qsort/1, pythag/1]).

qsort([]) -> [];
qsort([Pivot | UnsortedList]) ->
    qsort([X || X <- UnsortedList, X < Pivot])
    ++ [Pivot] ++
    qsort([X || X <- UnsortedList, X >= Pivot]).

pythag(Number) ->
    [ {A, B, C} ||
        A <- lists:seq(1, Number),
        B <- lists:seq(1, Number),
        C <- lists:seq(1, Number),
        A+B+C =< Number,
        A*A + B*B =:= C*C
    ].