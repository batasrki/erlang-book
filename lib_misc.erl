-module(lib_misc).
-export([for/3]).

for(Max, Max, Func) ->
    [Func(Max)];
for(I, Max, Func) ->
    [Func(I) | for(I+1, Max, Func)].
