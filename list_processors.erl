-module(list_processors).
-export([sum/1, map/2]).

sum([Head|Tail]) ->
    Head + sum(Tail);
sum([]) ->
    0.

map([Head|Tail], Func) ->
    [Func(Head) | map(Tail, Func)];
map([], _) ->
    [].
