-module(shop).
-export ([cost/1, total/1]).
-import (lists, [map/2, sum/1]).

cost(oranges) -> 5;
cost(newspaper) -> 8;
cost(apples) -> 2;
cost(pears) -> 9;
cost(milk) -> 7.

% total(List) -> sum(map(fun({What, NumOfItems}) -> cost(What) * NumOfItems end, List)).
total(List) -> sum([cost(What) * NumOfItems || {What, NumOfItems} <- List]).