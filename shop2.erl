-module(shop2).
-export([total/1]).
-import(lists, [map/2, sum/1]).

total(ItemList) ->
    sum(map(fun({What, Quantity}) ->
                    shop:cost(What) * Quantity end, ItemList)).
