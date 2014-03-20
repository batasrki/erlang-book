-module(shop1).
-export([total/1]).

total([{What, Quantity} | ShoppingList]) ->
    shop:cost(What) * Quantity + total(ShoppingList);
total([]) ->
    0.
