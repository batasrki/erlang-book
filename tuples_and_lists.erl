-module(tuples_and_lists).
-export([a_house/3, add_house/2]).

a_house(Name,Size,Bedrooms) ->
    {{owner, Name},{size,Size},{bedrooms,Bedrooms}}.

add_house(House, ListOfHouses) ->
    [House | ListOfHouses].
