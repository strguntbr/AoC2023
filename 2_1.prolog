day(2). testResult(8).

:- use_module(lib/solve).

set_is_possible(AvailableRed, AvailableGreen, AvailableBlue, set{red: Red, green: Green, blue: Blue}) :-
  Red =< AvailableRed,
  Green =< AvailableGreen,
  Blue =< AvailableBlue.
game_is_possible(_, _, _, []).
game_is_possible(AvailableRed, AvailableGreen, AvailableBlue, [H|T]) :-
  set_is_possible(AvailableRed, AvailableGreen, AvailableBlue, H),
  game_is_possible(AvailableRed, AvailableGreen, AvailableBlue, T).
game_is_possible(game{id: _, sets: Sets}) :- game_is_possible(12, 13, 14, Sets).

game_id(game{id: Id, sets: _}, Id).

result(Games, IdSum) :-
  include(game_is_possible, Games, PossibleGames),
  maplist(game_id, PossibleGames, PossibleGameIds),
  sum_list(PossibleGameIds, IdSum).

/* required for loadData */
data_line(game{id: Id, sets: Sets}, Line) :-
  split_string(Line, ":", " ", [IdString, SetsString]),
  id_string(Id, IdString),
  sets_string(Sets, SetsString).

id_string(Id, String) :- string_concat("Game ", IdString, String), number_string(Id, IdString).
sets_string(Sets, String) :-
  split_string(String, ";", " ", SetStrings),
  maplist(set_string, Sets, SetStrings).
set_string(Set, String) :- 
  split_string(String, ",", " ", CubeStrings),
  maplist(cube_string, Cubes, CubeStrings),
  set_cubes(Set, Cubes).
cube_string(cube{color: Color, amount: Amount}, String) :-
  split_string(String, " ", "", [AmountString, Color]),
  number_string(Amount, AmountString).

set_cubes(set{red: 0, green: 0, blue: 0}, []).
set_cubes(set{red: Red, green: Green, blue: Blue}, [cube{color: "red", amount: Amount}|T]) :-
  set_cubes(SetT, T),
  Red is SetT.red + Amount,
  Green is SetT.green,
  Blue is SetT.blue.
set_cubes(set{red: Red, green: Green, blue: Blue}, [cube{color: "green", amount: Amount}|T]) :-
  set_cubes(SetT, T),
  Red is SetT.red,
  Green is SetT.green + Amount,
  Blue is SetT.blue.
set_cubes(set{red: Red, green: Green, blue: Blue}, [cube{color: "blue", amount: Amount}|T]) :-
  set_cubes(SetT, T),
  Red is SetT.red,
  Green is SetT.green,
  Blue is SetT.blue + Amount.
