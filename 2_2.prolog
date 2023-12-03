day(2). testResult(2286).

:- use_module(lib/solve).

max(A, B, A) :- A > B, !.
max(_, B, B).

sets_mincubes([], 0, 0, 0).
sets_mincubes([set{red: Red, green: Green, blue: Blue}|OtherSets], MinRed, MinGreen, MinBlue) :-
  sets_mincubes(OtherSets, OtherMinRed, OtherMinGreen, OtherMinBlue),
  max(OtherMinRed, Red, MinRed),
  max(OtherMinGreen, Green, MinGreen),  
  max(OtherMinBlue, Blue, MinBlue).  

game_minpower(game{id: _, sets: Sets}, MinPower) :-
  sets_mincubes(Sets, MinRed, MinGreen, MinBlue),
  MinPower is MinRed * MinGreen * MinBlue.

result(Games, Powers) :-
  maplist(game_minpower, Games, MinimumPowers),
  sum_list(MinimumPowers, Powers).

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
