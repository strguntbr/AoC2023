day(6). testResult(288).

:- use_module(lib/solve), use_module(library(regex)).

number_of_wins(Time, Distance, Wins) :-
  Wins is ceil(Time/2 + sqrt((Time*Time/4)-Distance) - 1) - floor(Time/2 - sqrt((Time*Time/4)-Distance) + 1) + 1.

multiply_list([], 1).
multiply_list([H|T], Product) :- multiply_list(T, NextProduct), Product is NextProduct * H.

result([Times, Distances], Result) :-
  maplist(number_of_wins, Times, Distances, Wins),
  multiply_list(Wins, Result).

/* required for loadData */
data_line(Numbers, Line) :-
  split_string(Line, " ", " ", [_|NumberStrings]),
  maplist(number_string, Numbers, NumberStrings).

