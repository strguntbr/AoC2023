day(6). testResult(71503).

:- use_module(lib/solve).

number_of_wins(Time, Distance, Wins) :-
  Wins is ceil(Time/2 + sqrt((Time^2/4)-Distance) - 1) - floor(Time/2 - sqrt((Time^2/4)-Distance) + 1) + 1.

result([Time, Distance], Wins) :- number_of_wins(Time, Distance, Wins).

/* required for loadData */
data_line(Number, Line) :-
  split_string(Line, " ", " ", [_|NumberStrings]),
  string_concat(NumberStrings, NumberString),
  number_string(Number, NumberString).

string_concat([], "").
string_concat([H|T], Result) :-
  string_concat(T, TResult),
  string_concat(H, TResult, Result).
  