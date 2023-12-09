day(9).

:- use_module(lib/solve).

zero([]).
zero([0|T]) :- zero(T).

diffs([_], []).
diffs([F|[S|T]], [HDiff|TDiff]) :- HDiff is S - F, diffs([S|T], TDiff).

extend(Numbers, 0, 0) :- zero(Numbers), !.
extend(Numbers, PrevNumber, NextNumber) :-
  diffs(Numbers, Diffs),
  extend(Diffs, PrevDiff, NextDiff),
  last(Numbers, Last), Numbers = [First|_],
  NextNumber is Last + NextDiff, PrevNumber is First - PrevDiff.

/* required for loadData */
data_line(Numbers, Line) :-
  split_string(Line, " ", " ", NumberStrings),
  maplist(number_string, Numbers, NumberStrings).
