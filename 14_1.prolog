day(14). testResult(136).

:- use_module(lib/solve), use_module(lib/matrix).

weight([], 0, 0).
weight([H|T], Weight, I) :- 
  weight(T, WeightT, IT), I is IT + 1,
  (H = 'O' -> Weight is WeightT + I ; Weight = WeightT).
weight(Row, Weight) :- weight(Row, Weight, _).

next_rock(R, [R|T], [], T) :- !.
next_rock(R, ['.'|T], ['.'|NextEmpty], NextRemaining) :- next_rock(R, T, NextEmpty, NextRemaining).

tilt([], []) :- !.
tilt(Row, ['O'|NextTilted]) :-
  next_rock('O', Row, Empty, RemainingRow), !,
  append(Empty, RemainingRow, NextRow),
  tilt(NextRow, NextTilted).
tilt(Row, NextRow) :-
  next_rock('#', Row, Empty, RemainingRow), !,
  tilt(RemainingRow, TiltedRemainingRow),
  append([Empty, ['#'], TiltedRemainingRow], NextRow).
tilt(R, R).

tilt_matrix(Rocks, TiltedRocks) :- maplist(tilt, Rocks, TiltedRocks).

result(Rocks, WeightSum) :- 
  transpose(Rocks, TRocks),
  tilt_matrix(TRocks, Tilted),
  mapsum(Tilted, weight, WeightSum).

/* required for loadData */
data_line(Data, Line) :- string_chars(Line, Data).
