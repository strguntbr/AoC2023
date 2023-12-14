day(14). testResult(64).

:- use_module(lib/solve), use_module(lib/matrix).

weight([], 0, 0).
weight([H|T], Weight, I) :- 
  weight(T, WeightT, IT), I is IT + 1,
  (H = 'O' -> Weight is WeightT + I ; Weight = WeightT).

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

cycle(I, R) :- current_predicate(cycle_/2), cycle_(I, R).

cycles(0, R, R) :- !.
cycles(I, Rocks, FinalRocks) :-
  cycle(IP, Rocks), !,
  I0 is IP - mod(I, (IP - I)),
  cycle(I0, FinalRocks).
cycles(I, Rocks, FinalRocks) :-
  tilt_matrix(Rocks, T1), rotate(T1, R2),
  tilt_matrix(R2, T2), rotate(T2, R3),
  tilt_matrix(R3, T3), rotate(T3, R4),
  tilt_matrix(R4, T4), rotate(T4, NextRocks),
  NextI is I - 1,
  assert(cycle_(I, Rocks)),
  cycles(NextI, NextRocks, FinalRocks).

rotate(M, R) :- transpose(M, R0), reverse(R0, R).

result(Rocks, WeightSum) :- 
  transpose(Rocks, TRocks),
  cycles(1000000000, TRocks, Tilted),
  aggregate_all(sum(Weight), (member(Row, Tilted), weight(Row, Weight, _)), WeightSum).

/* required for loadData */
resetData :- retractall(cycle_(_,_)).
data_line(Data, Line) :- string_chars(Line, Data).
