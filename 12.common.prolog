day(12).

:- use_module(lib/solve).

damaged_springs_group([H|T], 0, T) :- !, H \= '#'.
damaged_springs_group([H|T], Length, RemainingSprings) :-
  !, H \= '.',
  NextLength is Length - 1,
  damaged_springs_group(T, NextLength, RemainingSprings).

operational_springs(['.'|T], RemainingSprings) :- !, operational_springs(T, RemainingSprings).
operational_springs(['?'|T], RemainingSprings) :- operational_springs(T, RemainingSprings).
operational_springs([H|T], [H|T]) :- !, H \= '.'.
operational_springs([], []) :- !.

spring(Springs, Length, RemainingSprings) :-
  damaged_springs_group(Springs, Length, NextSprings),
  operational_springs(NextSprings, RemainingSprings).

arrangement([], []).
arrangement(Springs, [H|T]) :-
  spring(Springs, H, RemainingSprings),
  arrangement(RemainingSprings, T).

arrangement([Springs, DamagedGroups]) :-
  operational_springs(Springs, RemainingSprings),
  arrangement(RemainingSprings, DamagedGroups).

split_springs_(['.'|T], ['.'], T) :- T \= [].
split_springs_(['?'|T], ['?'|S1], S2) :- T \= [], split_springs_(T, S1, S2).
split_springs_(['#'|T], ['#'|S1], S2) :- T \= [], split_springs_(T, S1, S2).
split_springs([H|T], [H|S1], S2) :- split_springs_(T, S1, S2), !.

assume_unknown(Springs, [Operational1, Operational2], Damaged) :-
  length(Springs, L), L > 30, !,
  append(P1, ['?'|Operational2], Springs),
  length(P1, L1), L1 > L/3, L1 < 2*L/3, !,
  append(P1, ['.'], Operational1),
  append([P1, ['#'], Operational2], Damaged).

min_length([], 0) :- !.
min_length([H|T], L) :- min_length(T, Ln), L is Ln + H + 1.

count_assumed([Operational1, Operational2], _, DamagedGroups, Count) :-
  append(DG1, DG2, DamagedGroups),
  count_part([Operational1, DG1], C1), C1 > 0,
  count_part([Operational2, DG2], C2), C2 > 0,
  Count is C1 * C2.
count_assumed(_, Damaged, DamagedGroups, Count) :-
  count_part([Damaged, DamagedGroups], Count),
  Count > 0.

count_part([Springs, DamagedGroups], _) :-
  min_length(DamagedGroups, MinLength),
  length(Springs, Length),
  Length < MinLength, !, fail.
count_part([Springs, DamagedGroups], Count) :-
  length(DamagedGroups, GroupCount), GroupCount < 2, !,
  aggregate_all(count, arrangement([Springs, DamagedGroups]), Count),
  Count > 0.
count_part([Springs, DamagedGroups], Count) :-
  split_springs(Springs, S1, S2), !,
  append(DG1, DG2, DamagedGroups),
  aggregate_all(count, arrangement([S1, DG1]), C1), C1 > 0,
  count_arrangements([S2, DG2], C2), C2 > 0,
  Count is C1 * C2.
count_part([Springs, DamagedGroups], Count) :-
  assume_unknown(Springs, AssumeOperational, AssumeDamaged), !,
  count_assumed(AssumeOperational, AssumeDamaged, DamagedGroups, Count).
count_part(ConditionRecord, Count) :- aggregate_all(count, arrangement(ConditionRecord), Count), Count > 0.
  
count_arrangements(ConditionRecord, Count) :- aggregate_all(sum(C), count_part(ConditionRecord, C), Count).

result(ConditionRecords, ArrangementsSum) :- 
  maplist(prepare_condition_record, ConditionRecords, PreparedConditionRecords),
  maplist(count_arrangements, PreparedConditionRecords, Arrangements),
  sumlist(Arrangements, ArrangementsSum).

/* required for loadData */
data_line([Springs, DamagedGroups], Line) :-
  split_string(Line, " ", "", [SString,DGString]),
  string_chars(SString, Springs),
  split_string(DGString, ",", "", DGStrings),
  maplist(number_string, DamagedGroups, DGStrings).
