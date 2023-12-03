day(3). testResult(4361).

:- use_module(lib/solve).

is_number(N) :- member(N, ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']).
is_empty('.').
is_symbol(S) :- not(is_number(S)), not(is_empty(S)).

optional_first([], [], []).
optional_first([H|T], [H], T).

contains_symbol([H|_]) :- is_symbol(H), !.
contains_symbol([_|T]) :- contains_symbol(T).

pick3(L, [E1,E2,E3]) :- append([E1,E2,E3], _, L).
pick3([_|T], Three) :- pick3(T, Three).

part_number_main_or_tail(P, C, N, [], Border) :- part_number_tail(P, C, N, Border).
part_number_main_or_tail(P, C, N, Number, Border) :- part_number_main(P, C, N, Number, Border).

part_number_tail(P, [Ch|_], N, Border) :-
  not(is_number(Ch)),
  optional_first(P, Ph, _),
  optional_first(N, Nh, _),
  append([Ph, [Ch], Nh], Border).
part_number_main(P, [Ch|Ct], N, [Ch|NumberT], Border) :-
  is_number(Ch),
  optional_first(P, Ph, Pt),
  optional_first(N, Nh, Nt),
  part_number_main_or_tail(Pt, Ct, Nt, NumberT, BorderT),
  append([Ph, Nh, BorderT], Border).
part_number_first(P, [Ch|Ct], N, Number, Border) :-
  not(is_number(Ch)),
  optional_first(P, Ph, Pt),
  optional_first(N, Nh, Nt),
  part_number_main(Pt, Ct, Nt, Number, BorderT),
  append([Ph, [Ch], Nh, BorderT], Border).

part_number(P, C, N, Number, Border) :-
  part_number_first(P, C, N, Number, Border).
part_number(P, [_|Ct], N, Number, Border) :-
  optional_first(P, _, Pt),
  optional_first(N, _, Nt),
  part_number(Pt, Ct, Nt, Number, Border).

part_number(BoxedSchematic, Number, Border) :-
  pick3(BoxedSchematic, [P,C,N]),
  part_number(P, C, N, Number, Border).

part_number(BoxedSchematic, Number) :-
  part_number(BoxedSchematic, NumberList, Border),
  contains_symbol(Border),
  number_chars(Number, NumberList).

extract_part_numbers(BoxedSchematic, PartNumbers) :-
  findall(Number, part_number(BoxedSchematic, Number), PartNumbers).

result(Schematic, PartNumberSum) :-
  append([[]], Schematic, S1),
  append(S1, [[]], BoxedSchematic),
  extract_part_numbers(BoxedSchematic, PartNumbers),
  sum_list(PartNumbers, PartNumberSum).

/* required for loadData */
data_line(Data, Line) :- string_chars(Line, D), append(['.'], D, D1), append(D1, ['.'], Data).
