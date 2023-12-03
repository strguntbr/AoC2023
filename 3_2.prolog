day(3). testResult(467835).

:- use_module(lib/solve).

is_number(N) :- member(N, ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']).
is_empty('.').

pick3(L, [E1,E2,E3]) :- append([E1,E2,E3], _, L).
pick3([_|T], Three) :- pick3(T, Three).

skip_numbers([H|T], Pos, NextPos, Remaining) :- is_number(H), TPos is Pos+1, skip_numbers(T, TPos, NextPos, Remaining).
skip_numbers([H|T], Pos, Pos, [H|T]) :- not(is_number(H)).

part_number_cont([H|T], Pos, EndPos, [H|PartNumberCont]):- is_number(H), !, TPos is Pos+1, part_number_cont(T, TPos, EndPos, PartNumberCont).
part_number_cont(_, Pos, EndPos, []) :- EndPos is Pos-1.

part_number([H|T], Pos, p{n: PartNumber, s: Pos, e: EndPos}) :- is_number(H), part_number_cont([H|T], Pos, EndPos, PartNumber).
part_number([H|T], Pos, PartNumber) :- is_number(H), skip_numbers([H|T], Pos, NextPos, Remaining), part_number(Remaining, NextPos, PartNumber).
part_number([H|T], Pos, PartNumber) :- not(is_number(H)), NextPos is Pos+1, part_number(T, NextPos, PartNumber).

gear(['*'|_], 0).
gear([_|T], Pos) :- gear(T, PosN), Pos is PosN+1.

adjacent_part_number(Line, Pos, PartNumber) :-
  part_number(Line, 0, p{n: PN, s: Start, e: End}),
  ST is Start - 1, ET is End + 1,
  Pos >= ST,
  Pos =< ET,
  number_chars(PartNumber, PN).

adjacent_part_number(Prev, _, _, Pos, PartNumber) :- adjacent_part_number(Prev, Pos, PartNumber).
adjacent_part_number(_, Cur, _, Pos, PartNumber) :- adjacent_part_number(Cur, Pos, PartNumber).
adjacent_part_number(_, _, Next, Pos, PartNumber) :- adjacent_part_number(Next, Pos, PartNumber).

gear_ratio(Prev, Cur, Next, GearRatio) :-
  gear(Cur, Pos),
  findall(PartNumber, adjacent_part_number(Prev, Cur, Next, Pos, PartNumber), [Ratio1, Ratio2]),
  GearRatio is Ratio1*Ratio2.

extract_gear_ratio(BoxedSchematic, GearRatio) :-
  pick3(BoxedSchematic, [P,C,N]),
  gear_ratio(P, C, N, GearRatio).

extract_gear_ratios(BoxedSchematic, GearRatios) :-
  findall(GearRatio, extract_gear_ratio(BoxedSchematic, GearRatio), GearRatios).

result(Schematic, GearRatioSum) :-
  append([[]], Schematic, S1),
  append(S1, [[]], BoxedSchematic),
  extract_gear_ratios(BoxedSchematic, GearRatios),
  sum_list(GearRatios, GearRatioSum).

/* required for loadData */
data_line(Data, Line) :- string_chars(Line, Data).
