day(18). testResult(62).

:- use_module(lib/solve).

move([X,Y], move{dir:up, l:L}, [Xn, Y]) :- Xn is X + L.
move([X,Y], move{dir:down, l:L}, [Xn, Y]) :- Xn is X - L.
move([X,Y], move{dir:left, l:L}, [X, Yn]) :- Yn is Y + L.
move([X,Y], move{dir:right, l:L}, [X, Yn]) :- Yn is Y - L.

instructions_polygon([], [[0,0]]).
instructions_polygon([H|T], [VH|[NVH|NVT]]) :-
  instructions_polygon(T, [NVH|NVT]),
  move(NVH, H, VH).

area(Polygon, Area) :-
  aggregate_all(sum(A), (append([_,[[X1,Y1],[X2,Y2]], _], Polygon), A is X2*Y1-X1*Y2), Area2),
  Area is integer(abs(Area2/2)).

clockwise(D1, D2) :- append([_,[D1,D2],_], [right,down,left,up,right]).

adjust(L, D1, D2, LF) :- clockwise(D1, D2) -> LF is L + 0.5 ; LF is L - 0.5.

prepare_instructions_([move{dir:D,l:L}], FirstDir, [move{dir:D,l:LF}]) :- adjust(L, D, FirstDir, LF).
prepare_instructions_(Instructions, FirstDir, [move{dir:D1,l:L1F}|FI]) :-
  append([move{dir:D1,l:L1}, move{dir:D2,l:L2}], T, Instructions),
  adjust(L1, D1, D2, L1F), adjust(L2, D1, D2, L2F),
  prepare_instructions_([move{dir:D2,l:L2F}|T], FirstDir, FI).

prepare_instructions([move{dir: FirstDir,l:L}|T], PreparedInstructions) :-
  prepare_instructions_([move{dir: FirstDir,l:L}|T], FirstDir, [_|PreparedInstructions]).

result(Instructions, Area) :-
  prepare_instructions(Instructions, PreparedInstructions),
  instructions_polygon(PreparedInstructions, Polygon),
  area(Polygon, Area).

/* required for loadData */
data_line(move{dir: Dir, l:Length}, Line) :-
  split_string(Line, " ", "", [DirString, LengthString, _]),
  dir_string(Dir, DirString),
  number_string(Length, LengthString).
dir_string(up, "U"). dir_string(down, "D"). dir_string(left, "L"). dir_string(right, "R").
