day(16). testResult(51).

:- use_module(lib/solve).

move(beam{p: P, d:D}, _, []) :- energized_(P, D), !.

move(beam{p:[X,Y], d:right}, '.', [beam{p:[X,Yn], d: right}]) :- !, Yn is Y+1.
move(beam{p:[X,Y], d:left}, '.', [beam{p:[X,Yn], d: left}]) :- !, Yn is Y-1.
move(beam{p:[X,Y], d:down}, '.', [beam{p:[Xn,Y], d: down}]) :- !, Xn is X+1.
move(beam{p:[X,Y], d:up}, '.', [beam{p:[Xn,Y], d: up}]) :- !, Xn is X-1.

move(beam{p:[X,Y], d:right}, '-', [beam{p:[X,Yn], d: right}]) :- !, Yn is Y+1.
move(beam{p:[X,Y], d:left}, '-', [beam{p:[X,Yn], d: left}]) :- !, Yn is Y-1.
move(beam{p:[X,Y], d:up}, '-', [beam{p:[X,Y1], d: right}, beam{p:[X,Y2], d: left}]) :- !, Y1 is Y+1, Y2 is Y-1.
move(beam{p:[X,Y], d:down}, '-', [beam{p:[X,Y1], d: right}, beam{p:[X,Y2], d: left}]) :- !, Y1 is Y+1, Y2 is Y-1.

move(beam{p:[X,Y], d:down}, '|', [beam{p:[Xn,Y], d: down}]) :- !, Xn is X+1.
move(beam{p:[X,Y], d:up}, '|', [beam{p:[Xn,Y], d: up}]) :- !, Xn is X-1.
move(beam{p:[X,Y], d:right}, '|', [beam{p:[X1,Y], d: down}, beam{p:[X2,Y], d: up}]) :- !, X1 is X+1, X2 is X-1.
move(beam{p:[X,Y], d:left}, '|', [beam{p:[X1,Y], d: down}, beam{p:[X2,Y], d: up}]) :- !, X1 is X+1, X2 is X-1.

move(beam{p:[X,Y], d:right}, '/', [beam{p:[Xn,Y], d: up}]) :- !, Xn is X-1.
move(beam{p:[X,Y], d:left}, '/', [beam{p:[Xn,Y], d: down}]) :- !, Xn is X+1.
move(beam{p:[X,Y], d:down}, '/', [beam{p:[X,Yn], d: left}]) :- !, Yn is Y-1.
move(beam{p:[X,Y], d:up}, '/', [beam{p:[X,Yn], d: right}]) :- !, Yn is Y+1.

move(beam{p:[X,Y], d:right}, '\\', [beam{p:[Xn,Y], d: down}]) :- !, Xn is X+1.
move(beam{p:[X,Y], d:left}, '\\', [beam{p:[Xn,Y], d: up}]) :- !, Xn is X-1.
move(beam{p:[X,Y], d:down}, '\\', [beam{p:[X,Yn], d: right}]) :- !, Yn is Y+1.
move(beam{p:[X,Y], d:up}, '\\', [beam{p:[X,Yn], d: left}]) :- !, Yn is Y-1.

move(_, _, []).

tile(P, S) :- tile_(P, S), !.
tile(_, ' ').

energized(P) :- energized_(P, _), tile(P, S), S \= ' '.

energize([]).
energize([beam{p:P, d:D}|T]) :-
  tile(P, S),
  move(beam{p:P, d:D}, S, Beams),
  assert(energized_(P, D)),
  energize(Beams),
  energize(T).

size([H|T], X, Y) :- length([H|T], X), length(H, Y).

calc(StartBeam, Energized) :-
  retractall(energized_(_,_)),
  energize([StartBeam]),
  aggregate_all(count, distinct(P, energized(P)), Energized).

setup([], _, _).
setup([H|T], X, Y) :- assert(tile_([X,Y], H)), Yn is Y+1, setup(T, X, Yn).

setup([], _).
setup([H|T], X) :- setup(H, X, 0), Xn is X+1, setup(T, Xn).

result(Map, Energized) :-
  setup(Map, 0),
  size(Map, H, W), MaxX is H - 1, MaxY is W - 1,
  list(H, [X,beam{p:[X,0],d:right}]>>true, LeftStart),
  list(H, [X,beam{p:[X,MaxY],d:left}]>>true, RightStart),
  list(W, [Y,beam{p:[0,Y],d:down}]>>true, TopStart),
  list(W, [Y,beam{p:[MaxX,Y],d:up}]>>true, DownStart),
  append([LeftStart, RightStart, TopStart, DownStart], Starts),
  aggregate_all(max(E), (member(Start, Starts), calc(Start, E)), Energized).

/* required for loadData */
resetData :- retractall(pos_(_,_)), retractall(energized_(_, _)).
data_line(Data, Line) :- string_chars(Line, Data).
