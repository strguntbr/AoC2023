day(23). testResult(94).

:- use_module(lib/solve).

neighbor([X,Y,'^'], [Xn,Y,_]) :- Xn is X-1.
neighbor([X,Y,'v'], [Xn,Y,_]) :- Xn is X+1.
neighbor([X,Y,'<'], [X,Yn,_]) :- Yn is Y-1.
neighbor([X,Y,'>'], [X,Yn,_]) :- Yn is Y+1.
neighbor([X,Y,'.'], [Xn,Y,_]) :- member(Xd, [-1,1]), Xn is X+Xd.
neighbor([X,Y,'.'], [X,Yn,_]) :- member(Yd, [-1,1]), Yn is Y+Yd.

next(Paths, Cur, Next, RemainingPaths) :-
  neighbor(Cur, Next),
  select(Next, Paths, RemainingPaths).

walk(Paths, [E,_,_], E, Length) :- length(Paths, Length).
walk(Paths, Cur, E, Length) :-
  aggregate_all(min(L), (
    next(Paths, Cur, Next, RemainingPaths),
    walk(RemainingPaths, Next, E, L)
  ), Length).

row_paths([], _, _, []).
row_paths(['#'|T], X, Y, Paths) :- !, Yn is Y+1, row_paths(T, X, Yn, Paths).
row_paths([H|T], X, Y, [[X,Y,H]|Paths]) :- Yn is Y+1, row_paths(T, X, Yn, Paths).

map_paths([], _, []).
map_paths([H|T], X, Paths) :-
  row_paths(H, X, 1, CurPaths),
  Xn is X+1,
  map_paths(T, Xn, NextPaths),
  append(CurPaths, NextPaths, Paths).

result(Map, Result) :-
  length(Map, E),
  map_paths(Map, 1, Paths),
  select([1,Y,'.'], Paths, NextPaths),
  length(NextPaths, A),
  walk(NextPaths, [1,Y,'.'], E, B),
  Result is A-B.

/* required for loadData */
data_line(Chars, Line) :- string_chars(Line, Chars).
