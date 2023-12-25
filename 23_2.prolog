day(23). testResult(154).

:- use_module(lib/solve).

neighbor([X,Y,_], [Xn,Y,_]) :- member(Xd, [-1,1]), Xn is X+Xd.
neighbor([X,Y,_], [X,Yn,_]) :- member(Yd, [-1,1]), Yn is Y+Yd.

next(Paths, Cur, Next, RemainingPaths) :-
  neighbor(Cur, Next),
  select(Next, Paths, RemainingPaths).

nextStep(Cur, Next, Visited, Dist) :- n(Cur, Next, Dist), \+ member(Next, Visited).
nextStep(Cur, Next, Visited, Dist) :- n(Next, Cur, Dist), \+ member(Next, Visited).

walk([E,_,_], _, E, CurLength, CurLength) :- !.
walk(Cur, Visited, E, CurLength, Length) :-
  nextStep(Cur, Next, Visited, Dist),
  NextLength is CurLength + Dist,
  walk(Next, [Cur|Visited], E, NextLength, Length).

isNode([E,_,_], _, E) :- !.
isNode(Node, Paths, _) :- aggregate_all(count, next(Paths, Node, _, _), C), C > 2.

assertNeighbor(A, B, Dist) :- assert(n(A, B, Dist)).

assertNeighbors(_, [], _, _, _, _) :- !.
assertNeighbors(_, _, LastNode, D, _, Cur) :- n(Cur, _, _), !, assertNeighbor(LastNode, Cur, D).
assertNeighbors(AllPaths, Paths, LastNode, D, E, Cur) :-
  (
    isNode(Cur, AllPaths, E)
    -> NextLastNode = Cur, Dn = 1, assertNeighbor(LastNode, Cur, D)
    ; NextLastNode = LastNode, Dn is D + 1
  ),
  foreach((next(Paths, Cur, Next, RemainingPaths)), assertNeighbors(AllPaths, RemainingPaths, NextLastNode, Dn, E, Next)).

assertNeighbors(Paths, E) :-
  select([1,Y,'.'], Paths, NextPaths),
  foreach((next(NextPaths, [1,Y,'.'], Next, RemainingPaths)), assertNeighbors(Paths,RemainingPaths, [1,Y,'.'], 1, E, Next)).

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
  assertNeighbors(Paths, E),
  member([1,Y,'.'], Paths),
  aggregate_all(max(L), walk([1,Y,'.'], [], E, 0, L), Result).

/* required for loadData */
resetData :- retractall(n(_,_,_)), retractall(curmax(_)), assert(curmax(0)).
data_line(Chars, Line) :- string_chars(Line, Chars).
