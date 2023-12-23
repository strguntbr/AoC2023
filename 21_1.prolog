day(21). testResult(auxData(6), 16).

:- use_module(lib/solve).

neighbor(X,Y,X,Yn) :- (Yn is Y-1 ; Yn is Y+1), plot(X,Yn,'.'), not(distance(X,Yn,_)).
neighbor(X,Y,Xn,Y) :- (Xn is X-1 ; Xn is X+1), plot(Xn,Y,'.'), not(distance(Xn,Y,_)).

assertDistance(X, Y, Distance) :-
  distance(X, Y, _) -> true ; assert(distance(X, Y, Distance)).

calcDist(D, D) :- !.
calcDist(Steps, D) :-
  StepsN is Steps+1,
  foreach((distance(X,Y,Steps),neighbor(X,Y,Xn,Yn)), assertDistance(Xn,Yn,StepsN)),
  calcDist(StepsN, D).

calcDist(Distance) :-
  plot(X, Y, 'S'),
  assert(distance(X, Y, 0)),
  calcDist(0, Distance).

reachable(X, Y, Mod) :-
  distance(X, Y, D),
  Mod is mod(D,2).

assertRow([], _, _).
assertRow([H|T], X, Y) :-
 assert(plot(X, Y, H)),
  Yn is Y + 1,
  assertRow(T, X, Yn).
assertMap([], _).
assertMap([H|T], X) :-
  assertRow(H, X, 0),
  Xn is X+1,
  assertMap(T, Xn).
assertMap(Map) :- assertMap(Map, 0).

result(Map, Result) :- result(Map, 64, Result).
result(Map, Steps, Result) :-
  assertMap(Map),
  calcDist(Steps),
  Mod is mod(Steps, 2),
  aggregate_all(count, reachable(_,_, Mod), Result).

/* required for loadData */
resetData :- retractall(plot(_,_,_)), retractall(distance(_,_,_)).
data_line(Data, Line) :- string_chars(Line, Data).
