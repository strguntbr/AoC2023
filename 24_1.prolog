day(24). testResult(auxData([7,27]), 2).

:- use_module(lib/solve).

parallel(H1, H2) :-
  H1 = h{p:_, v:[A1,B1,_]}, H2 = h{p:_, v:[A2,B2,_]},
  0 is (A1*B2 - B1*A2).

collision(h{p:[X1,Y1,_], v:[A1,B1,_]}, h{p:[X2,Y2,_], v:[A2,B2,_]}, [X,Y]) :-
  \+ parallel(h{p:[X1,Y1,_], v:[A1,B1,_]}, h{p:[X2,Y2,_], v:[A2,B2,_]}),
  C1 is (X2*B2 - Y2*A2 - B2*X1 + A2*Y1) / (A1*B2 - B1*A2),
  C1 > 0,
  C2 is (Y1-Y2+C1*B1)/B2,
  C2 > 0,
  X is X1+C1*A1, Y is Y1+C1*B1,
  true.

pick2([H|T], H, E2) :- member(E2, T).
pick2([_|T], E1, E2) :- pick2(T, E1, E2).

collideIn(H1, H2, [Min,Max]) :-
  \+ parallel(H1, H2),
  collision(H1, H2, [Xc, Yc]),
  Xc >= Min, Xc =< Max,
  Yc >= Min, Yc =< Max,
  true.
  

result(Hailstones, Collisions) :- result(Hailstones, [200000000000000,400000000000000], Collisions).
result(Hailstones, Area, Collisions) :- aggregate_all(count, (pick2(Hailstones, H1, H2), collideIn(H1, H2, Area)), Collisions).

/* required for loadData */
data_line(h{p:[X,Y,Z], v:[Vx,Vy,Vz]}, Line) :-
  split_string(Line, ",@", " ", NumberStrings),
  maplist(number_string, [X,Y,Z,Vx,Vy,Vz], NumberStrings).
