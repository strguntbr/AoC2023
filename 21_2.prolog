day(21).
/* Algorithm does not work for test data as it assumes that the horizontal and vertical line  */
/* from the start are all '.'. This is true for the (my) real data but not for the test data. */
/* Thus not tests are executed for this problem.                                              */

:- use_module(lib/solve).

plot_(X,Y,S,P) :-
  Xr is mod(X,S),
  Yr is mod(Y,S),
  plot(Xr,Yr,P).

neighbor(X,Y,S,X,Yn) :- (Yn is Y-1 ; Yn is Y+1), plot_(X,Yn,S,'.'), \+ distance(X,Yn,_).
neighbor(X,Y,S,Xn,Y) :- (Xn is X-1 ; Xn is X+1), plot_(Xn,Y,S,'.'), \+ distance(Xn,Y,_).

calcDist(D, _, D) :- !.
calcDist(Steps, Size, D) :-
  StepsN is Steps+1,
  foreach((distance(X,Y,Steps),neighbor(X,Y,Size,Xn,Yn)), assert(distance(Xn,Yn,StepsN))),
  calcDist(StepsN, Size, D).

calcDist(Size, Distance) :-
  plot(X, Y, 'S'),
  assert(distance(X, Y, 0)),
  retractall(plot(X,Y,_)),
  assert(plot(X,Y,'.')),
  calcDist(0, Size, Distance).

reachable(Mod, X, Y) :-
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

countReachable(Mod, Xc, Yc, S, Count) :-
  MinX is Xc * S,
  MaxX is MinX + S - 1,
  MinY is Yc * S,
  MaxY is MinY + S - 1,
  aggregate_all(count, (reachable(Mod, X,Y), between(MinX,MaxX,X), between(MinY,MaxY,Y)), Count).

assumptions(Size) :-
  plot(Xs, Ys, 'S'),
  foreach(plot(Xs, _, T), (T \= '#')), /* assumption: in the row of 'S' are not '#' */
  foreach(plot(_, Ys, T), (T \= '#')), /* assumption: in the col of 'S' are not '#' */
  aggregate_all(max(X), plot(X, _, _), S),
  aggregate_all(max(Y), plot(_, Y, _), S), /* assumption: map is square */
  Xs = Ys, Xs is S / 2, /* assumption: 'S' is in the center */
  Size is S + 1.

result(Map, Result) :- result(Map, 26501365, Result).
result(Map, Steps, Result) :-
  assertMap(Map),
  assumptions(Size),
  FullCycles is Steps // Size,
  Mod is mod(FullCycles+Steps,2),
  RequiredSteps is 2 * Size + mod(Steps, Size),
  calcDist(Size, RequiredSteps),

  countReachable(Mod, -2, -1, Size, Qu2_l1), /* x Cycles */
  countReachable(Mod, -2,  0, Size, Qu2_0), /* x 1 */
  countReachable(Mod, -2,  1, Size, Qu2_r1), /* x Cycles */

  countReachable(Mod, -1, -1, Size, Qu1_l1), /* x Cycles - 1 */
  countReachable(Mod, -1,  1, Size, Qu1_r1), /* x Cycles - 1 */

  countReachable(Mod,  0, -2, Size, Q0_l2), /* x 1 */
  countReachable(Mod,  0,  2, Size, Q0_r2), /* x 1 */

  countReachable(Mod,  1, -1, Size, Qd1_l1), /* x Cycles - 1 */
  countReachable(Mod,  1,  1, Size, Qd1_r1), /* x Cycles - 1 */

  countReachable(Mod,  2, -1, Size, Qd2_l1), /* x Cycles */
  countReachable(Mod,  2,  0, Size, Qd2_0), /* x 1 */
  countReachable(Mod,  2,  1, Size, Qd2_r1), /* x Cycles */

  countReachable(Mod,  0,  0, Size, Qe), /* x (cycles-1)*(cycles-2)/2 */
  countReachable(Mod, -1,  0, Size, Qo), /* x cycles*(cycles-1)/2 */

  Result is
    Qu2_l1 * FullCycles + Qu2_0 + Qu2_r1 * FullCycles +
    Qu1_l1 * (FullCycles-1) + Qu1_r1 * (FullCycles-1) +
    Q0_l2 + Q0_r2 +
    Qd1_l1 * (FullCycles-1) + Qd1_r1 * (FullCycles-1) +
    Qd2_l1 * FullCycles + Qd2_0 + Qd2_r1 * FullCycles +
    Qe * ((FullCycles-1)*(FullCycles-1)) + Qo * (FullCycles*FullCycles).

/* required for loadData */
resetData :- retractall(plot(_,_,_)), retractall(distance(_,_,_)).
data_line(Data, Line) :- string_chars(Line, Data).
