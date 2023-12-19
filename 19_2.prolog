day(19). groupData. testResult(167409079868000).

:- use_module(lib/solve).

subtract_([Min1, Max1], [Min2, Max2], []) :-  Min2 =< Min1, Max2 >= Max1.
subtract_([Min1, Max1], [Min2, Max2], [[Max2, Max1]]) :- Min2 =< Min1, Max2 < Max1.
subtract_([Min1, Max1], [Min2, Max2], [[Min1, Min2]]) :- Min2 > Min1, Max2 >= Max1.
subtract_([Min1, Max1], [Min2, Max2], [[Min1, Min2], [Max2, Max1]]) :- Min2 > Min1, Max2 < Max1.
subtract(Ranges, Subtract, Result) :-
  maplist([R,SR]>>subtract_(R, Subtract, SR), Ranges, SubtractedRanges),
  append(SubtractedRanges, Result).

intersect_([Min1, Max1], [Min2, Max2], []) :- Max2 < Min1 ; Max1 < Min2.
intersect_([Min1, Max1], [Min2, Max2], [[NMin,NMax]]) :-
  NMin is max(Min1, Min2),
  NMax is min(Max1, Max2),
  NMin < NMax.
intersect(Ranges, Intersect, Result) :-
  maplist([R,IR]>>intersect_(R, Intersect, IR), Ranges, IntersectedRanges),
  append(IntersectedRanges, Result).

intersectall(Ranges, [], Ranges).
intersectall(Ranges, [H|T], Result) :- intersect(Ranges, H, Next), intersectall(Next, T, Result).

r("x", [_,M,A,S], X, [X,M,A,S]).
r("m", [X,_,A,S], M, [X,M,A,S]).
r("a", [X,M,_,S], A, [X,M,A,S]).
r("s", [X,M,A,_], S, [X,M,A,S]).

range_if_else([1, 4001], _, Ranges, Ranges, []) :- !.
range_if_else(Range, Xmas, Ranges, IfRanges, ElseRanges) :-
  r(Xmas, _, XmasRanges, Ranges),
  intersect(XmasRanges, Range, IfXmasRanges),
  subtract(XmasRanges, Range, ElseXmasRanges),
  r(Xmas, Ranges, IfXmasRanges, IfRanges),
  r(Xmas, Ranges, ElseXmasRanges, ElseRanges).

range(c{c: <, v: V, r: _, n: _}, [1, V]).
range(c{c: >, v: V, r: _, n: _}, [Vn, 4001]) :- Vn is V + 1.
range(c{c: passthrough, v: _, r: _, n: _}, [1, 4001]).

prepareRule(_, [], _).
prepareRule(Name, [H|T], Ranges) :-
  H = c{c: _, v: _, r: R, n: N},
  range(H, Range),
  range_if_else(Range, R, Ranges, IfRanges, ElseRanges),
  assertRule(Name, IfRanges, N),
  prepareRule(Name, T, ElseRanges).
prepareRule([Name, Conditions]) :- prepareRule(Name, Conditions, [[[1,4001]], [[1,4001]], [[1,4001]], [[1,4001]]]).

assertRule(Name, Ranges, N) :- member([], Ranges) -> true ; assert(rule(Name, Ranges, N)).

combineRules([PrevStart, [Xp, Mp, Ap, Sp]], [[Xn, Mn, An, Sn], NextEnd]) :- 
  intersectall(Xp, Xn, Xc),
  intersectall(Mp, Mn, Mc),
  intersectall(Ap, An, Ac),
  intersectall(Sp, Sn, Sc),
  assertRule(PrevStart, [Xc, Mc, Ac, Sc], NextEnd).

via(Via) :- rule(_, _, Via), Via \= "in", Via \= "A".

unroll(Via) :-
  findall([PS,PR], rule(PS, PR, Via), PrevRules),
  findall([NR,NE], rule(Via, NR, NE), NextRules),
  foreach(member([PSD, PRD], PrevRules), retractall(rule(PSD, PRD, Via))),
  foreach(member([NRD, NED], NextRules), retractall(rule(Via, NRD, NED))),
  foreach((member(Prev, PrevRules), member(Next, NextRules)), combineRules(Prev, Next)).

ranges_possibilities(Ranges, Sum) :- mapsum(Ranges, [[S,E],A]>>(A is E-S), Sum).

accepted_possibilities(Possibilities) :-
  rule("in", [Xr,Mr,Ar,Sr], "A"),
  ranges_possibilities(Xr, X),
  ranges_possibilities(Mr, M),
  ranges_possibilities(Ar, A),
  ranges_possibilities(Sr, S),
  Possibilities is X*M*A*S.

result([Rules, _], Possibilities) :-
  foreach(member(Rule, Rules), prepareRule(Rule)),
  foreach(via(Via), unroll(Via)),
  aggregate_all(sum(P), accepted_possibilities(P), Possibilities).

/* required for loadData */
resetData :- retractall(rule(_,_,_)).
data_line(Xmas, Line) :-
  string_concat("{", _, Line), !,
  split_string(Line, ",", "{}", RatingStrings),
  maplist(rating_string, Xmas, RatingStrings).
data_line([Name,Steps], Line) :-
  split_string(Line, "{,", "}", [Name|D]),
  maplist(string_step, D, Steps).

rating_string(Rating, String) :-
  split_string(String, "=", "", [_,Value]), !,
  number_string(Rating, Value).

string_step(String, c{c: <, r: Rating, v: Value, n: Rule}) :-
  split_string(String, "<:", "", [Rating,ValueString,Rule]), !,
  number_string(Value, ValueString).
string_step(String, c{c: >, r: Rating, v: Value, n: Rule}) :-
  split_string(String, ">:", "", [Rating,ValueString,Rule]), !,
  number_string(Value, ValueString).
string_step(Rule,  c{c: passthrough, r: passthrough, v: passthrough, n: Rule}).
