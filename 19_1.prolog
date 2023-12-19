day(19). groupData. testResult(19114).

:- use_module(lib/solve).

xmas("x", [X, _, _, _], X).
xmas("m", [_, M, _, _], M).
xmas("a", [_, _, A, _], A).
xmas("s", [_, _, _, S], S).

cmp(A, <, B) :- A < B.
cmp(A, >, B) :- A > B.

part_rating([X, M, A, S], Result) :-
  accept("in", [X, M, A, S]) -> Result is X+M+A+S ; Result=0.

result([_, Parts], RatingSum) :-  mapsum(Parts, part_rating, RatingSum).

/* required for loadData */
resetData :- retractall(accept(_, _)), assert(accept("A", _)).
data_line(Xmas, Line) :-
  string_concat("{", _, Line), !,
  split_string(Line, ",", "{}", RatingStrings),
  maplist(rating_string, Xmas, RatingStrings).
data_line([Name,Steps], Line) :-
  split_string(Line, "{,", "}", [Name|D]),
  maplist(string_step, D, Steps),
  foreach(member(Step, Steps), assertStep(Name, Step)).

rating_string(Rating, String) :-
  split_string(String, "=", "", [_,Value]), !,
  number_string(Rating, Value).

string_step(String, c{c: <, r: Rating, v: Value, n: Rule}) :-
  split_string(String, "<:", "", [Rating,ValueString,Rule]), !,
  number_string(Value, ValueString).
string_step(String, c{c: >, r: Rating, v: Value, n: Rule}) :-
  split_string(String, ">:", "", [Rating,ValueString,Rule]), !,
  number_string(Value, ValueString).
string_step(Rule, else{n: Rule}).

assertStep(Name, c{c: C, r: R, v: Value, n: Next}) :-
  assertz((accept(Name, Xmas) :- xmas(R, Xmas, RV), cmp(RV, C, Value), !, accept(Next, Xmas))).
assertStep(Name, else{n: Next}) :-
  assertz((accept(Name, Xmas) :- accept(Next, Xmas))).
