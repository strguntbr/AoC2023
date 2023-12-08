day(8). testResult(2). testResult("test2", 6).

:- use_module(lib/solve), use_module(library(regex)).

navigate("ZZZ", _, _, 0) :- !.
navigate(Node, [H|T], Instructions, Length) :-
  next(Node, H, NextNode),
  navigate(NextNode, T, Instructions, NextLength),
  Length is NextLength + 1.
navigate(Node, [], Instructions, Length) :- navigate(Node, Instructions, Instructions, Length).


navigate(Instructions, Length) :- navigate("AAA", Instructions, Instructions, Length).

result([Instructions|_], Length) :- navigate(Instructions, Length).

/* required for loadData */
resetData :- retractall(next(_, _, _)).
data_line([Node,Left,Right], Line) :-
  split_string(Line, "=,", " ()", [Node,Left,Right]), !,
  assert(next(Node, 'L', Left)), assert(next(Node, 'R', Right)).
data_line(Instructions, Line) :- string_chars(Line, Instructions).
