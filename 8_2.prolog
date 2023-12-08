day(8). testResult("test3", 6).

:- use_module(lib/solve), use_module(library(regex)).

gcd(0, GCD, GCD) :- !.
gcd(X, Y, GCD) :- X < Y, NextY is Y mod X, gcd(X, NextY, GCD).
gcd(X, Y, GCD) :- X > Y, gcd(Y, X, GCD).

lcm(X, Y, LCM):-gcd(X, Y, GCD), LCM is X*Y/GCD.
lcm([H], H).
lcm([H|T], LCM) :- lcm(T, LN), lcm(H, LN, LCM).

navigate([_,_,'Z'], _, _, 0) :- !.
navigate(Node, [H|T], Instructions, Length) :-
  next(Node, H, NextNode),
  navigate(NextNode, T, Instructions, NextLength),
  Length is NextLength + 1.
navigate(Node, [], Instructions, Length) :- navigate(Node, Instructions, Instructions, Length).

result([Instructions|_], Length) :- 
  findall([A,B,'A'], next([A,B,'A'], 'R', _), StartNodes),
  maplist([Start, Length]>>navigate(Start,Instructions,Instructions,Length), StartNodes, Lengths),
  lcm(Lengths, Length).

/* required for loadData */
resetData :- retractall(next(_, _, _)).
data_line([Node,Left,Right], Line) :-
  split_string(Line, "=,", " ()", [Node,Left,Right]), !,
  string_chars(Node, NodeNode), string_chars(Left, LeftNode), string_chars(Right, RightNode),
  assert(next(NodeNode, 'L', LeftNode)), assert(next(NodeNode, 'R', RightNode)).
data_line(Instructions, Line) :- string_chars(Line, Instructions).
