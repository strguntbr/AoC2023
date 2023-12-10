day(10).

:- use_module(lib/solve).

assert_pipes([], _).
assert_pipes([H|T], X) :- assert_pipes(H, X, 0), NextX is X + 1, assert_pipes(T, NextX).
assert_pipes([], _, _).
assert_pipes([H|T], X, Y) :- assert_pipe(H, X, Y), NextY is Y + 1, assert_pipes(T, X, NextY).
assert_pipe(P, X, Y) :- assert(pipe(P, X, Y, false)).

connects('S', [X, Y], [X, Yc]) :- Yc is Y-1.
connects('S', [X, Y], [X, Yc]) :- Yc is Y+1.
connects('S', [X, Y], [Xc, Y]) :- Xc is X-1.
connects('S', [X, Y], [Xc, Y]) :- Xc is X+1.
connects('|', [X, Y], [Xc, Y]) :- Xc is X-1.
connects('|', [X, Y], [Xc, Y]) :- Xc is X+1.
connects('-', [X, Y], [X, Yc]) :- Yc is Y-1.
connects('-', [X, Y], [X, Yc]) :- Yc is Y+1.
connects('L', [X, Y], [X, Yc]) :- Yc is Y+1.
connects('L', [X, Y], [Xc, Y]) :- Xc is X-1.
connects('J', [X, Y], [X, Yc]) :- Yc is Y-1.
connects('J', [X, Y], [Xc, Y]) :- Xc is X-1.
connects('F', [X, Y], [X, Yc]) :- Yc is Y+1.
connects('F', [X, Y], [Xc, Y]) :- Xc is X+1.
connects('7', [X, Y], [X, Yc]) :- Yc is Y-1.
connects('7', [X, Y], [Xc, Y]) :- Xc is X+1.

connected(Pipe1, Pos1, Pipe2, Pos2) :- connects(Pipe1, Pos1, Pos2), connects(Pipe2, Pos2, Pos1).

open_end([X,Y]) :-
  pipe(VisitedPipe, X1, Y1, true),
  connected(VisitedPipe, [X1, Y1], NextPipe, [X, Y]),
  pipe(NextPipe, X, Y, false).

loop_closed :- not(open_end(_)).

find_loop(0) :- loop_closed, !.
find_loop(Length) :-
  open_end([X, Y]),
  pipe(P, X, Y, _),
  retractall(pipe(P, X, Y, _)),
  asserta(pipe(P, X, Y, true)),
  find_loop(NextLength),
  Length is NextLength + 1.
  
/* required for loadData */
resetData :- retractall(pipe(_, _, _, _)).
data_line(Pipes, Line) :-  string_chars(Line, Pipes).
