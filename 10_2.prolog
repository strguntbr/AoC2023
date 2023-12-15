testResult("test-2a", 4). testResult("test-2b", 8).

:- include('10.common.prolog').

remove_tangling_pipes([]).
remove_tangling_pipes([[X,Y]|T]) :- remove_tangling_pipes(T), retractall(pipe(_, X, Y, _)), assert(pipe('.', X, Y, true)).
remove_tangling_pipes :- findall([X,Y], pipe(_,X,Y,false), TanglingPipes), remove_tangling_pipes(TanglingPipes).

connected_pipe(Pipe, [X, Y], NextPipe, [NextX, NextY]) :-
  connected(Pipe, [X, Y], NextPipe, [NextX, NextY]),
  pipe(NextPipe, NextX, NextY, _).

replace_start :-
  pipe('S', X, Y, _),
  connected_pipe('S', [X, Y], NextPipe, NextPos),  connected_pipe('S', [X, Y], PrevPipe, PrevPos), NextPos \= PrevPos,
  connected_pipe(StartPipe, [X, Y], NextPipe, NextPos), connected_pipe(StartPipe, [X, Y], PrevPipe, PrevPos), StartPipe \= 'S',
  retractall(pipe('S', X, Y, _)), asserta(pipe(StartPipe, X, Y, true)).

pipeval('|', 1) :- !.
pipeval('F', 0.5) :- !.
pipeval('J', 0.5) :- !.
pipeval('L', -0.5) :- !.
pipeval('7', -0.5) :- !.
pipeval(_, 0).

left_of(Pipe, X, Y) :- pipe(Pipe, X, Yn, _), Yn < Y.

inside_tile([X, Y]) :-
  pipe('.', X, Y, _),
  findall(Pipe, left_of(Pipe, X, Y), PipesBefore),
  maplist(pipeval, PipesBefore, Values),
  sum_list(Values, Sum), 1 is integer(Sum) mod 2.
  
result(Pipes, InsideTilesCount) :-
  assert_pipes(Pipes, 0), pipe('S', X, Y, _), retractall(pipe('S', [_, _], _)), asserta(pipe('S', X, Y, true)),
  find_loop(_), remove_tangling_pipes, replace_start,
  aggregate_all(count, inside_tile(_), InsideTilesCount).
