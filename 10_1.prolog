testResult("test-1a", 4). testResult("test-1b", 8).

:- include('10.common.prolog').

result(Pipes, MaxDistance) :-
  assert_pipes(Pipes, 0), pipe('S', X, Y, _), retractall(pipe('S', [_, _], _)), asserta(pipe('S', X, Y, true)),
  find_loop(LoopLength), MaxDistance is LoopLength//2.
