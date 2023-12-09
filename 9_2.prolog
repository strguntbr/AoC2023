testResult(2).

:- include('9.common.prolog').

result(Numbers, Result) :-
  maplist(extend, Numbers, PrevNumbers, _),
  sum_list(PrevNumbers, Result).
