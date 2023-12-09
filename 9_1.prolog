testResult(114).

:- include('9.common.prolog').

result(Numbers, Result) :-
  maplist(extend, Numbers, _, NextNumbers),
  sum_list(NextNumbers, Result).
