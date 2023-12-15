testResult(2).

:- include('9.common.prolog').

result(NumbersList, Result) :- mapsum(NumbersList, [Numbers,Prev]>>extend(Numbers, Prev, _), Result).
