testResult(114).

:- include('9.common.prolog').

result(NumbersList, Result) :- mapsum(NumbersList, [Numbers,Next]>>extend(Numbers, _, Next), Result).
