testResult(auxData(10), 1030). testResult(auxData(100), 8410).

:- include('11.common.prolog').

result(Image, DistanceSum) :- result(Image, 1000000, DistanceSum).
