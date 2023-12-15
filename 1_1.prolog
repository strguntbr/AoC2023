day(1). testResult(142).

:- use_module(lib/solve).

is_number('0', 0).
is_number('1', 1).
is_number('2', 2).
is_number('3', 3).
is_number('4', 4).
is_number('5', 5).
is_number('6', 6).
is_number('7', 7).
is_number('8', 8).
is_number('9', 9).

first_number([N|_], V) :- is_number(N, V), !.
first_number([_|T], V) :- first_number(T, V).

last_number([_|T], V) :- last_number(T, V), !.
last_number([N|_], V) :- is_number(N, V).

extract_calibration_value(Line, CalibrationValue) :-
  first_number(Line, First),
  last_number(Line, Last),
  CalibrationValue is 10*First + Last.

result(CalibrationDocument, CalibrationValue) :- mapsum(CalibrationDocument, extract_calibration_value, CalibrationValue).

/* required for loadData */
data_line(Data, Line) :- string_chars(Line, Data).
