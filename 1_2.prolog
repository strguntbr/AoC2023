day(1). testResult("test2", 281).

:- use_module(lib/solve).

is_number(['0'], 0).
is_number(L, 0) :- string_chars('zero', Number), append(Number, _, L).
is_number(['1'], 1).
is_number(L, 1) :- string_chars('one', Number), append(Number, _, L).
is_number(['2'], 2).
is_number(L, 2) :- string_chars('two', Number), append(Number, _, L).
is_number(['3'], 3).
is_number(L, 3) :- string_chars('three', Number), append(Number, _, L).
is_number(['4'], 4).
is_number(L, 4) :- string_chars('four', Number), append(Number, _, L).
is_number(['5'], 5).
is_number(L, 5) :- string_chars('five', Number), append(Number, _, L).
is_number(['6'], 6).
is_number(L, 6) :- string_chars('six', Number), append(Number, _, L).
is_number(['7'], 7).
is_number(L, 7) :- string_chars('seven', Number), append(Number, _, L).
is_number(['8'], 8).
is_number(L, 8) :- string_chars('eight', Number), append(Number, _, L).
is_number(['9'], 9).
is_number(L, 9) :- string_chars('nine', Number), append(Number, _, L).

first_number(L, V) :- append(Number, _, L), is_number(Number, V), !.
first_number([_|T], V) :- first_number(T, V).

last_number([_|T], V) :- last_number(T, V), !.
last_number(L, V) :- append(Number, _, L), is_number(Number, V).

extract_calibration_value(Line, CalibrationValue) :-
  first_number(Line, First),
  last_number(Line, Last),
  CalibrationValue is 10*First + Last.

result(CalibrationDocument, CalibrationValue) :- mapsum(CalibrationDocument, extract_calibration_value, CalibrationValue).

/* required for loadData */
data_line(Data, Line) :- string_chars(Line, Data).
