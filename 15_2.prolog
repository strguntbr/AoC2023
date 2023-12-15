day(15). testResult(145).

:- use_module(lib/solve).

/*hash([], Hash, Hash).*/
hash(['-'|_], Hash, Hash) :- !.
hash(['='|_], Hash, Hash) :- !.
hash([H|T], Cur, Hash) :- char_code(H, Code), Next is mod((Cur + Code) * 17, 256), hash(T, Next, Hash).
hash(Chars, Hash) :- hash(Chars, 0, Hash).

remove([], _, []).
remove([lens{label: Label, focalLength: _}|T], Label, T) :- !.
remove([H|T], Label, [H|TN]) :- remove(T, Label, TN).

put([], Label, FocalLength, [lens{label: Label, focalLength: FocalLength}]).
put([lens{label: Label, focalLength: _}|T], Label, FocalLength, [lens{label: Label, focalLength: FocalLength}|T]) :- !.
put([H|T], Label, FocalLength, [H|TN]) :- put(T, Label, FocalLength, TN).

process_step(Step, Box, NextBox) :-
  append(Label, ['-'], Step),
  remove(Box, Label, NextBox).
process_step(Step, Box, NextBox) :-
  append([Label, ['='], FL], Step),
  number_chars(FocalLength, FL),
  put(Box, Label, FocalLength, NextBox).

process([], LensConfiguration, LensConfiguration).
process([H|T], LensConfiguration, ResultingLensConfiguration) :-
  hash(H, BoxNr),
  length(Prev, BoxNr), append([Prev, [Box], Next], LensConfiguration),
  process_step(H, Box, BoxN),
  append([Prev, [BoxN], Next], NextLensConfiguration),
  process(T, NextLensConfiguration, ResultingLensConfiguration).

process(Steps, LensConfiguration) :- list(256, [], List), process(Steps, List, LensConfiguration).

box_power([], _, 0).
box_power([lens{label: _, focalLength: FocalLength}|T], I, Power) :-
  J is I + 1,
  box_power(T, J, RemainingPower),
  Power is I * FocalLength + RemainingPower.

total_power([], _, 0).
total_power([H|T], I, FocusingPower) :-
  box_power(H, 1, FP),
  J is I + 1,
  total_power(T, J, TFP),
  FocusingPower is I * FP + TFP.

result([Steps], FocusingPower) :- process(Steps, LensConfiguration), total_power(LensConfiguration, 1, FocusingPower).

/* required for loadData */
data_line(Steps, Line) :- split_string(Line, ",", "", S), maplist(string_chars, S, Steps).
