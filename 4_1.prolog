day(4). testResult(13).

:- use_module(lib/solve), use_module(library(regex)).

count_winning_numbers(_, [], 0).
count_winning_numbers(WinningNumbers, [H|T], Count) :-
  count_winning_numbers(WinningNumbers, T, CountN),
  ( member(H, WinningNumbers) -> Count is CountN+1 ; Count = CountN ).

count_points(N, N) :- N =< 1.
count_points(Count, Points) :-
  NextCount is Count - 1,
  count_points(NextCount, NextPoints),
  Points is NextPoints * 2.

card_points(card{id: _, w: WinningNumbers, n: NumbersYouHave}, Points) :- count_winning_numbers(WinningNumbers, NumbersYouHave, Count), count_points(Count, Points).

result(ScratchCards, Points) :-
  maplist(card_points, ScratchCards, PointsList),
  sum_list(PointsList, Points).

/* required for loadData */
data_line(card{id: Id, w: WinningNumbers, n: NumbersYouHave}, Line) :-
  split_string(Line, ":", " ", [IdString, NumbersString]),
  id_string(Id, IdString),
  allnumbers_string(WinningNumbers, NumbersYouHave, NumbersString).

id_string(Id, String) :-
  split_string(String, " ", " ", [_,IdString]), number_string(Id, IdString).
allnumbers_string(WinningNumbers, NumbersYouHave, String) :-
  split_string(String, "|", " ", [WinningNumbersString, NumbersYouHaveString]),
  numbers_string(WinningNumbers, WinningNumbersString),
  numbers_string(NumbersYouHave, NumbersYouHaveString).

numbers_string(Numbers, String) :- 
  split_string(String, " ", " ", NumberStrings),
  maplist(number_string, Numbers, NumberStrings).
