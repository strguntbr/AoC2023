day(4). testResult(30).

:- use_module(lib/solve).

count_winning_numbers(_, [], 0).
count_winning_numbers(WinningNumbers, [H|T], Count) :-
  member(H, WinningNumbers), !,
  count_winning_numbers(WinningNumbers, T, CountN),
  Count is CountN+1.
count_winning_numbers(WinningNumbers, [_|T], Count) :-
  count_winning_numbers(WinningNumbers, T, Count).

update_card_counts(_, _, 0).
update_card_counts(CardId, Increment, Points) :-
  NextCardId is CardId + 1, NextPoints is Points - 1,
  card_count(NextCardId, OldCount), NewCount is OldCount + Increment,
  retractall(card_count(NextCardId, _)), assert(card_count(NextCardId, NewCount)),
  update_card_counts(NextCardId, Increment, NextPoints).

eval_cards([]).
eval_cards([FirstCardId|OtherCardIds]) :-
  card_count(FirstCardId, CardCount),
  winning_numbers_you_have(FirstCardId, Wining),
  update_card_counts(FirstCardId, CardCount, Wining),
  eval_cards(OtherCardIds).

result(CardIds, CardCount) :-
  eval_cards(CardIds),
  aggregate_all(sum(C), card_count(_, C), CardCount).

/* required for loadData */
resetData :- retractall(card(_,_)), retractall(card_count(_,_)).

data_line(Id, Line) :-
  split_string(Line, ":", " ", [IdString, NumbersString]),
  id_string(Id, IdString),
  allnumbers_string(WinningNumbers, NumbersYouHave, NumbersString),
  count_winning_numbers(WinningNumbers, NumbersYouHave, Count),
  assert(winning_numbers_you_have(Id, Count)), assert(card_count(Id, 1)).

id_string(Id, String) :-
  split_string(String, " ", " ", [_,IdString]), number_string(Id, IdString).
allnumbers_string(WinningNumbers, NumbersYouHave, String) :-
  split_string(String, "|", " ", [WinningNumbersString, NumbersYouHaveString]),
  numbers_string(WinningNumbers, WinningNumbersString),
  numbers_string(NumbersYouHave, NumbersYouHaveString).

numbers_string(Numbers, String) :- 
  split_string(String, " ", " ", NumberStrings),
  maplist(number_string, Numbers, NumberStrings).
