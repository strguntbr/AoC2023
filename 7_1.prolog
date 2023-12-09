day(7). testResult(6440).

:- use_module(lib/solve).

compare_cards(=, [], []) :- !.
compare_cards(<, [H1|_], [H2|_]) :- card_order(_, H1, BiggerCards), member(H2, BiggerCards), !.
compare_cards(>, [H1|_], [H2|_]) :- card_order(SmallerCards, H1, _), member(H2, SmallerCards), !.
compare_cards(Delta, [_|T1], [_|T2]) :- !, compare_cards(Delta, T1, T2).

compare_hands(>, Hand1, Hand2) :- Hand1.type < Hand2.type, !.
compare_hands(<, Hand1, Hand2) :- Hand2.type < Hand1.type, !.
compare_hands(Delta, Hand1, Hand2) :- !, compare_cards(Delta, Hand1.cards, Hand2.cards).

ranks([], [], _).
ranks([_|T], [NextPos|RankedT], Pos) :- NextPos is Pos + 1, ranks(T, RankedT, NextPos).
ranks(Hands, Ranks) :- ranks(Hands, Ranks, 0).

winning(Rank, hand{cards:_, type:_, bid: Bid}, Winning) :- Winning is Rank * Bid.

result(Hands, TotalWinnings) :-
  predsort(compare_hands, Hands, SortedHands),
  ranks(SortedHands, Ranks),  
  maplist(winning, Ranks, SortedHands, Winnings),
  sum_list(Winnings, TotalWinnings).

/* required for loadData */
data_line(hand{cards: Cards, bid: Bid, type: Type}, Line) :-
  split_string(Line, " ", " ", [CardsString,BidString]),
  string_chars(CardsString, Cards),
  number_string(Bid, BidString),
  cards_type(Cards, Type).

cards_type(Cards, 1) :- remove_n_equal(5, Cards, _), !.
cards_type(Cards, 2) :- remove_n_equal(4, Cards, _), !.
cards_type(Cards, 3) :- remove_n_equal(3, Cards, Rest), remove_n_equal(2, Rest, _), !.
cards_type(Cards, 4) :- remove_n_equal(3, Cards, _), !.
cards_type(Cards, 5) :- remove_n_equal(2, Cards, Rest), remove_n_equal(2, Rest, _), !.
cards_type(Cards, 6) :- remove_n_equal(2, Cards, _), !.
cards_type(_, 7) :- !.

card_order(SmallerCards, Card, BiggerCards) :- append([SmallerCards,[Card],BiggerCards], ['2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A']).

remove_n_equal(N, Cards, Remaining) :- remove_n(N, _, Cards, Remaining).

remove_n(0, _, R, R) :- !.
remove_n(N, Card, [H|T], Rest) :-
  equal_card(Card, H, RealCard),
  NextN is N - 1,
  remove_n(NextN, RealCard, T, Rest).
remove_n(N, Card, [H|T], [H|Rest]) :- remove_n(N, Card, T, Rest).

equal_card(Card, Card, Card) :- card_order(_, Card, _).
