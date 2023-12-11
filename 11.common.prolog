day(11).

:- use_module(lib/solve).

empty([]).
empty(['.'|T]) :- empty(T).

empty_row([H|_], X, X) :- empty(H).
empty_row([_|T], X, EmptyRow) :- NextX is X + 1, empty_row(T, NextX, EmptyRow).
empty_row(Image, EmptyRow) :- empty_row(Image, 0, EmptyRow).

head([H|_], H).
tail([_|T], T).

empty_col(Image, Y, Y) :- maplist(head, Image, FirstCol), empty(FirstCol).
empty_col(Image, Y, EmptyCol) :- maplist(tail, Image, NextImage), NextY is Y + 1, empty_col(NextImage, NextY, EmptyCol).
empty_col(Image, EmptyCol) :- empty_col(Image, 0, EmptyCol).

galaxy(['#'|_], X, Y, [X, Y]).
galaxy([_|T], X, Y, Galaxy) :- NextY is Y + 1, galaxy(T, X, NextY, Galaxy).

galaxy([H|_], X, Galaxy) :- galaxy(H, X, 0, Galaxy).
galaxy([_|T], X, Galaxy) :- NextX is X + 1, galaxy(T, NextX, Galaxy).
galaxy(Image, Galaxy) :- galaxy(Image, 0, Galaxy).

between(Start, X, End) :- Start < X, X < End.
between(Start, X, End) :- End < Start, between(End, X, Start).
between(List, Start, X, End) :- member(X, List), between(Start, X, End).

distance_x([X1, _], [X2, _], EmptyRows, Expansion, Distance) :-
  findall(EmptyRowBetween, between(EmptyRows, X1, EmptyRowBetween, X2), EmptyRowsBetween),
  length(EmptyRowsBetween, EmptyRowsBetweenCount),
  Distance is abs(X2 - X1) + EmptyRowsBetweenCount * (Expansion - 1).
distance_y([_, Y1], [_, Y2], EmptyCols, Expansion, Distance) :-
  findall(EmptyColBetween, between(EmptyCols, Y1, EmptyColBetween, Y2), EmptyColsBetween),
  length(EmptyColsBetween, EmptyColsBetweenCount),
  Distance is abs(Y2 - Y1) + EmptyColsBetweenCount * (Expansion - 1).

smaller_galaxy([X1, _], [X2, _]) :- X1 < X2.
smaller_galaxy([X, Y1], [X, Y2]) :- Y1 < Y2.

galaxy_distance(Galaxies, EmptyRows, EmptyCols, Distance, Expansion) :-
  member(G1, Galaxies), member(G2, Galaxies), smaller_galaxy(G1, G2),
  distance_x(G1, G2, EmptyRows, Expansion, DistanceX),
  distance_y(G1, G2, EmptyCols, Expansion, DistanceY),
  Distance is DistanceX + DistanceY.

result(Image, Expansion, DistanceSum) :-
  findall(Row, empty_row(Image, Row), EmptyRows),
  findall(Col, empty_col(Image, Col), EmptyCols),
  findall(Galaxy, galaxy(Image, Galaxy), Galaxies),
  findall(Distance, galaxy_distance(Galaxies, EmptyRows, EmptyCols, Distance, Expansion), Distances),
  sumlist(Distances, DistanceSum).

/* required for loadData */
data_line(Data, Line) :- string_chars(Line, Data).