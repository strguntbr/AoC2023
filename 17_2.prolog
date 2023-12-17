day(17). testResult(94). testResult('test2', 71).

:- use_module(lib/solve).

neighbor([[X,Y], leftright], [[Xn,Y], updown]) :-
  between(-10, 10, Xd), (Xd < -3 ;  Xd > 3), Xn is X + Xd,
  unknownNode([[Xn,Y], updown]).
neighbor([[X,Y], updown], [[X,Yn], leftright]) :-
  between(-10, 10, Yd), (Yd < -3 ;  Yd > 3), Yn is Y + Yd,
  unknownNode([[X,Yn], leftright]).

range(A, B, From, To) :- A =< B -> From is A+1, To = B ; From = B, To is A-1.

lostHeat([X1,Y], [X2,Y], LostHeat) :-
  range(X1, X2, Xs, Xe),
  aggregate_all(sum(HeatLoss), (between(Xs, Xe, X), cityBlock([X,Y], HeatLoss)), LostHeat).
lostHeat([X,Y1], [X,Y2], LostHeat) :-
  range(Y1, Y2, Ys, Ye),
  aggregate_all(sum(HeatLoss), (between(Ys, Ye, Y), cityBlock([X,Y], HeatLoss)), LostHeat).

updateNeighbor([NeighborPos, NeighborDir], [NodePos, _], HeatLoss) :-
  lostHeat(NeighborPos, NodePos, AdditionalLostHeat),
  NewNeighborHeatLoss is HeatLoss + AdditionalLostHeat,
  (
    (heatLoss([NeighborPos, NeighborDir], CurrentNeighborHeatLoss) -> NewNeighborHeatLoss < CurrentNeighborHeatLoss ; true)
    -> retractall(heatLoss([NeighborPos, NeighborDir], _)), assert(heatLoss([NeighborPos, NeighborDir], NewNeighborHeatLoss))
    ; true
  ).

calc(TotalHeatLoss) :-
  aggregate_all(min(HeatLoss, Node), (heatLoss(Node, HeatLoss), unknownNode(Node)), min(NextNodeHeatLoss, NextNode)),
  (
    NextNode = [[1,1], _]
    -> TotalHeatLoss = NextNodeHeatLoss
    ; step(NextNode, NextNodeHeatLoss), calc(TotalHeatLoss)
  ).

step(Node, HeatLoss) :- 
  retract(unknownNode(Node)), !,
  foreach(neighbor(Node, Neighbor), updateNeighbor(Neighbor, Node, HeatLoss)).

size([H|T], X, Y) :- length([H|T], X), length(H, Y).

setup([], _, _).
setup([H|T], X, Y) :-
  assert(cityBlock([X,Y], H)),
  assert(unknownNode([[X,Y], leftright])),
  assert(unknownNode([[X,Y], updown])),
  Yn is Y+1, setup(T, X, Yn).

setup([], _).
setup([H|T], X) :- setup(H, X, 1), Xn is X+1, setup(T, Xn).

setup(Map) :-
  setup(Map, 1), size(Map, H, W),
  assert(heatLoss([[H, W], leftright], 0)),
  assert(heatLoss([[H, W], updown], 0)).

result(Map, HeatLoss) :-
  setup(Map),
  calc(HeatLoss).

/* required for loadData */
resetData :- retractall(cityBlock(_,_)), retractall(unknownNode(_)), retractall(heatLoss(_, _)).
data_line(Data, Line) :- string_chars(Line, Chars), maplist(number_char, Data, Chars).
number_char(Number, Char) :- number_chars(Number, [Char]).
