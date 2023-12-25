day(25). testResult(54).

:- use_module(lib/solve).

connection(A, B) :- connection_(A, B) ; connection_(B, A).

connections(Component, OtherComponents, Count) :- aggregate_all(count, (connection(Component, OtherComponent), member(OtherComponent, OtherComponents)), Count).

pick(Components, AlreadyPicked, PickedComponent, RemainingComponents) :-
  aggregate_all(
    min(Count, [Component,Remaining]),
    (
      select(Component, Components, Remaining),
      connections(Component, Remaining, CountOut),
      connections(Component, AlreadyPicked, CountIn),
      Count is CountOut-CountIn
    ), 
    min(_, [PickedComponent,RemainingComponents])).

outsideConnections([], Components, PickedComponents, UnpickedComponents) :- !,
  select(Component, Components, RemainingUnpickedComponents),
  connections(Component, RemainingUnpickedComponents, Count),
  Count >=3, !,
  outsideConnections([Component], RemainingUnpickedComponents, PickedComponents, UnpickedComponents).
outsideConnections(PickedComponents, UnpickedComponents, FinalPickedComponents, FinalUnpickedComponents) :-
  aggregate_all(sum(Count), ((member(APickedComponent, PickedComponents), connections(APickedComponent, UnpickedComponents, Count))), OutsideConnections),
  (
    OutsideConnections = 3
    -> [FinalPickedComponents, FinalUnpickedComponents] = [PickedComponents, UnpickedComponents]
    ; (
      pick(UnpickedComponents, PickedComponents, Component, RemainingUnpickedComponents),
      outsideConnections([Component|PickedComponents], RemainingUnpickedComponents, FinalPickedComponents, FinalUnpickedComponents)
    )
  ).

result(Data, Result) :-
  append(Data, D), sort(D, Components),
  outsideConnections([], Components, Picked, Unpicked),
  length(Picked, PickedCount), length(Unpicked, UnpickedCount),
  Result is PickedCount * UnpickedCount.

/* required for loadData */
resetData :- retractall(connection_(_,_)).
data_line([Cur|Nexts], Line) :-
  split_string(Line, " ", ":", [Cur|Nexts]),
  foreach(member(Next, Nexts), assert(connection_(Cur,Next))).
