day(20). testResult(32000000). testResult("test2", 11687500).

:- use_module(lib/solve).

invertState(high, low).
invertState(low, high).

toggle(Module, NewState) :-
  state(Module, OldState),
  invertState(OldState, NewState),
  assertState(Module, NewState).

assertPulse(Name, Forward, State) :-
  assert(countPulse(State)),
  assert(pulse(Name, Forward, State)).

assertState(Module, State) :-
  retractall(state(Module, _)),
  assert(state(Module, State)).

assertState(Module, Input, State) :-
  retractall(state(Module, Input, _)),
  assert(state(Module, Input, State)).

allInputsHigh(Name) :- foreach(input(From, Name), state(Name, From, high)).

evalPulse(To, From, Type) :- module(To, From, Type), !.
evalPulse(_, _, _).

evalAll :-
  pulse(From, To, Type)
  -> (retract(pulse(From, To, Type)), evalPulse(To, From, Type), evalAll)
  ; true.

button :-
  assert(countPulse(low)),
  broadcast,
  evalAll.

result(_, Result) :-
  foreach(between(1, 1000, _), button),
  aggregate_all(count, countPulse(high), High),
  aggregate_all(count, countPulse(low), Low),
  Result is High*Low.

/* required for loadData */
resetData :-
  retractall(state(_,_)), retractall(state(_,_,_)),
  retractall(module(_,_, _)), retractall(input(_, _)),
  retractall(broadcast),
  retractall(pulse(_,_,_)),
  retractall(countPulse(_)).
data_line([[Type, Name],Forwards], Line) :-
  split_string(Line, "-,", "> ", [ModuleString|Forwards]),
  module_string([Type, Name], ModuleString),
  assertModule(Type, Name, Forwards).

module_string([flipflop,Name], String) :- string_concat("%", Name, String), !.
module_string([conjunction,Name], String) :- string_concat("&", Name, String), !.
module_string([broadcast,"broadcaster"], "broadcaster").

assertModule(flipflop, Name, Forwards) :-
  assertState(Name, low),
  assert(module(Name, _, high)),
  assert((
    module(Name, _, low) :-
      toggle(Name, State),
      foreach(
        member(Forward, Forwards),
        assertPulse(Name, Forward, State)
      )
  )),
  foreach(member(To, Forwards), assert(input(Name, To))).
assertModule(conjunction, Name, Forwards) :-
  assert((
    module(Name, Input, high) :-
      assertState(Name, Input, high),
      (
        allInputsHigh(Name) ->
          foreach(
            member(F1, Forwards),
            assertPulse(Name, F1, low)
          )
        ;
          foreach(
            member(F2, Forwards),
            assertPulse(Name, F2, high)
        )
      )
  )),
  assert((
    module(Name, Input, low) :-
      assertState(Name, Input, low),
      foreach(
        member(Forward, Forwards),
        assertPulse(Name, Forward, high)
     )
  )),
  foreach(member(To, Forwards), assert(input(Name, To))).
assertModule(broadcast, Name, Forwards) :-
  assert((
    broadcast :-
      foreach(
        member(Forward, Forwards),
        assertPulse(Name, Forward, low)
      )
  )),
  foreach(member(To, Forwards), assert(input(Name, To))).
