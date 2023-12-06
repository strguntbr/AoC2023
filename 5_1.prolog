day(5). testResult(35). groupData.

:- use_module(lib/solve).

do_map(Type, Dest, Source, Length, NewType) :-
  MaxSource is Source + Length,
  Type >= Source, Type =< MaxSource,
  NewType is Type + Dest - Source.

map(Type, [], Type).
map(Type, [[Dest,Source,Length]|_], NewType) :- do_map(Type, Dest, Source, Length, NewType), !.
map(Type, [_|T], NewType) :- map(Type, T, NewType).

map_all(Locations, [], Locations).
map_all(Types, [FirstTypeMapping|OtherTypeMappings], Locations) :-
  maplist([Seed,Location]>>map(Seed,FirstTypeMapping,Location), Types, NextType),
  map_all(NextType, OtherTypeMappings, Locations).

result([[Seeds]|Mappings], MinLocation) :-
  map_all(Seeds, Mappings, Locations),
  min_list(Locations, MinLocation).

/* required for loadData */
data_line(Seeds, Line) :-
  split_string(Line, " ", " ", ["seeds:"|SeedStrings]),
  maplist(number_string, Seeds, SeedStrings), !.
data_line([DestStart, SourceStart, Length], Line) :-
  split_string(Line, " ", " ", Numbers),
  maplist(number_string, [DestStart, SourceStart, Length], Numbers), !.
data_line(Header, Header).
