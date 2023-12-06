day(5). testResult(46). groupData.

:- use_module(lib/solve).

/* 1: |RR|           */
/* 2:     |----|     */
intersect(Start1, End1, Start2, _, [], [[Start1,LenghIntersection]]) :-
  End1 =< Start2,
  LenghIntersection is End1 - Start1.
/* 1: |RRRIII|       */
/* 2:      |----|    */
intersect(Start1, End1, Start2, End2, [[Start2,LenghIntersection]], [[Start1, LengthRest]]) :-
  Start1 < Start2, End1 > Start2, End1 =< End2,
  LenghIntersection is End1 - Start2, LengthRest is Start2 - Start1.
/* 1: |RRRIIIIIIRRR| */
/* 2:     |----|     */
intersect(Start1, End1, Start2, End2, [[Start2,LenghIntersection]], [[Start1, LengthRest1], [End2, LengthRest2]]) :-
  Start1 < Start2, End1 > End2,
  LenghIntersection is End2 - Start2, LengthRest1 is Start2 - Start1, LengthRest2 is End1 - End2.
/* 1:      |II|      */
/* 2:     |----|     */
intersect(Start1, End1, Start2, End2, [[Start1,LenghIntersection]], []) :-
  Start1 >= Start2, End1 =< End2,
  LenghIntersection is End1 - Start1.
/* 1:      |IIIIRRR| */
/* 2:     |----|     */
intersect(Start1, End1, Start2, End2, [[Start1,LenghIntersection]], [[End2, LengthRest]]) :-
  Start1 >= Start2, Start1 < End2, End1 > End2,
  LenghIntersection is End2 - Start1, LengthRest is End1 - End2.
/* 1:           |RR| */
/* 2:     |----|     */
intersect(Start1, End1, _, End2, [], [[Start1, LengthRest]]) :-
  Start1 >= End2,
  LengthRest is End1 - Start1.

shift([Start, Length], Dest, Source, [MappedStart, Length]) :- MappedStart is Start + Dest - Source.

do_map_single_type_range([TypesStart, Length], [Dest,Source,LengthSource], NotMappedRanges, MappedLocationRanges) :-
  TypesEnd is TypesStart+Length,
  EndSource is Source+LengthSource,
  intersect(TypesStart, TypesEnd, Source, EndSource, IntersectionRanges, NotMappedRanges),
  maplist([Types, MappedTypes]>>shift(Types, Dest, Source, MappedTypes), IntersectionRanges, MappedLocationRanges).

do_map([], _, [], []).
do_map([FirstTypes|OtherTypes], MapEntry, UnchangedTypes, MappedTypes) :-
  do_map_single_type_range(FirstTypes, MapEntry, UnchangedFirstTypes, MappedFirstTypes),
  do_map(OtherTypes, MapEntry, UnchangedOtherTypes, MappedOtherTypes),
  append(UnchangedFirstTypes, UnchangedOtherTypes, UnchangedTypes),
  append(MappedFirstTypes, MappedOtherTypes, MappedTypes).

map(Types, [], Types).
map(Types, [FirstMappingEntry|OtherMappingEntries], MappedTypes) :-
  do_map(Types, FirstMappingEntry, UnchangedTypes, FirstMappedTypes),
  map(UnchangedTypes, OtherMappingEntries, OtherMappedTypes),
  append(FirstMappedTypes, OtherMappedTypes, MappedTypes).

map_all(Locations, [], Locations).
map_all(Types, [[_|FirstTypeMapping]|OtherTypeMappings], Locations) :-
  map(Types, FirstTypeMapping,  MappedTypes),
  map_all(MappedTypes, OtherTypeMappings, Locations).

range_start([Start,_], Start).

result([[Seeds]|Mappings], MinLocation) :-
  map_all(Seeds, Mappings, Locations),
  maplist(range_start, Locations, Starts),
  min_list(Starts, MinLocation).

/* required for loadData */
data_line(SeedPairs, Line) :-
  split_string(Line, " ", " ", ["seeds:"|SeedStrings]),
  maplist(number_string, Seeds, SeedStrings), !,
  seed_pairs(Seeds, SeedPairs).
data_line([DestStart, SourceStart, Length], Line) :-
  split_string(Line, " ", " ", Numbers),
  maplist(number_string, [DestStart, SourceStart, Length], Numbers), !.
data_line(Header, Header).

seed_pairs([], []).
seed_pairs([Start|[Length|T]], [[Start,Length]|TPairs]) :- seed_pairs(T, TPairs).
