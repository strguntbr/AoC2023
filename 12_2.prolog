testResult(525152).

:- include('12.common.prolog').

prepare_condition_record([RawSprings, RawDamagedGroups], [UnfoldedSprings, UnfoldedDamagedGroups]) :-
  append([RawSprings,['?'],RawSprings,['?'],RawSprings,['?'],RawSprings,['?'],RawSprings,['.']], UnfoldedSprings),
  append([RawDamagedGroups,RawDamagedGroups,RawDamagedGroups,RawDamagedGroups,RawDamagedGroups], UnfoldedDamagedGroups).
