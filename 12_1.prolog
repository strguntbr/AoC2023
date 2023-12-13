testResult(21).

:- include('12.common.prolog').

prepare_condition_record([RawSprings, DamagedGroups], [PreparedSprings, DamagedGroups]) :- append(RawSprings, ['.'], PreparedSprings).
