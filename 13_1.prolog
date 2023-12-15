day(13). groupData. testResult(405).

:- use_module(lib/solve), use_module(lib/matrix).

mirror([H|T], M, 1) :- append(M, _, [H|T]), !.
mirror([H|T], M, 1) :- append([H|T], _, M), !.
mirror([H|T], M, Reflection) :- mirror(T, [H|M], NextReflection), Reflection is NextReflection + 1.

mirror([H|T], Reflection) :- mirror(T, [H], Reflection).

score(Pattern, Score) :- mirror(Pattern, RawScore), !, Score is RawScore * 100.
score(Pattern, Score) :- transpose(Pattern, TransposedPattern), mirror(TransposedPattern, Score).

result(Patterns, ScoreSum) :- mapsum(Patterns, score, ScoreSum).

/* required for loadData */
data_line(Data, Line) :- string_chars(Line, Data).
