day(13). groupData. testResult(400).

:- use_module(lib/solve), use_module(lib/matrix).

row_diff_1([H|T1], [H|T2]) :- row_diff_1(T1, T2).
row_diff_1([H1|T], [H2|T]) :- H1 \= H2.

pattern_diff_1([H1|T1], [H1|T2]) :- !, pattern_diff_1(T1, T2).
pattern_diff_1([H1|T1], [H2|T2]) :- row_diff_1(H1, H2), (append(T1, _, T2) ; append(T2, _, T1)).

mirror(P, M, 1) :- pattern_diff_1(P, M), !.
mirror([H|T], M, Reflection) :- mirror(T, [H|M], NextReflection), Reflection is NextReflection + 1.

mirror([H|T], Reflection) :- mirror(T, [H], Reflection).

score(Pattern, Score) :- mirror(Pattern, RawScore), !, Score is RawScore * 100.
score(Pattern, Score) :- transpose(Pattern, TransposedPattern), mirror(TransposedPattern, Score).

result(Patterns, ScoreSum) :- mapsum(Patterns, score, ScoreSum).

/* required for loadData */
data_line(Data, Line) :- string_chars(Line, Data).
