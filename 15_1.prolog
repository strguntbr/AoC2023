day(15). testResult(1320).

:- use_module(lib/solve).

hash([], Hash, Hash).
hash([H|T], Cur, Hash) :- char_code(H, Code), Next is mod((Cur + Code) * 17, 256), hash(T, Next, Hash).
hash(Step, Hash) :- hash(Step, 0, Hash).

result([Steps], VerificationNumber) :- mapsum(Steps, hash, VerificationNumber).

/* required for loadData */
data_line(Steps, Line) :- split_string(Line, ",", "", Split), maplist(string_chars, Split, Steps).
