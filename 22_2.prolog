day(22). testResult(7).

:- use_module(lib/solve).

notlower(brick{s: [_,_,Z1], e: _}, brick{s: [_,_,Z2], e: _}) :-  Z1 >= Z2.

next(Bricks, Next, Remaining) :-
  select(Next, Bricks, Remaining),
  foreach(member(T, Remaining), notlower(T, Next)).

brick_block(Brick, Block) :- member(Block, Brick).
bricks_block(Bricks, Block) :- member(Brick, Bricks), brick_block(Brick, Block).

brickXY(brick{s: [X,Y,_], e: [X,Y,_]}, X, Y).
brickXY(brick{s: [X,Ys,Z], e: [X,Ye,Z]}, X, Y) :- between(Ys, Ye, Y).
brickXY(brick{s: [Xs,Y,Z], e: [Xe,Y,Z]}, X, Y) :- between(Xs, Xe, X).

brickXYMaxZ(brick{s: [X,Y,Zs], e: [X,Y,Ze]}, X, Y, MaxZ) :- MaxZ is max(Zs, Ze).
brickXYMaxZ(brick{s: [X,Ys,Z], e: [X,Ye,Z]}, X, Y, Z) :- between(Ys, Ye, Y).
brickXYMaxZ(brick{s: [Xs,Y,Z], e: [Xe,Y,Z]}, X, Y, Z) :- between(Xs, Xe, X).

brickXYMinZ(brick{s: [X,Y,Zs], e: [X,Y,Ze]}, X, Y, MinZ) :- MinZ is min(Zs, Ze).
brickXYMinZ(brick{s: [X,Ys,Z], e: [X,Ye,Z]}, X, Y, Z) :- between(Ys, Ye, Y).
brickXYMinZ(brick{s: [Xs,Y,Z], e: [Xe,Y,Z]}, X, Y, Z) :- between(Xs, Xe, X).

dropBrick(Brick, Bricks, [DroppedBrick|Bricks]) :-
  Brick = brick{s: [Xs,Ys,Zs], e: [Xe,Ye,Ze]},
  (aggregate_all(max(Z), (brickXY(Brick,X,Y), member(B, Bricks), brickXYMaxZ(B,X,Y,Z)), MaxZ) -> true ; MaxZ=0),
  Zds is MaxZ+1, Zde is Ze - Zs + MaxZ+1,
  DroppedBrick = brick{s: [Xs,Ys,Zds], e: [Xe,Ye,Zde]}.

drop([], FinalDroppedBricks, FinalDroppedBricks).
drop(Bricks, DroppedBricks, FinalDroppedBricks) :-
  once(next(Bricks, Next, Remaining)),
  dropBrick(Next, DroppedBricks, NextDroppedBricks),
  drop(Remaining, NextDroppedBricks, FinalDroppedBricks).

supports(Support, Brick) :- brickXYMaxZ(Support, X, Y, Z1), brickXYMinZ(Brick, X, Y, Z2), Z2 is Z1+1.

unsupported(Brick, Bricks) :- \+ (member(Support, Bricks), supports(Support, Brick)).

removeBrick(Bricks, Brick) :-
  select(Brick, Bricks, RemainingBricks),
  forall(
    ( member(C, Bricks), supports(Brick, C) ),
    once(( member(S, RemainingBricks), supports(S, C) ))
  ).

removeBricks(Bricks, Remove, AllRemoved) :-
  subtract(Bricks, Remove, NextBricks),
  findall(S, (member(S, NextBricks), once((member(R, Remove), supports(R, S), unsupported(S, NextBricks)))), Unsupported),
  (
    Unsupported == []
    -> AllRemoved = []
    ; (removeBricks(NextBricks, Unsupported, NextRemoved), append(NextRemoved, Unsupported, AllRemoved))
  ).

result(Bricks, Result) :-
  drop(Bricks, [], DroppedBricks),
  aggregate_all(sum(C), (member(B, DroppedBricks), removeBricks(DroppedBricks, [B], Removed), length(Removed, C)), Result).

/* required for loadData */
resetData :- retractall(cache(_,_)).
data_line(brick{s: Start, e: End}, Line) :-
  split_string(Line, "~,", "", NumberStrings),
  maplist(number_string, Numbers, NumberStrings),
  numbers_startEnd(Numbers, Start, End).

numbers_startEnd([Xs,Y,Z,Xe,Y,Z], [Xs,Y,Z], [Xe,Y,Z]) :- Xs < Xe.
numbers_startEnd([X,Ys,Z,X,Ye,Z], [X,Ys,Z], [X,Ye,Z]) :- Ys < Ye.
numbers_startEnd([X,Y,Zs,X,Y,Ze], [X,Y,Zs], [X,Y,Ze]) :- Zs < Ze.
numbers_startEnd([Xe,Y,Z,Xs,Y,Z], [Xs,Y,Z], [Xe,Y,Z]) :- Xs < Xe.
numbers_startEnd([X,Ye,Z,X,Ys,Z], [X,Ys,Z], [X,Ye,Z]) :- Ys < Ye.
numbers_startEnd([X,Y,Ze,X,Y,Zs], [X,Y,Zs], [X,Y,Ze]).
