doof(1,[doo2(1,2),doo2(1,3),doo2(1,4),doo2(1,5),doo2(1,6),doo2(1,7)]).
doo2(1,doo2(1,2)).
doo2(1,doo2(1,3)).
doo2(1,doo2(1,4)).
doo2(1,doo2(1,5)).
doo2(1,doo2(1,6)).
doo2(1,doo2(1,7)).

bm:-
    flag(count,_,0),
	repeat,
		flag(count,C,C+1),
		doof(1,L),
		member(doo2(1,7),L),
		C==2000000,
	!.

bm2:-
    flag(count,_,0),
	repeat,
		flag(count,C,C+1),
		doo2(1,doo2(1,7)),
		C==2000000,
	!.
	