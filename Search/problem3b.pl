

maxcolumn(3).
maxrow(3).

initboard(B) :-
	maxcolumn(M),
	initboard(M, B).

initboard(0, []).

initboard(Max,[(Max, 0, .)|L]) :-
	Max > 0,
	Max1 is Max - 1,
	initboard(Max1, L), !.

checkmoves(Board, Moves) :-
	maxrow(Mrow),
	maxcolumn(Mcol),
	allcolumns(Mcol, L),
	checkmoves(Mrow, L, Board, Moves).
	
checkmoves(_, [], _, []).

checkmoves(Mrow, [C|L], Board, Moves) :-
	member((C, Mrow, _), Board),
	checkmoves(Mrow, L, Board, Moves).

checkmoves(Mrow, [C|L], Board, [C|Moves]) :-
	checkmoves(Mrow, L, Board, Moves).

allcolumns(0,[]):-!.

allcolumns(N,[N|L]):-
	N1 is N-1,allcolumns(N1,L).



makemove(Col, Plr, Board, [(Col, R1, Plr)|Board]) :-
	findall(R, member((Col, R, _), Board), [Hrow|_]),
	maxrow(Mrow),
	Hrow < Mrow,
	R1 is (Hrow) + 1.

applymoves([], _, _, []).

applymoves([Move|Movelist], Player, Board, [Newboard|Boardlist]) :-
	makemove(Move, Player, Board, Newboard),
	applymoves(Movelist, Player, Board, Boardlist).

printboard(Board) :-
	maxcolumn(C),
	maxrow(R),
	printboard(Board, C, 1, R), !, nl.


printboard(Board, Maxcol, Column, Row) :-
	Row > 0,	
	(member((Column, Row, Player), Board) -> write(Player);write('.') ),
	(Column == Maxcol -> ( Column1 is 1, Row1 is Row - 1, nl );   
	( Column1 is Column + 1, Row1 is Row )),
	printboard(Board, Maxcol, Column1, Row1).


printboard(_, _, _, 0).


win(Board, Player) :-
	% a vertical three in a row
	member((C, R, Player), Board),(Player == x ; Player == o),
	R1 is R + 1, member((C, R1, Player), Board),
	R2 is R + 2, member((C, R2, Player), Board);
	
	member((C,R,Player),Board),(Player == x ; Player==o),
	C1 is C + 1,member((C1,R,Player),Board),
	C2 is C + 2,member((C2,R,Player),Board),!
	;

	member((C,R,Player),Board),(Player == x ; Player==o),
	C1 is C + 1,R1 is R + 1,member((C1,R1,Player),Board),
	C2 is C + 2,R2 is R + 2,member((C2,R2,Player),Board),!
	;

	member((C,R,Player),Board),(Player == x ; Player==o),
	C1 is C - 1,R1 is R - 1,member((C1,R1,Player),Board),
	C2 is C - 2,R2 is R - 2,member((C2,R2,Player),Board).	

isterminal(Board) :-
	maxcolumn(Mcol),
	maxrow(Mrow),
	findall(C,member((C,Mrow,_),Board),Completed),
	length(Completed, Mcol).

isterminal(Board) :-
	win(Board,_).

utility(Board, Value) :-
	win(Board, x),
	maxcolumn(Mcol),length(Board,Len),
	Value is ((-1)*(100-Len-Mcol))
	,!;
	win(Board, o),maxcolumn(Mcol1),
	maxcolumn(Mcol),length(Board,Len1),
	Value is (100-Len1-Mcol1)
	,!;
	Value is 0.

nextplayer(x, o).

nextplayer(o, x).


alphabeta(Board, BestState, Util,Alphavalue,Betavalue,PrevPlayer) :-
	isterminal(Board),
	utility(Board, Util), BestState = Board,!;
	nextplayer(PrevPlayer,Player),
	checkmoves(Board,Moves),
	length(Moves, L), L > 0,
	applymoves(Moves,Player,Board,ChildStates),
	(Player==x ->
		findbeststate(ChildStates,BestState,Alphavalue,Betavalue,200,[],Util,Player)
	;
		findbeststate(ChildStates,BestState,Alphavalue,Betavalue,-200,[],Util,Player)
	)
.	

findbeststate([Head|Tail],BestState,Alphavalue,Betavalue,SofarbestUtil,SofarbestBoard,Util,Player):-
	alphabeta(Head,_,Util1,Alphavalue,Betavalue,Player),

	(Player==x -> 
		(SofarbestUtil<Util1 -> 
			NewbestUtil is SofarbestUtil,NewbestBoard=SofarbestBoard
		;
			NewbestUtil is Util1,NewbestBoard=Head)
	;
		(SofarbestUtil>Util1 -> 
			NewbestUtil is SofarbestUtil,NewbestBoard=SofarbestBoard
		;
			NewbestUtil is Util1,NewbestBoard=Head)
	),
	(canPrune(Player, AlphaValue, BetaValue, NewbestUtil) ->
		Util = NewbestUtil, BestState = NewbestBoard
	;
		(Player==o ->
			Betavalue1 = Betavalue,
			(NewbestUtil>Alphavalue ->
				Alphavalue1 = NewbestUtil
			;
				Alphavalue1 = Alphavalue)
		;
			Alphavalue1 = Alphavalue,
			(NewbestUtil<Betavalue ->
				Betavalue1 = NewbestUtil
			;
				Betavalue1 = Betavalue
			)
		),
		findbeststate(Tail,BestState,Alphavalue1,Betavalue1,NewbestUtil,NewbestBoard,Util,Player))
		)
		
	).
	

canPrune(Player, Alphavalue, Betavalue, NewbestUtil) :- 
	(Player==o ->
		Betavalue =< NewbestUtil
			
	;	
		Alphavalue >= NewbestUtil
	).


findbeststate([],SofarbestBoard,_,_,SofarbestUtil,SofarbestBoard,SofarbestUtil,_).
	
play :-
	initboard(B),
	play(B, o).

play(B, Player) :-
	printboard(B),
	win(B, P),
	write(P), write(' wins!'), nl, nl
	;
	
	isterminal(B),
	write('Draw!'), nl, nl
	;
	
	alphabeta(B,BestState,Util,-200,200,Player),
	%minimax(B,BestState,Util,Player),
	%alphabeta(B, Player),
	write(Util), nl, nl,
	nextplayer(Player, Nplayer),
	play(BestState,Nplayer),!.

humanplay :-
	initboard(B),
	printboard(B),
	hplay(B, x).

hplay(B, Player) :-
	win(B, P),
	write(P), write(' wins!'), nl, nl
	;
	isterminal(B),
	write('Draw!'), nl, nl
	;
	read(C),
	(   makemove(C, Player, B, N) ->
	(   printboard(N),
	    (   
		win(N, P),
		write(P), write(' wins!'), nl, nl
	    ;   
		isterminal(N),
		write('Draw!'), nl, nl
	    ;   
		alphabeta(N,BestState,Util,-200,200,Player),
		%minimax(N,BestState,Util,Player),
		printboard(BestState),
		write(Util), nl, nl,
		hplay(BestState, Player), !))
	;   
	    write('Invalid move.'), nl, write('Try again.'), nl, nl,
	    printboard(B),
	    hplay(B, Player),!
).






