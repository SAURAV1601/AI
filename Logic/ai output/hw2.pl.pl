:-dynamic kb/1.

:- op(1000, fx, '~').
:- op(1010, yfx, '^').
:- op(1020, yfx, 'v').
:- op(1030, xfx, '=>').
:- op(1040, xfx, '<=>').

commutativityand(A ^ B, B ^ A).
commutativityor(A v B, B v A).


equiv(~(~A), A) :-
	% format('Double negation~n'),
	% the format below just prints things out,
	% much like a printf.  You can use write and
	% writeln if you prefer.
	%format('~~(~~~w) -----> ~w~n', [A, A]),
	!.
	

equiv(A <=> B,X):-
	format('Bi-conditional elimination~n'),
	format('(~w) <=> (~w)  ----->  ((~w) => (~w)) ^ ((~w) => (~w))~n',[A,B,A,B,B,A]),
	equiv(A=>B, A1),
	equiv(B=>A, B1),
	format('(~w)~n',[A1]),
	format('(~w)~n',[B1]),
	equiv(A1 ^ B1,X),!.	
	
equiv(A => B, X) :-
	format('Implication elimination~n'),
	format('(~w) => (~w) -----> ~~(~w) v (~w)~n', [A, B, A, B]),
	equiv(A, A1),
	format('(~w)~n',[A1]),
	equiv(B, B1),
	format('(~w)~n',[B1]),
	equiv(~A1 v B1,X),
	!.
	
equiv(A ^ B,A1 ^ B1):-
	format('inside and~n'),
	%format('((~w) v (~w))~n',[A,B]),
	equiv(A,A1),
	%format('(~w)~n',[A1]),
	equiv(B,B1),
	%format('(~w)~n',[B1]),
	!.
	
equiv(A v B,A1 v B1):-
	format('inside or~n'),
	format('((~w) v (~w))~n',[A,B]),
	equiv(A,A1),
	equiv(B,B1),
	!.
	
equiv(~(A v B),~A1 ^ ~B1):-
	format('demorgan~n'),
	format('((~w) v (~w))~n',[A,B]),
	equiv(A,A1),
	%format('(~w)~n',[A1]),
	equiv(B,B1),
	%format('(~w)~n',[B1]),
	!.
	
equiv(~(A ^ B),~A1 v ~B1):-
	format('demorgan~n'),
	format('((~w) v (~w))~n',[A,B]),
	equiv(A,A1),
	equiv(B,B1),
	!.
	
equiv(A, A).

equiv(~A,~A).

distribute(A ^ B,A1 ^ B1):- 
	distribute(A,A1),
	distribute(B,B1),!.

distribute(A v B,X):-
	distribute(A,A1),
	distribute(B,B1),
	distributivity(A1,B1,X),!.

distribute(~A,~A).
distribute(A,A).	

distributivity(A ^ B,C,A1 ^ B1):-
	distributivity(A,C,A1),
	distributivity(B,C,B1).

distributivity(A,B ^ C,A1 ^ B1):-
	A \== (_ ^ _),
	distributivity(A,B,A1),
	distributivity(A,C,B1).
	
distributivity(A,B,A v B):-
	A \== (_ ^ _),
	B \== (_ ^ _).

convert_to_list(A ^ B, Res) :-
     convert_to_list(A, A1),
	 convert_to_list(B, B1),
	 append(A1, B1, Res), !.

convert_to_list(A, [Res]) :-
     remove_or(A, Res).

% Flatten OR (associativity)
remove_or(A v B, Res) :-
     remove_or(A, LA),
	 remove_or(B, LB),
	 append(LA, LB, Res), !.

remove_or(A, [A]).

cnf(A, E) :-
	equiv(A, B),
	distribute(B,C),
	convert_to_list(C,E).

% Is the disjunction a tautology?
% (includes complementary literals, e.g. a v ~a)

% hint: see how complentary literals are
% identified here.  the same idea may be useful in your
% implementation of resolution.

nottautology([~A|R]) :-
	member(A, R),
	!, fail.

nottautology([A|R]) :-
	member(~A, R),
	!, fail.

nottautology([_|R]) :-
	nottautology(R).

nottautology([]).

% remove duplicates for a list

removedups([], []).

removedups([A|B], [A|C]) :-
	subtract(B, [A], D),
	removedups(D, C).

% already in kb?

notinkb(P) :-
	findall(Q, kb(Q), X),
	compareeach(P, X), !.

compareeach(P, [A|R]) :-
	(   sameset(P, A) -> !, fail ; true),
	compareeach(P, R).

compareeach(_, []).

sameset(P, Q) :-
	length(P, A),
	length(Q, B),
	A == B,
	subset(P, Q), !.

subset([X|R],S) :- member(X,S), subset(R,S).

subset([],_).

% Clear the knowledge base.

kbclear :-
	retractall(kb(_)).

% Add to the knowledge base.

kbadd(P) :-
	cnf(P, C),
	addeach(C).

% Put each disjunction into the KB,
% but don't add it if it is a tautology.
% (that would just be adding TRUE to the
% KB, which doesn't add any knowledge.)

addeach([A|L]) :-
	removedups(A, B),
	(  ( nottautology(B), notinkb(B) ) -> assert(kb(B)) ; true),
	addeach(L).

addeach([]).

% Resolution

% Resolve two clauses:
% Given two clauses, A and B, find all of
% the possible clauses that result from
% resolving A and B, without tautologies.

resolve(Clause1, Clause2, RClause) :-
        % FILL IN HERE
	run_resolve(Clause1, Clause2, RClause, Clause1).

run_resolve(Clause1, Clause2, RClause, [F|R]) :-
         equiv(~(F), NF),	 
	 member(NF,Clause2),
	 subtract(Clause2, [NF], T1),
	 subtract(Clause1, [F], T2),
	 union(T1,T2,UT),
	 run_resolve(Clause1, Clause2, ML, R),
	 RClause = [UT | ML], !.

run_resolve(Clause1, Clause2, RClause, [F|R]) :-
	equiv(~(F), NF),
	not(member(NF,Clause2)),
	run_resolve(Clause1, Clause2, RClause, R), !.

run_resolve(_, _, [], []).

expand_kb :-
       findall(OR, kb(OR), ORs),
       length(ORs, Ol),
       findall(OR1, (kb(OR1), findall(OR2, (kb(OR2), OR1 \== OR2, resolve(OR1, OR2, LOR), addeach(LOR)), _)), _),
       findall(NR, kb(NR), NRs),
       length(NRs, Nl),
       Al is Nl - Ol,
       (Al > 0 -> expand_kb;true).

kbresolve :-
	% gets a list of the clauses in the KB
	% (you may not need this; I used it)
	expand_kb,
	findall(X, kb(X), Lkb),
	length(Lkb,C),
	% print the number of clauses in the KB
	write(C), writeln(' clauses in KB.').

kbprint :-
	findall(P, (kb(P), writeln(P)), L),
	length(L, N),
	format('~d clauses~n', [N]).

kbentails(A) :-
	cnf(A, B),
	% FILL IN HERE
	entails(B).

entails([L | List]) :-
	not(notinkb(L)), entails(List).

entails([]).
:- kbclear.
	
	
