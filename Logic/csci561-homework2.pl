%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% USC CSCI 561 Spring 2012
%
% Homework 2
% Propositional Logic
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% In this assignment you will implement
% a simple resolution system for
% propositional logic.  Given arbitrary
% knowledge encoded as propositional
% logic sentences, your system infer
% new pieces of knowledge encoded as
% propositional logic.

% First you should
% implement conversion of propositional
% logic sentences to conjunctive normal
% form.	 This should work in the form of
% a predicate 'cnf' as in the example
% below:
%
% ?- cnf(((a ^ b) => c) ^ d, X).
% Implication elimination
% (a^b) => (c) -----> ~(a^b) v (c)
% De Morgan
% ~((a) ^ (b)) -----> (~(a) v ~(b))
% X = [[ (~a), (~b), c], [d]].
%
% As part of the conversion, your predicate
% should print out what logical equivalences
% are being applied.  The output above
% corresponds to
% (( a AND b ) => c ) AND d
% ( NOT( a AND b ) OR c ) AND d
% (( NOT(a) OR NOT(b) ) OR c ) AND d
% ( NOT(a) OR NOT(b) OR c ) AND d
%
% which we represent using lists, where
% each inner list is a disjunction, and
% the outer list is a conjunction (i.e. the
% inner commas are ORs and the outer commas
% are ANDs).  So the CNF sentence is
% [[ ~a, ~b, c ], [d]]
%
% These inner lists are CLAUSES.  A clause
% is a disjunction of literals.
%
% We represent the operators as follows:
% NOT: ~
% AND: ^
% OR: v
% IMPLICATION: =>
% BICONDITIONAL: <=>
%
% The necessary code for these operators
% is given to you, as discussed in class.
%
%
% You will need to implement (at least)
% the following predicates:
%
% - kbresolve/0: run resolution on the
% knowledge already in the knowledge base,
% and add the new (inferred) knowledge to the
% knowledge base (using kbadd/1, which is
% given to you).  Each piece of new knowledge
% is a clause (as defined above).
% This should also print out how many clauses
% are in the knowledge base.
%
% - kbentails/1: determine whether a
% propositional logic sentence is entailed by
% the knowledge base.  The argument is the
% propositional logic sentence.  This predicate
% should return true if the sentence is entailed,
% and false otherwise.
%
% - cnf/2: convert a propositional logic sentence
% to CNF, as discussed above.  This is where
% you will need to use the logic equivalences.
%
% The following predicates are given to you:
%
% - kbadd/1: add knowledge to the KB.  The
% single argument is a sentence in
% propositional logic.  Once a new sentence
% is added to the KB, resolution does not
% happen automatically; it needs to be
% invoked explicitly using the resolve/0
% predicate (see above).  In the implementation
% of kbadd, you will see that it uses the
% predicates nottautology/1 and notinkb/1.
% This is so that we can avoid adding to the KB
% knowledge that is not useful, either because
% it is already in the KB, or because it is
% trivially true (contains complementary
% literlas, e.g. a, ~a).  You should familiarize
% yourself with how these predicates work.
%
% - kbclear/0: clear the KB.
%
% - kbprint/0: print the contents of the
% knowledge base.  This uses the list format
% discussed above.


% Propositional logic operators
% with precedence

:- op(1000, fx, '~').
:- op(1010, yfx, '^').
:- op(1020, yfx, 'v').
:- op(1030, xfx, '=>').
:- op(1040, xfx, '<=>').

% First we need to convert propositional logic
% sentences to conjunctive normal form.  For this,
% we'll first define predicates that correspond
% to the logical equivalences we saw in class.

% Some of the equivalences are given here, but
% feel free to reimplement as needed.  Others you
% need to implement yourself.
%
% In the reference implementation, many of the
% equivalences are implemented in a recursive
% predicate equiv/2, as seen below.  You are not
% required to implement the equivalences this
% way, as long as you end up with a functioning
% and correct way to convert to CNF.

% Commutativity for AND and OR

commutativityand(A ^ B, B ^ A).

commutativityor(A v B, B v A).

% Distributivity for AND and OR

% Double negation

equiv(~(~A), X) :-
	format('Double negation~n'),
	% the format below just prints things out,
	% much like a printf.  You can use write and
	% writeln if you prefer.
	format('~~(~~~w) -----> ~w~n', [A, A]),

	% FILL IN HERE
	.

% Biconditional elimination

% Implication elimination

% (a^b) => (c)

equiv(A => B, X) :-
	format('Implication elimination~n'),
	format('(~w) => (~w) -----> ~~(~w) v (~w)~n', [A, B, A, B]),
	equiv(A, A1),
	equiv(B, B1),
	equiv(~A1 v B1, X),!.

% De Morgan's
% (hint: you may need two of these)


% You may want to do the conversion recursively,
% in which case you would probably want a base
% case like this:

equiv(A, A).


% Flatten AND (associativity)

% Depending on how things are implemented,
% you may end up with things like this:
% (((a ^ b) ^ c) ^ (d ^ e))
% But you may want this:
% (a ^ b ^ c ^ d ^ e)


% Flatten OR (associativity)


% The main predicate for converting to CNF.
% The converted sentence will be a list
% of conjuncts, and each element of this list
% is a list of disjuncts.

cnf(A, E) :-
	% FILL IN HERE
	yourcodegoeshere(A, B).


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

resolve(A, B, L) :-
        % FILL IN HERE
	.

kbresolve :-
	% gets a list of the clauses in the KB
	% (you may not need this; I used it)

	% print the number of clauses in the KB
	write(C), writeln(' clauses in KB.').

kbprint :-
	findall(P, (kb(P), writeln(P)), L),
	length(L, N),
	format('~d clauses~n', [N]).

kbentails(A) :-
	cnf(A, B),

	% FILL IN HERE
	.

:- kbclear.

