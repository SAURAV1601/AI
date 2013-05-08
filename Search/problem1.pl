isort([],[]).


isort(Input,Result):-
	insert_sort(Input,Result1),reverse(Result1,Result).


insert_sort([Head|Tail],Result1):-
	insert_sort(Tail,Temp),insert(Head,Temp,Result1).

insert_sort([],[]).

insert(Head,[THead|Temp],[THead|Result]):-
	Head<THead,insert(Head,Temp,Result),!.

insert(Head,Temp,[Head|Temp]).


