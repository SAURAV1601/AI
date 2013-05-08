
% Question 2

use_module(library(heaps)).


city(shanghai, 10, 2).
city(beijing, 9, 5).
city(hongkong, 8, 0).
city(wuhan, 8, 2).
city(chengdu, 6, 2).
city(lahsa, 3, 2).
city(urumqi, 2, 6).
flight(beijing, shanghai).
flight(beijing, wuhan).
flight(beijing, urumqi).
flight(shanghai, hongkong).
flight(hongkong, wuhan).
flight(hongkong, chengdu).
flight(chengdu, wuhan).
flight(chengdu, lahsa).
flight(urumqi, lahsa).

adjacent(X,Y) :- flight(X,Y); flight(Y,X).

transition(X,Y) :- adjacent(X,Y).



			% Depth First Search	

depthfirst(Source,Source,Visited,0,[Source|Visited]).

depthfirst(Source,Target,Visited,Dist,Path):- 
	transition(Source,X), 
	not(member(X,Visited)),
	depthfirst(X,Target,[Source|Visited],Dist1,Path),
	calculate_distance(Source,X,Dist2),
	Dist is Dist1+Dist2.

dfs(Source,Target,Path,Dist) :- 
	findall(Path1,depthfirst(Source,Target,[],_,Path1),Pathlist),
	findall(Dist2,depthfirst(Source,Target,[],Dist2,_),Distlist),
	findmin(Pathlist,Distlist,10000,[],Dist,Path3),
	reverse(Path3,Path).


findmin([PHead|PTail],[DHead|DTail],Currentmin,Currentminpath,Finalmin,FinalPath):- 
	(DHead < Currentmin -> 
		Newmin = DHead, Newminpath = PHead 
	;
		Newmin=Currentmin, Newminpath = Currentminpath
	),
	findmin(PTail,DTail,Newmin,Newminpath,Finalmin,FinalPath).
  			

findmin([],[],Currentmin,Currentminpath,Currentmin,Currentminpath).  			

calculate_distance(S,T,Dist1):-
	city(S,Sx,Sy),city(T,Tx,Ty),Dist1 is sqrt((Sx-Tx)**2 + (Sy-Ty)**2).



			% Breadth First Search
			


bfs( Start,Target,Solution,Distance) :-

	breadthfirst( [([Start],0)],Target,Solution,Distance).


breadthfirst([([Node|Rpath],Distance)|_],Target,[Node|Rpath],Distance) :-
	goal(Node,Target).

breadthfirst( [(Path,Dist2)| Paths],Target,Solution,Distance):-
	extend( Path,Dist2,Newpaths),
	append( Paths, Newpaths, Paths1),
	breadthfirst( Paths1,Target,Solution,Distance).


extend([Node|Path],Distance,Newpaths) :-
	findall( ([NewNode, Node | Path],Dist),
	( transition(Node, NewNode), not(member(NewNode, [Node | Path])),calculate_distance(Node,NewNode,Dist1),
	Dist is Distance+Dist1),Newpaths).

goal(Node,Node).


			%   Dijkstra's Algorithm


dijkstra(Start,Target,Solution) :-

	TL0 = [0-[Start]],list_to_heap(TL0,Heap0),
	dijkstras_body(Heap0,Target,Solution).


dijkstras_body(Heap0,Target,Solution) :- 
	min_of_heap(Heap0,Priority,[KHead|KTail]),
	KHead==Target,
	reverse([KHead|KTail],Path),
	Solution=(Path,Priority),!.

dijkstras_body(Heap0,Target,Solution):-
	get_from_heap(Heap0, Priority, Key, Heap1),
	extend_dijkstra(Key,Priority,Newpaths),
	add_path(Newpaths,Heap1,Heap2),
	dijkstras_body(Heap2,Target,Solution).	

extend_dijkstra([KHead|KTail],Priority,Newpaths) :-	
	findall((Dist,[NewNode,KHead|KTail]),(transition(KHead, NewNode),not(member(NewNode,[KHead|KTail])),
	calculate_distance(KHead,NewNode,Dist1),Dist is Priority+Dist1),Newpaths).

add_path([],Heap1,Heap1).

add_path([(Priority,Key)|Tail],Heap1,Heap2):-
	add_to_heap(Heap1,Priority,Key,TempHeap),
	add_path(Tail,TempHeap,Heap2).



			%     A* Algorithm



a_star(Start,Target,Solution) :-
	calculate_distance(Start,Target,Heuristic_dist),
	TL0 = [Heuristic_dist-[Start]],
	list_to_heap(TL0,Heap0),
	astar_imp(Heap0,Target,Solution).

astar_imp(Heap0,Target,Solution) :- 
	min_of_heap(Heap0,Priority,[KHead|KTail]),
	KHead==Target,
	reverse([KHead|KTail],Path),
	Solution=(Path,Priority),!.

astar_imp(Heap0,Target,Solution):-	
	get_from_heap(Heap0, Priority, [KHead|KTail], Heap1),
	calculate_distance(KHead,Target,Heuristic_dist),
	Priority1 is Priority-Heuristic_dist,
	extend_astar([KHead|KTail],Priority1,Target,Newpaths),
	add_path(Newpaths,Heap1,Heap2),
	astar_imp(Heap2,Target,Solution).	


extend_astar([KHead|KTail],Priority,Target,Newpaths) :-
	findall((Dist,[NewNode,KHead|KTail]),(transition(KHead, NewNode),not(member(NewNode,[KHead|KTail])),
	calculate_distance(KHead,NewNode,Dist1),calculate_distance(Target,NewNode,Heuristic_dist),Dist is Priority
	+Dist1+Heuristic_dist),Newpaths).




				





				



