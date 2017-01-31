% bubble sort with relational data structure .
maxTime(5).
fluents	location(_, _).
actions	swap(_,_,_,_).

initially	location(d, 1), location(c, 2), location(b, 3),  location(a,4).

if	location(X, N1) at T1, N2 is N1 +1,  location(Y, N2) at T1,  Y@<X
then	swapped(X, N1, Y, N2) from T2 to T3.

% swapped may not work if the order of the two clauses below is
% reversed. Perhaps for good reasons.

swapped(X, N1, Y, N2) from T1 to T2 if 	
	location(X, N1) at T1, location(Y, N2) at T1,  
	Y@<X, swap(X, N1, Y, N2) from T1 to T2.

swapped(X, N1, Y, N2) from T to T if
	location(X, N1) at T, location(Y, N2) at T, X@<Y.

swap(X, N1, Y, N2)  	initiates 	location(X, N2).
swap(X, N1, Y, N2)  	initiates 	location(Y, N1).

swap(X, N1, Y, N2)  	terminates 	location(X, N1).
swap(X, N1, Y, N2)  	terminates 	location(Y, N2).

false 	swap(X, N1, Y, N2), swap(Y, N2, Z, N3).

/** <examples>
?- go(Timeline).
*/