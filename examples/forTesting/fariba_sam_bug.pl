
:- expects_dialect(lps).

% This illustrates the need for "backtracking" accross cycles

maxTime(10).
actions a(N), b(N), e.
fluents f(N).

initially f(1).

observe e from 2 to 3.

e initiates f(2).

m(X) from T1 to T2 if 
	f(X) at T1, a(X) from T1 to T2.

if true then 
	m(X) from T1 to T2, T3 is T2 + 1, b(X) from T3 to T4.

false b(1).
