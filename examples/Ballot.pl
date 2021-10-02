
:- expects_dialect(lps).

% By BK, modified by MC: 
% single ballot per program instance, added pre conditions and method names as in Solidity original
% http://solidity.readthedocs.io/en/v0.4.24/solidity-by-example.html
maxTime(15).

events 	
	ballot(_Chairman, _Proposals), giveRightToVote(_Chairman, _Voter), 
	delegate(_FromVoter, _ToVoter), vote(_Voter, _Candidate).

fluents	chairman(_Chairman), voter(_Voter, _Weight), voted(_Voter, _Candidate), 
	delegateOf(_Voter,_D), voteCount(_Candidate, _Votes).

observe ballot(chair, [trump, clinton]) from 1 to 2.
observe giveRightToVote(chair, miguel), 
	giveRightToVote(chair, fariba),
	giveRightToVote(chair, bob), giveRightToVote(chair, jacinto)  from 3 to 4.
observe delegate(bob, miguel)  from 4 to 5.
observe vote(miguel, clinton)  from 5 to 6.
observe delegate(jacinto,bob) from 6 to 7.
observe delegate(fariba, miguel)  from 7 to 8.

ballot(_Chairman, Proposals) initiates voteCount(Candidate, 0) if 
	member(Candidate, Proposals).
ballot(Chairman, _Proposals) initiates voter(Chairman,1).
ballot(Chairman, _Proposals) initiates chairman(Chairman).

% the ballot is new:
false ballot(_,_), voteCount(_,_).

giveRightToVote(Chairman, Voter) initiates voter(Voter, 1) if chairman(Chairman), not voter(Voter,_).
	
delegate(Voter1, _Voter2) updates Old to 0 in voter(Voter1, Old).
delegate(Voter1, Voter2) updates Old to New in voter(Voter3, Old) if
		delegateOf(Voter2,Voter3), voter(Voter1, N1), New is N1 + Old. 
% The next clause deals with the case a delegate has already voted when a delegation is made:
delegate(Voter1, Voter2) updates OldVotes to NewVotes in voteCount(Candidate, OldVotes) if
		delegateOf(Voter2,Voter3), voted(Voter3, Candidate),  voter(Voter1, N), NewVotes is OldVotes + N.
delegate(Voter1,Voter2) initiates voted(Voter1,delegated(Voter2)).

% deal with delegate chains
delegateOf(Voter,D) if voted(Voter,delegated(DD)), delegateOf(DD,D).
delegateOf(Voter,Voter) if not voted(Voter,delegated(_)).

/* This allows only one delegation to Voter2 at a time. 
 * We may need to extend LPS to have several delegations simultaneously. */
false delegate(Voter1, Voter), delegate(Voter2, Voter), Voter1 \= Voter2.
/* This allows only one weighted vote for the same candidate at a time. 
 * We may need to extend LPS to have several votes for the same candidate simultaneously.*/
false delegate(Voter,_), voted(Voter,_).
false delegate(Voter1,Voter2), Voter1==Voter2.
false delegate(Voter, Voter1), delegate(Voter, Voter2), Voter1 \= Voter2.
% prevent delegation loops:
false delegate(Voter1,Voter2), delegateOf(Voter2,Voter1).

vote(Voter, Candidate) initiates voted(Voter, Candidate).

/* This allows only one weighted vote for the same candidate at a time. 
 * We need to extend LPS to have several votes for the same candidate simultaneously. */
vote(Voter, Candidate) updates OldVotes to NewVotes in voteCount(Candidate, OldVotes)	if
		voter(Voter, N), 
		NewVotes is OldVotes + N.

false vote(Voter1, Candidate), vote(Voter2, Candidate), Voter1 \= Voter2.
false vote(Voter,_), voted(Voter,_).
false vote(Voter, Candidate1), vote(Voter, Candidate2), Candidate1 \= Candidate2.

winningProposal(Candidate,N) at T if 
	findall(N-Candidate, voteCount(Candidate,N), L) at T,
	sort(L,SL), append(_,[N-Candidate],SL).

if winningProposal(_Candidate,4) then lps_terminate from _.

/** <examples>
?- go(Timeline).
*/