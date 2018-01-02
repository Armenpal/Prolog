/*
CPS 721 Assignment 5 - Question 2

Group Members' Name       Student Number
Deborah Mepaiyeda         500713214
Armen Palvetzian          500696282
Malcolm Gomes             500680348
*/

/* CPS 721 - Question 2: NASA domain: taking images from satellites */

/* The following is necessary if rules with the same predicate in the head are not
 consecutive in your program. Read handout about Eclipse Prolog 6 for details.*/
:- dynamic pointsTo/3.

  /* Universal situations and fluents based planner  */

solve_problem(L,N)  :-  C0 is cputime,
                   max_length(L,N),
                   reachable(S,L), goal_state(S),
                   Cf is cputime, D is Cf - C0, nl,
                   write('Elapsed time (sec): '), write(D), nl.

reachable(S,[]) :- initial_state(S).
reachable(S2, [M | History]) :- reachable(S1,History),
                        legal_move(S2,M,S1).

/*
reachable(S2, [M | History]) :- reachable(S1,History),
                        legal_move(S2,M,S1),
                        not useless(M,History).
*/
legal_move([A | S], A, S) :- poss(A,S).

initial_state([]).

max_length([],N).
max_length([_|L],N1) :- N1 > 0, N is N1 - 1, max_length(L,N).

:- [ 'initNASA' ].
/* This is to compile the file initNASA.pl  before you
   run a query. Do NOT insert this file here because your program
   will be tested using different initial and goal states.      */

    /* Precondition Axioms */
  poss(up(Ins, Sat), S):-  not powered(Ins,Sat,S), not ((powered(Ins2, Sat, S), not Ins2 = Ins)).

  poss(down(Ins, Sat), S) :-  powered(Ins, Sat, S).

  poss(turnTo(Sat,Dir1,Dir2), S) :- pointsTo(Sat, Dir1, S),available(Sat, Dir1), available(Sat, Dir2).

  poss(runCalibrateProc(Ins, Sat, G), S):- pointsTo(Sat, G, S), target(Ins, G), powered(Ins, Sat, S).

  poss(takeImage(Ins, Sat, M, Dir), S):- powered(Ins, Sat, S), calibrated(Ins, Sat, S), pointsTo(Sat, Dir, S), supports(Ins, Sat,M).

    /* Successor state axioms */
  powered(Ins, Sat, [up(Ins, Sat)|S]).
  powered(Ins, Sat, [A|S]) :- powered(Ins, Sat, S), not (A = down(Ins, Sat)).

  pointsTo(Sat, Dir, [turnTo(Sat, D, Dir)|S]).
  pointsTo(Sat, Dir, [A|S]) :- pointsTo(Sat, Dir, S), not(A=turnTo(Sat, Dir, Dir2)).

  calibrated(Ins, Sat, [runCalibrateProc(Ins, Sat, G)|S]).
  calibrated(Ins, Sat, [A|S]):- calibrated(Ins, Sat, S), not(A = down(Ins, Sat)).

  hasImage(Sat, M, Dir, [takeImage(Ins, Sat, M, Dir)|S]).
  hasImage(Sat, M, Dir, [A|S]):- hasImage(Sat, M, Dir, S), not (A = down(Ins, Sat)).
