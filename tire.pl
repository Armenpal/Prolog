	/* cps721, assignment 5, Part 1: how to replace a flat tire */
        
/* The following rules are necessary if rules with the same predicate in the head 
   are not consecutive in your program. Read Handout "How to use Eclipse Prolog 6"
   in labs for details.    */ 
%% :- dynamic inside/3, on/3, tight/3, isClosed/2, fastened/2, inflated/2.
:- discontiguous(inside/3).
:- discontiguous(on/3).
:- discontiguous(tight/3).
:- discontiguous(isClosed/2).
:- discontiguous(fastened/2).
:- discontiguous(inflated/2).

  /* Universal situations and fluents based planner  */

solve_problem(G,L,N)  :-  C0 is cputime,
                   max_length(L,N), 
                   reachable(S,L), goal_state(G,S),
                   Cf is cputime, D is Cf - C0, nl,
                   write('Elapsed time (sec): '), write(D), nl.

reachable(S,[]) :- initial_state(S).


reachable(S2, [M | History]) :- reachable(S1,History),
                        legal_move(S2,M,S1),
                        not useless(M,History).



legal_move([A | S], A, S) :- poss(A,S).

initial_state([]).

max_length([],N).
max_length([_|L],N1) :- N1 > 0, N is N1 - 1, max_length(L,N).

  %% Initial situation and a domain specification %%
:- [ 'initTire' ].
/* This is to compile the file initTire.pl  before you
   run a query. Do NOT insert this file here because your program 
   might be tested using different initial and goal states.      */

    /* Precondition Axioms */

poss(open(C), S) :- is_container(C), isClosed(C, S).

poss(close(C), S) :-  is_container(C), not isClosed(C, S).

poss(fetch(X, Y), S) :- is_container(Y), inside(X, Y, S), not isClosed(Y, S).
poss(putAway(X, Y), S) :-  is_container(Y), have(X, S), not isClosed(Y, S).

poss(loosen(X, Y), S) :- nut(X), hub(Y), have(wrench, S), on(X, Y, S), tight(X, Y, S), on(Y, ground, S).
poss(tighten(X, Y), S) :- nut(X), hub(Y), have(wrench, S), on(X, Y, S), on(Y, ground, S), not tight(X, Y, S).

poss(jackUp(Object), S) :- have(jack, S), hub(Object), on(W, Object, S), not lifted(Object, S).
poss(jackDown(Object), S) :- have(jack, S), hub(Object), on(W, Object, S), lifted(Object, S), not on(Object, ground, S).

poss(remove(Nuts, Hub), S) :- nut(Nuts), hub(Hub), lifted(Hub, S), have(wrench, S), fastened(Hub, S), not tight(Nuts, Hub, S).
poss(remove(W, H), S) :- wheel(W), hub(H), on(W, H, S), lifted(H, S), not fastened(H, S).

poss(putOn(N, H), S) :- nut(N), hub(H), have(N, S), have(wrench, S), lifted(H, S), not fastened(H, S). 
poss(putOn(W, H), S) :- wheel(W), hub(H), have(W, S), lifted(H, S), free(H, S), not on(W, H, S), not fastened(H, S).

    /* Successor State Axioms */

inside(Object, Container, [putAway(Object,Container) | S]) :- is_container(Container).
inside(Object, Container, [A | S]) :- inside( Object, Container, S ),
    not A=fetch(Object,Container).

inflated(W, [A|S]) :- wheel(W), inflated(W, S).

isClosed(C, [close(C)|S]) :- is_container(C).
isClosed(C, [A|S]) :- is_container(C), isClosed(C, S), not (A = open(C)).


have(X, [fetch(X, Y)|S]).
have(X, [remove(X, Y)|S]).
have(jack, [jackDown(X)|S]).
have(X, [A|S]) :- have(X, S), not(A=putAway(X,C)), not(A=putOn(X,Y)), not((X = jack, A= jackUp(Object))).


tight(N, H, [tighten(N, H)|S]).
tight(N, H, [A|S]) :- tight(N, H, S), not (A=loosen(N, H)).

lifted(X, [jackUp(X)|S]) :- hub(X).
lifted(X, [A|S]) :- hub(X), lifted(X,S), not (A=jackDown(X)).


on(X, Y, [putOn(X,Y)|S]).
on(X, ground, [jackDown(X)|S]) :- hub(X).
on(Y, ground, [jackDown(X)|S]) :- hub(X), on(Y, X, S).
on(X, Y, [A|S]) :- on(X, Y, S), not(A=remove(X,Y)), not(A=jackUp(X)).

fastened(H, [putOn(N, H)|S]) :- nut(N), hub(H).
fastened(H, [A|S]) :- hub(H), fastened(H, S), not (A=remove(Nuts, H)).

free(H, [remove(W, H)|S]) :- hub(H), wheel(W).
free(H, [A|S]) :- hub(H), wheel(W), free(H, S), not(A=putOn(W, H)).


    /* Declarative  Heuristics */

useless(open(C), [close(C)|S]).
useless(open(C), [A|S]) :- useless(open(C), S).

useless(close(C), [open(C)|S]).

useless(jackUp(Object), [jackDown(Object)|S]).
useless(jackUp(Object), [A|S]) :- useless(jackUp(Object), S).

useless(jackDown(Object), [jackUp(Object)|S]).

useless(remove(X, Y), [putOn(X, Y)|S]). 
useless(remove(X, Y), [A|S]) :- useless(remove(X, Y), S).

useless(putOn(X,Y), [remove(X,Y)|S]).

useless(tighten(X, Y), [loosen(X, Y)|S]).

useless(loosen(X, Y), [tighten(X, Y)|S]).
useless(loosen(X, Y), [A|S]) :- useless(loosen(X, Y), S).

useless(fetch(X, Y), [putAway(X, Y)|S]).
useless(fetch(X, Y), [A|S]) :- useless(fetch(X, Y), S).

useless(putAway(X, Y), [fetch(X, Y)|S]).

/* the ones above this are now fine you dont need to change them  BUT
YOU NEED TO ADD MORE LOGIC ONES BELOW ive added a few for you  */

useless(putAway(X,Y),S) :- nut(X).

useless(putAway(X,Y),S) :- nut(X), hub(Y), not(tight(Nuts,Hub,S)).

useless(jackDown(Object),S) :-  not((wheel(Wheel), on(Wheel,Object,S))).

useless(putOn(X,Y),S) :- nut(X), not(on(Wheel,Y,S)), wheel(Wheel).

useless(jackUp(Object), S) :- not((wheel(Wheel), on(Wheel, Object, S))).

