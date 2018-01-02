	/* % cps721, 5th assignment, Part 1: file  initTire.pl   %*/
			/* Initial state */
nut(nuts1).	nut(nuts2).	nut(nuts3).	nut(nuts4).
hub(hub1).	hub(hub2).	hub(hub3).	hub(hub4).
wheel(wheel1).	wheel(wheel2).	wheel(wheel3).	wheel(wheel4).	wheel(wheel5).

is_container(trunk).    is_container(car).

inside(jack,trunk,[]).   inside(wheel5,trunk,[]).  inside(wrench,trunk,[]).

on(wheel1,hub1,[]).   on(wheel2,hub2,[]).    on(wheel3,hub3,[]).    on(wheel4,hub4,[]).
on(wheel1,ground,[]). on(wheel2,ground,[]).  on(wheel3,ground,[]).  on(wheel4,ground,[]).
on(hub1,ground,[]).   on(hub2,ground,[]).    on(hub3,ground,[]).    on(hub4,ground,[]).
on(nuts1,hub1,[]).    on(nuts2,hub2,[]).     on(nuts3,hub3,[]).     on(nuts4,hub4,[]).

tight(nuts1,hub1,[]). tight(nuts2,hub2,[]).  tight(nuts3,hub3,[]).  tight(nuts4,hub4,[]).
fastened(hub1,[]).    fastened(hub2,[]).  fastened(hub3,[]).  fastened(hub4,[]).

inflated(wheel5,[]).  inflated(wheel1,[]). inflated(wheel2,[]).  inflated(wheel3,[]).

isClosed(trunk,[]).  

	/* Goal state: all goal states are enumerated so that you can solve a planning
	   problem for each goal state G using an argument G in solve_problem(G,L,N)  */

goal_state(0,S) :- wheel(W), on(W,Hub,S), not inflated(W,S), not isClosed(trunk,S). /*% Works %*/

goal_state(1,S) :- have(jack,S), have(wrench,S). /*& Works &*/

goal_state(2,S) :- nut(Nuts), hub(Hub), on(Nuts,Hub,S), not tight(Nuts,Hub,S), 
	wheel(W), on(W,Hub,S), not inflated(W,S). /*& Works &*/

goal_state(3,S) :- nut(Nuts), hub(Hub), on(Nuts,Hub,S), not tight(Nuts,Hub,S), have(jack,S). /* Works */

goal_state(4,S) :- nut(Nuts), hub(Hub), on(Nuts,Hub,S), not tight(Nuts,Hub,S), lifted(Hub,S). /* Works */

goal_state(5,S) :- have(wheel4,S).     % needs 7 actions to solve % /* Works */

goal_state(6,S) :- on(wheel5,hub4,S).  % needs 9 actions to solve % /* Works */

goal_state(7,S) :- on(wheel5,hub4,S),  inside(wheel4,trunk,S). % needs 10 actions %

goal_state(8,S) :- on(wheel5,hub4,S), fastened(hub4,S).  % needs 10 actions %

goal_state(9,S) :- on(wheel5,hub4,S), nut(Nuts), on(Nuts,hub4,S), tight(Nuts,hub4,S). %12 actions%

goal_state(10,S) :- on(wheel5,hub4,S), inside(jack,trunk,S), inside(wheel4,trunk,S). %12 actions%

goal_state(11,S) :- on(wheel5,hub4,S), fastened(hub4,S), inside(wheel4,trunk,S). %11 actions%

goal_state(12,S) :- on(wheel5,hub4,S), fastened(hub4,S),  tight(nuts4,hub4,S), 
inside(jack,trunk,S), inside(wheel4,trunk,S).  %14 actions %%

goal_state(13, S) :- on(wheel5,hub4,S), fastened(hub4,S), tight(nuts4,hub4,S),
inside(jack,trunk,S), inside(wheel4,trunk,S), isClosed(trunk,S).
%% This needs a plan consisting of 15 actions %%

