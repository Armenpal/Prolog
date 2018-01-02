% This file includes two testing configurations. The first combination
% of an initial and a goal state is simple and have to be used
% to test your precondition and successor state axioms from Part 2
% of the assignment. If you would like to work on the bonus Part 3,
% then comment out the top portion of this file and remove comments
% from the bottom part. Use the bottom part to test your rules for
% the predicate  useless(A,ListOfActions).

  % comment out this initial and goal states after testing Part 2

	%%  Initial situation and a domain specification %%
supports(instr1,sat1,therm).
available(sat1,ground17).
available(sat1,comet2).
available(sat1,orionStars).
target(instr1,ground17).

pointsTo(sat1,orionStars,[]).

		%% Goal State %%
goal_state(S) :- hasImage(sat1,therm,comet2,S).

  % comment this out after testing successfully your rules in Part 2


/*  %% remove this comment only if you work on Part 3 (bonus question)

	%% Initial situation and a domain specification %%
supports(tool1,sat1,therm).
supports(tool2,sat1,spectr).
available(sat1,andromeda).
available(sat1,ground17).
available(sat1,comet2).
available(sat1,orion).
target(tool1,ground17).
target(tool2,ground17).

pointsTo(sat1,orionStars,[]).

		%%  Goal state  %%
goal_state(S) :- hasImage(sat1,therm,comet2,S), 
                 hasImage(sat1,spectr,andromeda,S).

*/  %% remove this comment to test your rules for useless(A,ListOfActions)
