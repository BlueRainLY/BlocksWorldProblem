%%%%%%%%% Two Room Prolog Planner %%%%%%%%%%%%%%%%%%%%%%%%%%
%%% UCF Fall 2017
%%% Two-Room planner
%%% by Nicolas Lopez and Alexandra Aguirre
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module( planner,
	   [
	       plan/4,change_state/3,conditions_met/2,member_state/2,
	       move/3,go/2,test/0,test2/0,test4/0
	   ]).

:- [utils].

plan(State, Goal, _, Moves) :-	equal_set(State, Goal),
				write('moves are'), nl,
				reverse_print_stack(Moves).
plan(State, Goal, Been_list, Moves) :-
				move(Name, Preconditions, Actions),
				conditions_met(Preconditions, State),
				change_state(State, Actions, Child_state),
				not(member_state(Child_state, Been_list)),
				stack(Child_state, Been_list, New_been_list),
				stack(Name, Moves, New_moves),
			plan(Child_state, Goal, New_been_list, New_moves),!.

change_state(S, [], S).
change_state(S, [add(P)|T], S_new) :-	change_state(S, T, S2),
					add_to_set(P, S2, S_new), !.
change_state(S, [del(P)|T], S_new) :-	change_state(S, T, S2),
					remove_from_set(P, S2, S_new), !.
conditions_met(P, S) :- subset(P, S).

member_state(S, [H|_]) :-	equal_set(S, H).
member_state(S, [_|T]) :-	member_state(S, T).

%%% move types for single rooms

move(pickup(X), [handlocation(Z), handempty, clear(X, Z), on(X, Y, Z)],
                [del(handempty), del(clear(X, Z)), del(on(X, Y, Z)),
				 add(clear(Y, Z)), add(holding(X, Z))]).

move(putdown(X), [holding(X, Y), handlocation(Y)],
                [del(holding(X, Y)), add(ontable(X, Y)), add(clear(X, Y)),
                 add(handempty)]).

move(stack(X, Y), [holding(X, Z), handlocation(Z), clear(Y, Z)],
	        [del(holding(X, Z)), del(clear(Y, Z)), add(handempty),
		 add(on(X, Y, Z)), add(clear(X, Z))]).

%%% move types for two rooms

move(pickup(X), [handlocation(Z), handempty, clear(X, Z), on(X, Y, Z)],
		[del(handempty), del(clear(X, Z)), del(on(X, Y, Z)),
				 add(clear(Y, Z)), add(holding(X, Z))]).

move(pickup(X), [handempty(Z), clear(X,Z), on(X, Y, Z)],
		[del(handempty(Z)), del(clear(X, Z)), del(on(X, Y, Z)),
		 add(clear(Y, Z)),  add(holding(X, Z))]).

move(pickup(X), [handlocation(Y), handempty, ontable(X, Y), clear(X, Y)],
		[del(handempty), del(clear(X, Y)), del(ontable(X, Y)),
		                    add(holding(X, Y))]).

move(putdown(X), [holding(X, Y), room(C), handin(C)],
		[del(room(C)), del(holding(X, Y)), add(ontable(X, C)),
		 add(clear(X, C)), add(handempty)]).

move(stack(X, Y), [holding(X, Z), room(C), handlocation(C), clear(Y, C)],
		[del(room(C)), del(holding(X, Z)), del(clear(Y, C)),
		 add(handempty), add(on(X, Y, C)), add(clear(X))]).

%%% moving the hand from room 2 to room 1

move(goroom1, [handlocation(2), room(2)],
             [del(handlocation(2)), add(handlocation(1)), add(room(1)), del(room(2))]).

move(goroom1, [handlocation(2)],
	     [del(handlocation(2)), add(handlocation(1)), add(room(1))]).

%%% moving the hand from room 1 to room 2

move(goroom, [handlocation(1), room(1)],
             [del(handlocation(1)), add(handlocation(2)), add(room(2)), del(room(1))]).

move(goroom, [handlocation(1)],
	     [del(handlocation(1)), add(handlocation(2)), add(room(2))]).

%%% Looking for any backtracks

move(putdown(X), [holding(X, Y), handlocation(Y)],
		 [del(holding(X, Y)), add(ontable(X, Y)), add(clear(X, Y)),
		                      add(handempty)]).

move(stack(X, Y), [holding(X, Z), handlocation(Z), clear(Y, Z)],
		  [del(holding(X, Z)), del(clear(Y, Z)), add(handempty),
		                       add(on(X, Y, Z)), add(clear(X, Z))]).

move(pickup(X), [room(C), handlocation(Z), handempty, clear(X, Z), on(X, Y, Z)],
                [del(room(C)), del(handempty), del(clear(X, Z)),
		 del(on(X, Y, Z)), add(clear(Y, Z)), add(holding(X, Z))]).

move(putdown(X), [room(C), holding(X, Y), handlocation(Y)],
		 [del(room(C)), del(holding(X, Y)), add(ontable(X, Y)),
		  add(clear(X, Y)), add(handempty)]).

move(stack(X, Y), [room(C), holding(X, Z), handlocation(Z), clear(Y, Z)],
		  [del(room(C)), del(holding(X, Z)), del(clear(Y, Z)),
		   add(handempty), add(on(X, Y, Z)), add(clear(X, Z))]).


/* run commands */

go(S, G) :- plan(S, G, [S], []).

test :- go([handlocation(1), handempty, ontable(b, 1), ontable(c, 1), on(a, b, 1), clear(c, 1), clear(a, 1)],
	   [handlocation(1), handempty, ontable(c, 1), on(a, b, 1), on(b, c, 1), clear(a, 1)]).

test2 :- go([handlocation(1), handempty, ontable(b, 1), ontable(c, 1), on(a, b, 1), clear(c, 1), clear(a, 1)],
	    [handempty, ontable(a, 1), ontable(b, 1), on(c, b, 1), clear(a, 1), clear(c, 1)]).

test4 :- go([handlocation(1), handempty, ontable(b, 1), on(a, b, 1), clear(a, 1)],
	   [handlocation(1), handempty, ontable(b, 2), on(a, b, 2), clear(a, 2)]).