% Group members:
%  * Alexander Nead-Work, 933190259
%
% Grading notes: 10pts total
%  * Part 1: 6pts (1pt each)
%  * Part 2: 4pts (3pts for cmd, 1pt for prog)


% Part 1. It's a bird-eat-bug world out there!

% A small database of animals. Each relation gives the animal's name,
% it's habitat, and its biological class.
animal(cranefly, trees, insects).
animal(duck, ponds, birds).
animal(minnow, ponds, fish).
animal(scrubjay, trees, birds).
animal(squirrel, trees, mammals).
animal(waterstrider, ponds, insects).
animal(woodpecker, trees, birds).

% A small database capturing what each animal eats. Note that most animals eat
% more than one kind of food, but craneflies don't eat anything after they
% reach adulthood!
diet(scrubjay, insects).
diet(scrubjay, seeds).
diet(squirrel, nuts).
diet(squirrel, seeds).
diet(duck, algae).
diet(duck, fish).
diet(duck, insects).
diet(minnow, algae).
diet(minnow, insects).
diet(waterstrider, insects).
diet(woodpecker, insects).

% A binary predicate that includes all of the animals and where they live.
habitat(Animal, Where) :- animal(Animal, Where, _).

% A binary predicate that includes each animal and its biological class.
class(Animal, Class) :- animal(Animal, _, Class).


% 1. Define a predicate neighbor/2 that determines whether two animals live
%    in the same habitat. Note that two animals of the same kind always
%    live in the same habitat.
neighbor(L,R) :- animal(L, X, _), animal(R, X, _).

% 2. Define a predicate related/2 that includes all pairs of animals that
%    are in the same biological class but are not the same kind of animal.
related(L,R) :- animal(L, _, X), animal(R, _, X), L \= R.

% 3. Define a predicate competitor/3 that includes two kinds of animals and
%    the food they compete for. Two animals are competitors if they live in
%    the same place and eat the same food.
competitor(A1, A2, F) :- animal(A1, H, _), animal(A2, H, _), diet(A1, F), diet(A2, F).

% 4. Define a predicate would_eat/2 that includes all pairs of animals where
%    the first animal would eat the second animal (because the second animal
%    is a kind of food it eats), if it could.
would_eat(A1, A2) :- animal(A2, _, F), diet(A1, F).

% 5. Define a predicate does_eat/2 that includes all pairs of animals where
%    the first animal would eat the second, and both animals live in the same
%    place, so it probably does.
does_eat(A1, A2) :- animal(A1, H, _), animal(A2, H, F), diet(A1, F).

% 6. Define a predicate cannibal/1 that includes all animals that might eat
%    their own kind--eek!
cannibal(A) :- animal(A, _, F), diet(A, F).


% Part 2. Implementing a stack language

% A slightly larger example program to use in testing.
example(P) :-
  P = [ 2, 3, 4, lte,            % [tru, 2]
        if([5, 6, add], [fls]),  % [11, 2]
        3, swap,                 % [11, 3, 2]
        4, 5, add,               % [9, 11, 3, 2]
        lte, if([tru], [mul]),   % [6]
        "whew!", swap,           % [6, "whew!"]
        "the answer is" ].       % ["the answer is", 6, "whew!"]

% 1. Define the predicate `cmd/3`.
cmd(C, S1, S2)             :- number(C), S2 = [C|S1].
cmd(C, S1, S2)             :- string(C), S2 = [C|S1].
cmd(tru, S1, S2)           :- S2 = [tru|S1].
cmd(fls, S1, S2)           :- S2 = [fls|S1].
cmd(dup, [H|T], S2)        :- S2 = [H,H|T].
cmd(swap, [A,B|T], S2)     :- S2 = [B,A|T].
cmd(add, [A,B|T], [H|T])   :- H is A+B.
cmd(mul, [A,B|T], [H|T])   :- H is A*B.
cmd(lte, [A,B|T], S2)      :- ( A >= B -> S2 = [tru|T] ; S2 = [fls|T] ).
cmd(if(L,_), [tru|T], S2)  :- prog(L, T, S2).
cmd(if(_,R), [fls|T], S2)  :- prog(R, T, S2).

% 2. Define the predicate `prog/3`.
prog([], S1, S2)    :- S2 = S1.
prog([H|T], S1, S2) :- cmd(H, S1, Ret), prog(T, Ret, S2).
