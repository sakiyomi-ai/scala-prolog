%% children
1/2::child(luke,vader).
0.5::child(leia,vader).

%child(luke,Who).
%child(X,vader).
%query(child(_,vader)).

%
%% Basic ProbLog: Probabilistic facts and clauses
0.5::heads1.
0.6::heads2.

% Rules:
twoHeads :- heads1, heads2.

% Queries:
%query(heads1).
%query(heads2).
%query(twoHeads).

%% Probabilistic clauses
0.6::heads(C) :- coin(C).

% Background information:
coin(c1).
coin(c2).
coin(c3).
coin(c4).

% Rules:
someHeads :- heads(_).

% Queries:
%query(someHeads).
