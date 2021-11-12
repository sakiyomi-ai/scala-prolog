# scala-prolog

A toy ProbLog interpreter, inspired by scala-prolog.
Thank you to Mr. Joe Halliwell.
His code was very interesting.

## Modifications
- added mod,log operator
- added comparison operator
- added probability to Atom and Predicate
- added query predicate
- Improved parsing methods
- Quarkus command-line Project
- Native Image compilable
- etc.

## Execution example
file: test.pl
```
%% children
1/2::child(luke,vader).
0.5::child(leia,vader).
  
%% Basic ProbLog: Probabilistic facts and clauses
0.5::heads1.
0.6::heads2.
  
% Rules:
twoHeads :- heads1, heads2.
  
%% Probabilistic clauses
0.6::heads(C) :- coin(C).
  
% Background information:
coin(c1).
coin(c2).
coin(c3).
coin(c4).
  
% Rules:
someHeads :- heads(_).
```

Execution result.
```
% ./target/scala-prolog-1.0.0-SNAPSHOT-runner 
__  ____  __  _____   ___  __ ____  ______ 
 --/ __ \/ / / / _ | / _ \/ //_/ / / / __/ 
 -/ /_/ / /_/ / __ |/ , _/ ,< / /_/ /\ \   
--\___\_\____/_/ |_/_/|_/_/|_|\____/___/   
2021-11-12 15:43:59,118 INFO  [io.quarkus] (main) scala-prolog 1.0.0-SNAPSHOT native (powered by Quarkus 2.4.1.Final) started in 0.011s. 
2021-11-12 15:43:59,251 INFO  [io.quarkus] (main) Profile prod activated. 
2021-11-12 15:43:59,251 INFO  [io.quarkus] (main) Installed features: [cdi, scala]
Simple Prolog Interpreter in Scala

Type \? for help
----------------------------------------------------
Loading initial.pl
? consult("test.pl").
consult("test.pl").
Loading test.pl
Yes.
? \l
\l
member(X_0, [X_0, |, T_0])
:-(member(X_0, [H_0, |, T_0]), member(X_0, T_0))
length([], 0.0)
:-(length([H_0, |, T_0], X_0), ,(length(T_0, Y_0), is(X_0, +(Y_0, 1.0))))
append([], X_0, X_0)
:-(append([H_0, |, T_0], X_0, [H_0, |, T2_0]), append(T_0, X_0, T2_0))
::(/(1.0, 2.0), child(luke, vader))
::(0.5, child(leia, vader))
::(0.5, heads1)
::(0.6, heads2)
:-(twoHeads, ,(heads1, heads2))
:-(::(0.6, heads(C_0)), coin(C_0))
coin(c1)
coin(c2)
coin(c3)
coin(c4)
:-(someHeads, heads(__0_0))
? child(luke,Who).
child(luke,Who).
Who=vader
P=0.5
More y/n? y
y
No.
? child(X,vader).
child(X,vader).
X=luke
P=0.5
More y/n? y
y
X=leia
P=0.5
More y/n? y
y
No.
? query(child(_,vader)).
query(child(_,vader)).
P=0.75
Yes.
? query(heads1).
query(heads1).
P=0.5
Yes.
? query(heads2).
query(heads2).
P=0.6
Yes.
? query(twoHeads).
query(twoHeads).
P=0.30
Yes.
? query(someHeads).
query(someHeads).
P=0.9744
Yes.
? \q
\q

2021-11-12 15:45:44,251 INFO  [io.quarkus] (Shutdown thread) scala-prolog stopped in 0.000s
```

## Quarkus for Packagin(Uber-Jar) & run
mvn clean package -Dquarkus.package.type=uber-jar
java -jar target/scala-prolog-1.0.0-SNAPSHOT-runner.jar 

## Quarkus for Native
$ mvn -Pnative clean package
$ target/scala-prolog-1.0.0-SNAPSHOT-runner


---
The following is the original README.md

# scala-prolog

Learning Scala by writing a toy Prolog interpreter.

## Compiling & running

This is an SBT project.

## TODO

- Tests
- More examples from http://www.cs.toronto.edu/~hojjat/384w09/simple-prolog-examples.html
- User-defined/infix operators cf http://www.swi-prolog.org/pldoc/man?predicate=op/3
- Some sort of calling convention between Prolog and Scala

- Fibonacci example: requires correct goal sequencing -- or a cut!
