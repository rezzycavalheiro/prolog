media(N,P,X):-
    X is (N + P)/2.

entre(X,Y,Z):-
    X > Y,
    X < Z.

iguais(X,X).

funcao(Y,X):-
    Y < 10,
    X = -1;
    Y >= 10,
    Y < 100,
    X = 0;
    Y >= 100,
    X = 1.

