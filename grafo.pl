%Grafo

aresta(0,1).
aresta(1,2).
aresta(2,3).
aresta(3,4).
aresta(4,5).
aresta(5,6).
aresta(0,2).
aresta(6,1).
aresta(6,2).

adjacente(X,Y):-
    aresta(X,Y);
    aresta(Y,X).

listaadjacente(X,L):-
    bagof(A,adjacente(X,A),L).

tamL([_], 0):- !.
tamL([_|L], T):-
    tamL(L, X), T is X + 1.

listavertice(L,T):-
    findall(A,listaadjacente(A,_),L),
    tamL([_|L],T).

listaaresta(L):-
    findall(A,aresta(A,_),L).

qtdearesta(T):-
   listaaresta(L),
   tamL([_|L],T).

qtdeadjacentes(L,T):-
    listaadjacente(_,L),
    tamL([_|L],T).

pares(T):-
    qtdeadjacentes(_,T),
    T mod 2 =:= 0.

impares(T):-
    qtdeadjacentes(_,T),
    T mod 2 =:= 1.

qtdepares(T,L):-
    findall(A,pares(A),L),
    tamL([_|L],T).

qtdeimpares(T,L):-
    findall(A,impares(A),L),
    tamL([_|L],T).

