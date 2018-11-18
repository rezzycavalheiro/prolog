% Implemente um grafo não direcionado que representa os caminhos
% existentes entre as capitais do Brasil. Considere que existe uma
% estrada ligando duas capitais quando os respectivos estados fazem
% fronteira. Utilize o mapa em anexo.

estrada(portoalegre,florianopolis).
estrada(florianopolis,curitiba).
estrada(curitiba,saopaulo).
estrada(curitiba,campogrande).
estrada(saopaulo,riodejaneiro).
estrada(saopaulo,belohorizonte).
estrada(saopaulo,campogrande).
estrada(riodejaneiro,vitoria).
estrada(riodejaneiro,belohorizonte).
estrada(belohorizonte,vitoria).
estrada(belohorizonte,goiania).
estrada(belohorizonte,salvador).
estrada(belohorizonte,brasilia).
estrada(campogrande,goiania).
estrada(campogrande,cuiaba).
estrada(goiania,palmas).
estrada(goiania,cuiaba).
estrada(goiania,brasilia).
estrada(goiania,salvador).
estrada(vitoria,salvador).
estrada(cuiaba,palmas).
estrada(cuiaba,portovelho).
estrada(cuiaba,belem).
estrada(cuiaba,manaus).
estrada(portovelho,riobranco).
estrada(portovelho,manaus).
estrada(riobranco,manaus).
estrada(manaus,boavista).
estrada(manaus,belem).
estrada(boavista,belem).
estrada(belem,macapa).
estrada(belem,saoluis).
estrada(belem,palmas).
estrada(palmas,saoluis).
estrada(palmas,teresina).
estrada(palmas,salvador).
estrada(saoluis,teresina).
estrada(teresina,fortaleza).
estrada(teresina,recife).
estrada(teresina,salvador).
estrada(fortaleza,natal).
estrada(fortaleza,joaopessoa).
estrada(fortaleza,recife).
estrada(natal,joaopessoa).
estrada(joaopessoa,recife).
estrada(recife,maceio).
estrada(recife,salvador).
estrada(maceio,aracaju).
estrada(aracaju,salvador).

adjacente(X,Y):-
    estrada(X,Y);
    estrada(Y,X).

listaadjacente(X,L):-
    bagof(A,adjacente(X,A),L).

% Implemente um predicado que percorre o grafo em PROFUNDIDADE e
% verifica se uma cidade X pode ser alcançada a partir de uma cidade
% Y retornando o caminho percorrido (nós visitados) em uma lista.

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


