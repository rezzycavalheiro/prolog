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

% Retorna a lista de adjacentes a uma cidade. Exemplo:
% ?- listaadjacente(portoalegre,L).
% L = [florianopolis].
listaadjacente(X,L):-
    bagof(A,adjacente(X,A),L).

%profundidade(X,Y,L):-


% Predicado para ver o tamanho de uma lista. Se o tamanho for 0, para.
% Se houver elementos, ele conta na vari�vel T onde T ser� X + 1 para
% somar o tanto de elementos.
tamL([_], 0):- !.
tamL([_|L], T):-
    tamL(L, X), T is X + 1.


listavertice(L,T):-
    findall(A,listaadjacente(A,_),L),
    tamL([_|L],T).

listaaresta(L):-
    findall(A,estrada(A,_),L).

qtdearesta(T):-
   listaaresta(L),
   tamL([_|L],T).


%qtdeadjacentes(L,T):-
%    listaadjacente(_,L),
%    tamL([_|L],T).

% Quantidade de adjacentes a uma cidade. Retorna a lista e o tamanho
% dessa lista. Exemplo:
% ?- qtdeadjacentescidade(riobranco,L,T).
% L = [manaus, portovelho],
% T = 2.
qtdeadjacentescidade(X,L,T):-
    listaadjacente(X,L),
    tamL([X|L],T).

pares(T):-
    qtdeadjacentescidade(_,_,T),
    T mod 2 =:= 0.

impares(T):-
    qtdeadjacentescidade(_,_,T),
    T mod 2 =:= 1.

qtdepares(T,L):-
    findall(A,pares(A),L),
    tamL([_|L],T).

qtdeimpares(T,L):-
    findall(A,impares(A),L),
    tamL([_|L],T).


