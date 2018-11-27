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

%Destino final para busca
final(portoalegre).

% Capitais adjacentes
adjacente(X,Y):-
    estrada(X,Y);
    estrada(Y,X).

% Retorna a lista de adjacentes a uma cidade. Exemplo:
% ?- listaadjacente(portoalegre,L).
% L = [florianopolis].
listaadjacente(X,L):-
    bagof(A,adjacente(X,A),L).

% Verifica se um elemento faz parte da lista. Exemplo:
% ?- pertence(curitiba, [saopaulo,curitiba,manaus]).
% true.
pertence(X,[X|_]):-!.
pertence(X,[_|Y]):-
    pertence(X,Y),!.

% Concatena (une) uma lista L1 com uma lista L2. Exemplo:
% ?- concatena([a,b,c],[d,e,f],L).
% L = [a, b, c, d, e, f].
concatena(L1,L2,R):-
    append(L1,L2,R).

% Inverte uma lista. Exemplo:
% inverte([a,b,c],R).
% R = [c, b, a].
inverte([],[]).
inverte([H|C],Resultado):-
    inverte(C,R2),
    append(R2,[H],Resultado).

% Predicado para ver o tamanho de uma lista. Se o tamanho for 0, para.
% Se houver elementos, ele conta na vari�vel T onde T ser� X + 1 para
% somar o tanto de elementos. Exempplo
% ?- tamanho([a,b,c],T).
% T = 2.
tamanho([_], 0):-!.
tamanho([_|Lista], Tamanho):-
    tamanho(Lista, X),
    Tamanho is X + 1.

% Predicado para busca em PROFUNDIDADE.
% Para consultar, primeiro alterar o destino final em final(N) e ent�o:
% ?- profundidade(curitiba,R).

profundidadeMax(No,Resultado,Max):-
    profundidade(No,[],Resultado,Max).
profundidadeMax(No,_,[No],_):-
    final(No).
profundidadeMax(No,Caminho,[No|Resultado],Max):-
    Max > 0,
    Max1 is Max - 1,
    not(pertence(No,Caminho)),
    adjacente(No,No1),
    profundidade(No1,[No|Caminho],Resultado,Max1).

profundidade(No,Resultado):-
    profundidade(No,[],Resultado).
profundidade(No,_,[No]):-
    final(No).
profundidade(No,Caminho,[No|Resultado]):-
    not(pertence(No,Caminho)),
    adjacente(No,No1),
    profundidade(No1,[No|Caminho],Resultado).


% Predicado para busca em LARGURA.
% Para consultar, primeiro alterar o destino final em final(N) e ent�o:
% ?- largura(portoalegre,R,L).
largura(No,Resultado,ListaInvertida) :-
		buscal([[No]],Resultado),
                inverte(Resultado,ListaInvertida).
buscal([[No|Caminho]|_],[No|Caminho]):-
		final(No).
buscal([[No|Caminho]|Caminho2],Resultado):-
		bagof([X,No|Caminho],(estrada(No,X),not(pertence(X,[No|Caminho]))),Caminho3),
		concatena(Caminho2, Caminho3, Caminhos1), !,
		buscal(Caminhos1, Resultado);
		buscal(Caminho2, Resultado).

% Predicado para a DISTANCIA.
%
% Retorna o tamanho da lista encontrada na profudundidade.
% Exemplo:
% ?- profundidade_distancia(curitiba,R,X).
% R = [curitiba, saopaulo, riodejaneiro, vitoria],
% X = 4 ;
% R = [curitiba, saopaulo, riodejaneiro, belohorizonte, vitoria],
% X = 5 ;
% R = [curitiba, saopaulo, belohorizonte, vitoria],
% X = 4 ;
profundidade_distancia(No,Resultado,X1):-
    profundidade(No,Resultado),
    length(Resultado,X),
    X1 is X-1.

distancia(Distancia,No,Lista):-
    profundidade_distancia(No,Resultado,X),
    X == Distancia,
    .

listavertice(L,T):-
    findall(A,listaadjacente(A,_),L),
    tamanho([_|L],T).

listaaresta(L):-
    findall(A,estrada(A,_),L).

qtdearesta(T):-
   listaaresta(L),
   tamanho([_|L],T).

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
    tamanho([X|L],T).

pares(T):-
    qtdeadjacentescidade(_,_,T),
    T mod 2 =:= 0.

impares(T):-
    qtdeadjacentescidade(_,_,T),
    T mod 2 =:= 1.

qtdepares(T,L):-
    findall(A,pares(A),L),
    tamanho([_|L],T).

qtdeimpares(T,L):-
    findall(A,impares(A),L),
    tamanho([_|L],T).



