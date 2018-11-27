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
final(curitiba).

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

% Predicado para ver o tamanho de uma lista. Se o tamanho for 0, para.
% Se houver elementos, ele conta na vari�vel T onde T ser� X + 1 para
% somar o tanto de elementos.
tamanho([_], 0):- !.
tamanho([_|L], T):-
    tamanho(L, X), T is X + 1.

%profundidade(N,[N],_) :-
%    final(N).
%profundidade(N,[N|Caminho],Max) :-
%    Max > 0,
%    estrada(N,N1),               % fa�a um movimento v�lido
%    Max1 is Max - 1,
%    profundidade(N1,Caminho,Max1).    % recursividade

% Predicado para busca em PROFUNDIDADE.
profundidade(No,Resultado):-
    profundidade(No,[],Resultado).
profundidade(No,_,[No]):-
    final(No).
profundidade(No,Caminho,[No|Resultado]):-
    not(pertence(No,Caminho)),
    estrada(No,No1),
    profundidade(No1,[No|Caminho],Resultado).

% Predicado para busca em LARGURA.
largura([No|_],L,[No]):-
    pertence(No,L),!.
largura([No|Lista],N,[No|L]):-
    listaadjacente(Lista,L1),
    concatena(Lista,L1,Nos),
    largura(Nos,N,L).

%TESTE TESTE TESTE

resolve_largura(Inicio, Solucao) :-
		busca_largura([[Inicio]],Solucao).
busca_largura([[N|Caminho]|_],[N|Caminho]):-
		final(N).
busca_largura([[N|Caminho]|Caminhos],Solucao):-
		bagof([M,N|Caminho],
			(estrada(N,M),not(pertence(M,[N|Caminho]))),
			NovosCaminhos),
		concatena(Caminhos, NovosCaminhos, Caminhos1), !,
		busca_largura(Caminhos1, Solucao);
		busca_largura(Caminhos, Solucao).


%TESTE TESTE TESTE

%Usar append para colocar os itens na lista
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



