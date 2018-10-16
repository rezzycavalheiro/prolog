% This buffer is for notes you don't want to save.
% If you want to create a file, visit that file with C-x C-f,
% then enter the text in that file's own buffer.

%Listas como representação de Estados
%eh_solucao([_,2]).

%Atividade:
%Implementar o resolvedor usando Prolog
%Implementar os operadores para o problema dos jarros de agua
%da seguinte forma:
%
%1. enche X
%2. enche Y
%3. esvazia X
%4. esvazia Y
%5. transfere tudo de X para Y
%6. transfere tudo de Y para X
%7. transfere de X para Y restando em X
%8. transfere de Y para X restando em Y
%solução: 2 litros no jarro de 4 litros
%Estado inicial: [0,0]
%Estado final: [_,2]

%algoritmo resolvedor

operador([X,Y],[3,Y]):-
    X<3.

eh_solucao([_,2]).

resolvedor(Estado,Visitados).
eh_solucao(Estado):-
    s =([Estado|Visitados]).
eh_solucao(Estado):-
    s \= (membro[Estado,Visitados).
operador(op,Estado,NovoEstado):-


aplica(op,Estado,Visitados):-
    s = resolvedor(NovoEstado,[Estado|Visitados]).
aplica(op,Estado,Visitados):-
    s \= 0.


