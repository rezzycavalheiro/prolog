% This buffer is for notes you don't want to save.
% If you want to create a file, visit that file with C-x C-f,
% then enter the text in that file's own buffer.
%
imc(Peso,Altura,X):-
    X is (Peso/(Altura*Altura)).

classifica(IMC,abaixo_ideal):-
    IMC =< 17, !.

classifica(IMC,ideal):-
    IMC < 25, !.

classifica(IMC,acima_ideal):-
    IMC < 30, !.

classifica(_,obesidade).

imc :-
    write('Entre com o peso: '),
    read(Peso),
    write('Entre com a altura: '),
    read(Altura),
    imc(Peso,Altura,IMC),
    classifica(IMC, Classe),
    write('Seu IMC é: '),
    write(IMC),
    nl,write('Sua classificação é: '),
    write(Classe).
