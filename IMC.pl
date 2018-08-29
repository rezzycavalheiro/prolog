imc(Peso,Altura,X):-
    X is (Peso/(Altura*Altura)).

classifica(IMC,abaixo_ideal):-
    IMC < 17.

classifica(IMC,ideal):-
    IMC >= 17, IMC =< 23.

classifica(IMC,acima_ideal):-
    IMC > 23.

imc :-
    write('Entre com o peso: '),
    read(Peso),
    write('Entre com a altura: '),
    read(Altura),
    imc(Peso,Altura,IMC),
    classifica(IMC, Classe),
    write('Seu IMC é: '),
    write(IMC),
    format('~2f',[IMC]),
    nl,write('Sua classificação é: '),
    write(Classe).





