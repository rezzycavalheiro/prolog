% This buffer is for notes you don't want to save.
% If you want to create a file, visit that file with C-x C-f,
% then enter the text in that file's own buffer.

% Família Pereira Santos
pessoa(joao, pereira_santos, nascimento(27,05,1938), origem(curitiba), profissao(engenheiro)).
pessoa(maria, pereira_santos, nascimento(12,06,1945), origem(sao_paulo), profissao(professora)).
pessoa(julio, pereira_santos, nascimento(14,09,1972), origem(curitiba), profissao(medico)).
pessoa(ana, pereira_santos, nascimento(22,11,1975), origem(curitiba), profissao(dentista)).
pessoa(claudia, pereira_santos, nascimento(05,05,1978), origem(curitiba), profissao(musico)).

% Família Silva Pinheiro
pessoa(carlos, silva_pinheiro, nascimento(01,04,1962), origem(guarulhos), profissao(mecanico)).
pessoa(ana_claudia, silva_pinheiro, nascimento(18,07,1966), origem(castro), profissao(do_lar)).
pessoa(silvia, silva_pinheiro, nascimento(27,12,1998), origem(sao_paulo), profissao(nenhuma)).
pessoa(carolina, silva_pinheiro, nascimento(27,12,1998), origem(sao_paulo), profissao(nenhuma)).
pessoa(claudia, silva_pinheiro, nascimento(15,04,2003), origem(curitiba), profissao(nenhuma)).

% Família Nogueira Carvalho
pessoa(marcos, nogueira_carvalho, nascimento(12,07,1952), origem(curitiba), profissao(advogado)).
pessoa(patricia, nogueira_carvalho, nascimento(07,09,1952), origem(jau), profissao(enfermeira)).
pessoa(andrea,  nogueira_carvalho, nascimento(14,02,1978), origem(curitiba), profissao(nenhuma)).
pessoa(augusto, nogueira_carvalho, nascimento(22,12,1983), origem(curitiba), profissao(nenhuma)).

mesma_cidade_profissao(X,Y,C,P):-
    pessoa(X,_,_,origem(C),profissao(P)),
    pessoa(Y,_,_,origem(C),profissao(P)),
    X\=Y.

dias(D,M,A,Dias):-
    Dias is D + M*30 + A*365.

gemeos(X,Y,Z):-
    pessoa(X,Z,nascimento(D,M,A),_,_),
    pessoa(Y,Z,nascimento(D,M,A),_,_),
    X\=Y.

encontra_membros :-
    write('Por favor, entre do o nome da família: '),
    read(Sobrenome),
    write('Os membros dessa família são: '),
    pessoa(Nome,Sobrenome,nascimento(D,M,A),origem(C),profissao(P)),
    nl,nl,write('Nome: '),
    write(Nome),
    nl,write('Data de Nascimento: '),
    write(D),
    write('/'),
    write(M),
    write('/'),
    write(A),
    nl,write('Local de Nascimento: '),
    write(C),
    nl,write('Profissao: '),
    write(P),
    nl,
    fail.

encontra_membros.

