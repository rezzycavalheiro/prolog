casado(pedroluiz,denize).
pai(pedroluiz,renata).
pai(pedroluiz,rodrigo).
pai(pedroluiz,thais).

casado(pedro,eneida).
pai(pedro,pedroluiz).
pai(pedro,joaoaugusto).
pai(pedro,rita).
pai(pedro,fernando).

casado(juca,cemila).
pai(juca,rose).
pai(juca,bernadete).
pai(juca,cleonice).
pai(juca,joaocarlos).
pai(juca,denize).

casado(joaoaugusto,marilda).
pai(joaoaugusto,paulo).
pai(joaoaugusto,miguel).

casado(fernando,sandra).
pai(fernando,beatriz).
pai(fernando,eduardo).

casado(luis,bernadete).
pai(luis,raffael).
pai(luis,felipe).

casado(sella,cleonice).
pai(sella,junior).
pai(sella,anapaula).
pai(sella,luisroberto).

casado(joaocarlos,valeria).
pai(joaocarlos,carlaeduarda).
pai(joaocarlos,carloseduardo).

casado(carloseduardo,fulana).
pai(carloseduardo,gordinho).

mae(X,Y) :-
    casado(Z,X),
    pai(Z,Y).
irmao(X,Y) :-
    mae(Z,X),
    mae(Z,Y),
    X \= Y.
irmao(X,Y) :-
    pai(Z,X),
    pai(Z,Y),
    X \= Y.
tio(X,Y) :-
    irmao(X,Z),
    mae(Z,Y).
tio(X,Y) :-
    irmao(X,Z),
    pai(Z,Y).
avo(X,Y) :-
    pai(X,Z),
    pai(Z,Y),!.
avo(X,Y) :-
    pai(X,Z),
    mae(Z,Y).
avo_(X,Y) :-
    mae(X,Z),
    mae(Z,Y).
avo_(X,Y) :-
    mae(X,Z),
    pai(Z,Y).
primo(X,Y) :-
    pai(Z,X),
    tio(Z,Y).
primo(X,Y) :-
    mae(Z,X),
    tio(Z,Y).
cunhado(X,Y):-
    casado(X,Z),
    irmao(Z,Y).
nora(X,Y):-
    mae(Y,Z),
    casado(Z,X).
nora(X,Y):-
    pai(Y,Z),
    casado(Z,X).
primo2o(X,Y):-
    primo(Y,Z),
    pai(Z,X).







