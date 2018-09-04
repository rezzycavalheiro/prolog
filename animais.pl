animal(leao).
animal(baleia).
animal(cobra).
animal(macaco).
animal(girafa).

%negação por falha
nao_eh_macaco(macaco):-
   !,fail.

nao_eh_macaco(_).

gosta(joana, X):-
    animal(X),
    nao_eh_macaco(X).
