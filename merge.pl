% This buffer is for notes you don't want to save.
% If you want to create a file, visit that file with C-x C-f,
% then enter the text in that file's own buffer.


%Para testar: merge([2,10,17],[5,8,12,20,45],X).

merge([],L,L).
merge(L,[],L).
merge([X|Y],[X1|Y1],[X|R]):-
    X < X1,
    merge(Y,[X1|Y1],R),!.
merge(L,[X1|Y1],[X1|R]):-
    merge(L,Y1,R).

pertence(X,[X|_]).
pertence(X,[_|Y]):-
    pertence(X,Y).

%pares([]). terminar
