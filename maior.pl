% This buffer is for notes you don't want to save.
% If you want to create a file, visit that file with C-x C-f,
% then enter the text in that file's own buffer.

maior(X,Y,X):-
    X >= Y.
maior(X,Y,Y):-
    Y > X.

maior2(X,Y,X):-
    writeln(1),
    X >= Y, !.
maior2(X,Y,Y):-
    writeln(2),
    Y > X.
maior3(X,Y,X):-
    X >= Y, !.
maior3(_,Y,Y).
