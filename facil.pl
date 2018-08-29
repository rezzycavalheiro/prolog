% This buffer is for notes you don't want to save.
% If you want to create a file, visit that file with C-x C-f,
% then enter the text in that file's own buffer.

facil(1).
facil(2).
facil(3).
gizmo(a,1).
gizmo(b,3).
gizmo(a,2).
gizmo(d,5).
gizmo(c,3).
gizmo(a,3).
gizmo(c,4).

a(a1,1).
a(A,2).
a(a3,N).
b(1,b1).
b(2,B).
b(N,b3).
c(X,Y) :-
	a(X,N),
	b(N,Y).
d(X,Y) :-
	a(X,N),
	b(Y,N).

d(X,Y) :-
	a(N,X),
	b(N,Y).

p(a).  q(a,1).  r(1,1).  r(3,5).
p(b).  q(a,2).  r(1,2).  r(3,6).
       q(b,3).  r(2,3).  r(4,7).
       q(b,4).  r(2,4).  r(4,8).


