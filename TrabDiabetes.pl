
% Benjamin Gordon  <BenBxg.Org>
% Search in Exshell

% Updated 21-Aug-2009 by Benjamin Gordon
% Changes:
%   * Removed conflicting predicates to work with SWI Prolog 5.7.6
%   * Added human-readable question to be printed with queries
%   * Improved handling of case 7 (fallthrough to assuming input is Prolog code)
%   * Small adjustments to help and printouts

% solve/2 succeeds with
%	argument 1 bound to a goal proven true using the current knowledge base
%	argument 2 bound to the confidence in that goal.
%
% solve/2 calls solve/4 with appropriate arguments.  After solve/4 has completed,
% it writes the conclusions and prints a trace.

:- dynamic known/2.

solve(Goal, CF) :-
	retractall(known(_,_)),
	print_instructions,
	solve(Goal, CF, [], 5),
	write(Goal), write(' foi concluido com certeza '), write(CF), nl,nl,
	build_proof(Goal, _, Proof),nl,
	write('A prova é: '),nl,nl,
	write_proof(Proof, 0), nl,nl.

%solve/4 succeeds with
%	argument 1 bound to a goal proven true using the current knowledge base
%	argument 2 bound to the confidence in that goal.
%	argument 3 bound to the current rule stack
%	argument 4 bound to the threshold for pruning rules.
%
%solve/4 is the heart of exshell.  In this version, I have gone back to the
% simpler version.  It still has problems with negation, but I think that
% this is more a result of problems with the semantics of Stanford Certainty
% factors than a bug in the program.
% The pruning threshold will vary between 20 and -20, depending whether,
% we are trying to prove the current goal true or false.
% solve/4 handles conjunctive predicates, rules, user queries and negation.
% If a predicate cannot be solved using rules, it will call it as a PROLOG predicate.

% Case 1: truth value of goal is already known
solve(Goal, CF, _, Threshold) :-
	known(Goal, CF),!,
	above_threshold(CF, Threshold).

% Case 2: negated goal
solve( not(Goal), CF, Rules, Threshold) :- !,
	invert_threshold(Threshold, New_threshold),
	solve(Goal, CF_goal, Rules, New_threshold),
	negate_cf(CF_goal, CF).

% Case 3: conjunctive goals
solve((Goal_1,Goal_2), CF, Rules, Threshold) :- !,
	solve(Goal_1, CF_1, Rules, Threshold),
	above_threshold(CF_1, Threshold),
	solve(Goal_2, CF_2, Rules, Threshold),
	above_threshold(CF_2, Threshold),
	and_cf(CF_1, CF_2, CF).

%Case 4: backchain on a rule in knowledge base
solve(Goal, CF, Rules, Threshold) :-
	regra((Goal :- (Premise)), CF_rule),
	solve(Premise, CF_premise,
		[regra((Goal :- Premise), CF_rule)|Rules], Threshold),
	rule_cf(CF_rule, CF_premise, CF),
	above_threshold(CF, Threshold).

%Case 5: fact assertion in knowledge base
solve(Goal, CF, _, Threshold) :-
	regra(Goal, CF),
	above_threshold(CF, Threshold).

% Case 6: ask user
solve(Goal, CF, Rules, Threshold) :- !,
	askable(Goal, Question),
	askuser(Goal, Question, CF, Rules),!,
	assert(known(Goal, CF)),
	above_threshold(CF, Threshold).

% Case 7A: All else fails, see if goal can be solved in prolog.
solve(Goal,100,_,_) :-   % colocado
	mycall(Goal).

/*solve(Goal, 100, _, _) :-
	unknown(X,fail),
	(call(Goal);unknown(_,X),fail),
	unknown(_,X).*/

solve(_,_,[],_) :-
	write( 'Nenhuma solução encontrada com a atual base de conhecimento.' ), nl, fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%
% Certainty factor predicates.  Currently, these implement a variation of
% the MYCIN certainty factor algebra.
% The certainty algebra may be changed by modifying these predicates.

% negate_cf/2
%	argument 1 is a certainty factor
%	argument 2 is the negation of that certainty factor

negate_cf(CF, Negated_CF) :-
	Negated_CF is -1 * CF.

% and_cf/3
%	arguments 1 &amp; 2 are certainty factors of conjoined predicates
%	argument 3 is the certainty factor of the conjunction

and_cf(A, B, A) :- A >= B,!.
and_cf(_, B, B).

%rule_cf/3
%	argument 1 is the confidence factor given with a rule
%	argument 2 is the confidence inferred for the premise
%	argument 3 is the confidence inferred for the conclusion

rule_cf(CF_rule, CF_premise, CF) :-
	CF is CF_rule * CF_premise/100.

%above_threshold/2
%	argument 1 is a certainty factor
%	argument 2 is a threshold for pruning
%
% If the threshold, T, is positive, assume we are trying to prove the goal
% true.  Succeed if CF >= T.
% If T is negative, assume we are trying to prove the goal
% false.  Succeed if CF <= T.

above_threshold(CF, T) :-
	T >= 0, CF >= T.
above_threshold(CF, T) :-
	T < 0, CF =< T.

%invert_threshold/2
%	argument 1 is a threshold
%	argument 2 is that threshold inverted to account for a negated goal.
%
% If we are trying to prove not(p), then we want to prove p false.
% Consequently, we should prune proofs of p if they cannot prove it
% false.  This is the role of threshold inversion.

invert_threshold(Threshold, New_threshold) :-
	New_threshold is -1 * Threshold.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%
% Predicates to handle user interactions.  As is typical, these
% constitute the greatest bulk of the program.
%
% askuser/3
%	argument 1 is a goal whose truth is to be asked of the user.
%	argument 2 is the confidence the user has in that goal
%	argument 3 is the current rule stack (used for why queries).
%
% askuser prints the query, followed by a set of instructions.
% it reads the response and calls respond/4 to handle that response

askuser(Goal, Question, CF, Rules) :-
	nl,write('Pergunta: '), write(Question),
	write('? '),
	read(Answer),
	respond(Answer,Goal, Question, CF, Rules).

%respond/4
%	argument 1 is the user response
%	argument 2 is the goal presented to the user
%	argument 3 is the CF obtained for that goal
%	argument 4 is the current rule stack (used for why queries).
%
% The basic scheme of respond/4 is to examine the response and return
% the certainty for the goal if possible.
% If the response is a why query, how query, etc., it processes the query
% and then calls askuser to re prompt the user.

% Case 1: user enters a valid confidence factor.
respond(CF, _, _, CF, _) :-
	number(CF),
	CF =< 100, CF >= -100.

% Case 2: user enters 'n' for no.  Return a confidence factor of -1.0
respond(n, _, _, -100, _).

% Case 3: user enters 'y' for yes.  Return a confidence factor of 1.0
respond(s, _, _, 100, _).

% Case 4: user enters a pattern that matches the goal.  This is useful if
% the goal has variables that need to be bound.
respond(Goal, Goal, _, CF, _) :-
	write('Insira a confiança na resposta'), nl,
	write('?'),
	read(CF).

% Case 5: user enters a why query
respond(por_que, Goal, Question, CF, [Rule|Rules]) :-
	write_rule(Rule),
	askuser(Goal, Question, CF, Rules).

respond(por_que, Goal, Question, CF, []) :-
	write('De volta para o topo da pilha.'),
	askuser(Goal, Question, CF, []).

% Case 6: User enters a how query.  Build and print a proof
respond(como(X), Goal, Question, CF, Rules) :-
	build_proof(X, CF_X, Proof),!,
	write(X), write(' foi concluido com certeza '), write(CF_X), nl,nl,
	write('A prova é: '),nl,nl,
	write_proof(Proof, 0), nl,nl,
	askuser(Goal, Question, CF, Rules).

% User enters how query, could not build proof
respond(como(X), Goal, Question, CF, Rules):-
	write('A verdade de '), write(X), nl,
	write('ainda não é conhecida.'), nl,
	askuser(Goal, Question, CF, Rules).

% Case 7: User asks for the rules that conclude a certain predicate
respond(regra(X), _, _, _, _) :-
	write('As regras seguintes concluem sobre '), write(X),nl,nl,
	regra((X :- Premise), CF),
	write(regra((X :- Premise), CF)), nl,
	fail.

respond(regra(_),Goal, Question, CF, Rules) :-
	askuser(Goal, Question, CF, Rules).

% Case 8: User asks for help.
respond(ajuda, Goal, Question, CF, Rules) :-
	print_instructions,
	askuser(Goal, Question, CF, Rules).

%Case 9: User wants to quit.
respond(sair,_, _, _, _) :- quit.

%Case 10: Unrecognized input
respond(_, Goal, Question, CF, Rules) :-
	write('Resposta não reconhecida.'),nl,
	askuser(Goal, Question, CF, Rules).

%build_proof/3
%	argument 1 is the goal being traced.
%	argument 2 is the CF of that goal
%	argument 3 is the proof tree
%
% build_proof does not do threshold pruning, so it can show
% the proof for even goals that would not succeed.
build_proof(Goal, CF, ((Goal,CF) :- given)) :-
	known(Goal, CF),!.

build_proof(not(Goal), CF, not(Proof)) :- !,
	build_proof(Goal, CF_goal, Proof),
	negate_cf(CF_goal, CF).

build_proof((Goal_1, Goal_2), CF, (Proof_1, Proof_2)) :- !,
	build_proof(Goal_1, CF_1, Proof_1),
	build_proof(Goal_2, CF_2, Proof_2),
	and_cf(CF_1, CF_2, CF).

build_proof(Goal, CF, ((Goal,CF) :- Proof)) :-
	regra((Goal :- (Premise)), CF_rule),
	build_proof(Premise, CF_premise, Proof),
	rule_cf(CF_rule, CF_premise, CF),!.

build_proof(Goal, CF, ((Goal, CF):- fact)) :-
	regra(Goal, CF),!.  %colocado cut

build_proof(Goal, 1, ((Goal, 1):- call)) :-  % colocado
	mycall(Goal).

/*
build_proof(Goal, 1, ((Goal, 1):- call)) :-
	unknown(X,fail),
	(call(Goal);unknown(_,X),fail),
	unknown(_,X).
*/

% write_proof/2
%	argument 1 is a portion of a proof tree
%	argument 2 is the depth of that portion (for indentation)
%
% writes out a proof tree in a readable format
write_proof(((Goal,CF) :- given), Level) :-
	indent(Level),
	write(Goal), write(' CF= '), write(CF),
	write(' foi fornecido pelo usuário'), nl,!.

write_proof(((Goal, CF):- fact), Level) :-
	indent(Level),
	write(Goal), write(' CF= '), write(CF),
	write(' era um fato da base de conhecimento'), nl,!.

write_proof(((Goal, CF):- call), Level) :-
	indent(Level),
	write(Goal), write(' CF= '), write(CF),
	write(' foi provado por uma chamada Prolog'), nl,!.

write_proof(((Goal,CF) :- Proof), Level) :-
	indent(Level),
	write(Goal), write(' CF= '), write(CF), write(' :-'), nl,
	New_level is Level + 1,
	write_proof(Proof, New_level),!.

write_proof(not(Proof), Level) :-
	indent(Level),
	write('not'),nl,
	New_level is Level + 1,
	write_proof(Proof, New_level),!.

write_proof((Proof_1, Proof_2), Level) :-
	write_proof(Proof_1, Level),
	write_proof(Proof_2, Level),!.

% indent/1
%	argument 1 is the number of units to indent
indent(0).
indent(I) :-
	write('     '),
	I_new is I - 1,
	indent(I_new).

%print_instructions/0
% Prints all options for user responses
print_instructions :-
	nl, write('Resposta de ser:'), nl,
	write(': Um número entre -100 e 100.'), nl,
	write(': s ou n, onde s é equivalente a fator de confiança de 100 e'), nl,
	write('	            n é equivalente a fator de certeza de -100.'), nl,
	write(': Meta, onde Meta é um padrão que unificará com a query'), nl,
	write(': por_que'),nl,
	write(': como(X), onde X é uma meta'),nl,
	write(': regra(X) para mostrar todas as regras que concluem algo sobre X.'),nl,
	write(': sair, para encerrar'),nl,
	write(': ajuda, para imprimir esse menu'), nl,nl,
	write('Sua resposta deve sempre finalizar com (.) para ser processada.'),nl.


% write_rule/1
%	argument 1 is a rule specification
% writes out the rule in a readable format
write_rule(regra((Goal :- (Premise)), CF)) :-
	write(Goal), write(' se'), nl,
	write_premise(Premise),nl,
	write('CF = '), write(CF), nl.

write_rule(regra(Goal, CF)) :-
	write(Goal),nl,
	write('CF = '), write(CF), nl.


% write_premise
%	argument 1 is a rule premise
% writes it in a readable format.
write_premise((Premise_1, Premise_2)) :-!, write_premise(Premise_1), write(' E '), write_premise(Premise_2).
write_premise(\+ Premise) :- !, write('     '), write(not),write(' '), write(Premise),nl.
write_premise(Premise) :- write('     '), write(Premise),nl.

mycall(Goal) :-
	clause(Goal,_),
	Goal.

% This is the sample automotive diagnostic knowledge base for use
% with the EXSHELL expert system shell in section 12.2 of the text.
% When running it, be sure to load it with the file containing
% EXSHELL.

% To start it, give PROLOG the goal:
%		solve(indicacao(X), CF).

% Knowledge Base for simple automotive diagnostic expert system.
% Top level goal, starts search.

regra((indicacao(Advice) :-
      (resultado(X),indicacao(X, Advice))), 100).

% Regras para inferir resultados:

regra((resultado(improvavel) :-
      (doenca(regra2))),90).

regra((resultado(semjejum) :-
      (doenca(regra1))),90).

regra((resultado(naodiabetes) :-
      (doenca(regra3))),90).

regra((resultado(semjejum) :-
      (doenca(regra4))),90).

regra((resultado(naodiabetes) :-
      (doenca(regra5))),90).

regra((resultado(comjejum) :-
      (doenca(regra6))),90).

regra((resultado(outrosemjejum) :-
      (doenca(regra7))),90).

regra((resultado(diabetes) :-
      (doenca(regra8))),90).

regra((resultado(naodiabetes) :-
      (doenca(regra9))),90).

regra((resultado(glicose) :-
      (doenca(regra10))),90).

regra((resultado(outrocomjejum) :-
      (doenca(regra11))),90).

regra((resultado(diabetes) :-
      (doenca(regra12))),90).

regra((resultado(comjejum) :-
      (doenca(regra13))),90).

regra((resultado(naodiabetes) :-
      (doenca(regra14))),90).

regra((resultado(diabetes) :-
      (doenca(regra15))),90).

regra((resultado(intglicose) :-
      (doenca(regra16))),90).

regra((resultado(glicose) :-
      (doenca(regra19))),90).

regra((resultado(comjejum) :-
      (doenca(regra20))),90).

% Regras de inferencia para doenca.

regra((doenca(regra3) :-
	((primcon), qs_igual(0))),90).

regra((doenca(regra2) :-
	((primcon), qs_menor(7), qs_maior(0))),90).

regra((doenca(regra1) :-
	((primcon), qs_maior(6))),90).

regra((doenca(regra4) :-
        (not(primcon), not(etapasemjejum),semjejum_igual(0))),90).

regra((doenca(regra5) :-
	(not(primcon), not(etapasemjejum),semjejum_maior(0), glicemiasemjejum_menor(100))),90).

regra((doenca(regra6) :-
	(not(primcon), not(etapasemjejum), semjejum_maior(1), glicemiasemjejum_maior(100), glicemiasemjejum_menor(200))),90).

regra((doenca(regra8) :-
        (not(primcon), not(etapasemjejum), semjejum_maior(1),glicemiasemjejum_maior(199), glicemiasemjejum2_maior(199))),90).

regra((doenca(regra7) :-
	(not(primcon), not(etapasemjejum), semjejum_igual(1), glicemiasemjejum_maior(199))),90).

regra((doenca(regra9) :-
	(not(primcon), etapasemjejum, not(fimconsulta),not(etapacomjejum),comjejum_maior(0), glicemiacomjejum_menor(100))),90).

regra((doenca(regra10) :-
	(not(primcon), etapasemjejum, not(fimconsulta), not(etapacomjejum), comjejum_maior(0), glicemiacomjejum_maior(100), glicemiacomjejum_menor(139))),90).

regra((doenca(regra11) :-
	(not(primcon), etapasemjejum, not(fimconsulta), not(etapacomjejum), comjejum_igual(1), glicemiacomjejum_maior(139))),90).

regra((doenca(regra12) :-
	(not(primcon), etapasemjejum, not(fimconsulta), not(etapacomjejum), comjejum_maior(1), glicemiacomjejum_maior(139), glicemiacomjejum2_maior(139))),90).

regra((doenca(regra13) :-
	(not(primcon), etapasemjejum, not(fimconsulta), not(etapacomjejum), comjejum_igual(0))),90).

regra((doenca(regra14) :-
        (not(primcon), etapasemjejum, not(fimconsulta), not(notfimconsulta2), ttg_menor(140))),90).

regra((doenca(regra15) :-
	(not(primcon), etapasemjejum, not(fimconsulta), not(fimconsulta2), ttg_maior(199))),90).

regra((doenca(regra16) :-
	(not(primcon), etapasemjejum, not(fimconsulta), not(fimconsulta2), ttg_maior(139), ttg_menor(198))),90).

regra((doenca(regra19) :-
	(not(primcon), etapasemjejum, not(etapacomjejum), comjejum_maior(1), glicemiacomjejum_maior(139), glicemiacomjejum2_menor(14))),90).

regra((doenca(regra20) :-
	(not(primcon), not(etapasemjejum),semjejum_maior(1),glicemiasemjejum_maior(199),glicemiasemjejum2_menor(200))),90).

% Regras de recomendacao para o item identificado.

regra(indicacao(improvavel, 'Diabetes improvavel. Retorne depois de 1 mes para nova consulta.'),100).
regra(indicacao(diabetes, 'Diabetes!'),100).
regra(indicacao(naodiabetes, 'Nao diabates!'),100).
regra(indicacao(intglicose, 'Intolerancia a glicose.'),100).
regra(indicacao(semjejum, 'Realize o teste de glicemia sem jejum.'),100).
regra(indicacao(outrosemjejum, 'Realize outro teste de glicemia sem jejum.'),100).
regra(indicacao(comjejum, 'Realize o teste de glicemia com jejum.'),100).
regra(indicacao(outrocomjejum, 'Realize outro teste de glicemia com jejum.'),100).
regra(indicacao(glicose, 'Realize o teste de tolerancia a glicose'),100).

% Variaveis que podem ser questionadas ao usuario.

askable(primcon, 'Eh a sua primeira consulta').
askable(qs_maior(X),M) :- string_concat('QS maior que ',X,M).
askable(qs_igual(X),M) :- string_concat('QS igual a ',X,M).
askable(qs_menor(X),M) :- string_concat('QS menor que ',X,M).
askable(etapasemjejum, 'Etapa de glicemia sem jejum concluida').
askable(etapacomjejum, 'Etapa de glicemia com jejum concluida').
askable(semjejum_maior(X),M) :- string_concat('Quantidade de vezes que realizou
o exame sem jejum maior que ',X,M).
askable(semjejum_menor(X),M) :- string_concat('Quantidade de vezes que realizou
o exame sem jejum menor que ',X,M).
askable(semjejum_igual(X),M) :- string_concat('Quantidade de vezes que realizou
o exame sem jejum igual a ',X,M).
askable(glicemiasemjejum_maior(X),M) :- string_concat('Glicemia sem jejum maior que ',X,M).
askable(glicemiasemjejum_igual(X),M) :- string_concat('Glicemia sem jejum igual a ',X,M).
askable(glicemiasemjejum_menor(X),M) :- string_concat('Glicemia sem jejum menor que ',X,M).
askable(glicemiasemjejum2_maior(X),M) :- string_concat('Segundo exame de glicemia
sem jejum maior que ',X,M).
askable(glicemiasemjejum2_igual(X),M) :- string_concat('Segundo exame de glicemia
sem jejum igual a ', X,M).
askable(glicemiasemjejum2_menor(X),M) :- string_concat('Segundo exame de glicemia
sem jejum menor	que ', X,M).
askable(fimconsulta, 'Fim consulta').
askable(fimconsulta2, 'Fim consulta 2').
askable(comjejum_maior(X),M) :- string_concat('Quantidade de vezes que realizou o exame com jejum maior que ',X,M).
askable(comjejum_igual(X),M) :- string_concat('Quantidade de vezes que realizou o exame com jejum igual a ',X,M).
askable(comjejum_menor(X),M) :- string_concat('Quantidade de vezes que realizou o exame com jejum menor que ',X,M).
askable(glicemiacomjejum_maior(X),M) :- string_concat('Glicemia com jejum maior que ',X,M).
askable(glicemiacomjejum_igual(X),M) :- string_concat('Glicemia com jejum igual a ',X,M).
askable(glicemiacomjejum_menor(X),M) :- string_concat('Glicemia com jejum menor que ',X,M).
askable(glicemiacomjejum2_maior(X),M) :- string_concat('Segundo exame de glicemia com jejum maior que ',X,M).
askable(glicemiacomjejum2_igual(X),M) :- string_concat('Segundo exame de glicemia com jejum igual a ',X,M).
askable(glicemiacomjejum2_maior(X),M) :- string_concat('Segundo exame de glicemia com jejum menor que ',X,M).
askable(ttg_maior(X),M) :- string_concat('Teste de tolerancia a glicose maior que ',X,M).
askable(ttg_igual(X),M) :- string_concat('Teste de tolerancia a glicose igual a ',X,M).
askable(ttg_menor(X),M) :- string_concat('Teste de tolerancia a glicose menor que ',X,M).







