% Baseado na relação progenitor, elaborar um programa Prolog para perguntar o nome do pai e fornecer o nome e quantidade de filhos utilizando E/S

main :-
  %Progenitor(Parente,Filho).
  write('Digite o nome do pai: '),
  read(Pai), nl,
  findall(Filho, progenitor(Pai, Filho), ListaFilhos),
  write('Filhos: '), nl,
  imprimir_lista(ListaFilhos),
  quantidade(ListaFilhos, Total),
  write('Total de filhos: '),
  write(Total),nl.


imprimir_lista([]) :- !.
imprimir_lista([Elemento | Restante]) :-
  write(Elemento),nl,
  imprimir_lista(Restante).

quantidade([],0).
quantidade([_|Restante], Tamanho) :-
  quantidade(Restante, Contador),
  Tamanho is Contador+1.


% Fatos
homem(joao).
homem(carlos).
homem(fabio).
homem(hercules).
homem(aiden).
homem(estevan).
homem(dimitri).
homem(bruno).
homem(dimitri).
homem(roman).
homem(elias).

mulher(ana).
mulher(silva).
mulher(tereza).
mulher(joana).
mulher(gilvana).
mulher(brenda).
mulher(beatriz).
mulher(ines).
mulher(zilda).
mulher(jackeline).

% Pais(Filho,Pai,Mae).
pais(carlos,fabio,ana).
pais(brenda,fabio,ana).

pais(hercules,bruno,ines).
pais(joao,hercules,silva).
pais(dimitri,joao,joana).
pais(beatriz,joao,joana).
pais(roman,dimitri,zilda).
pais(jackeline,elias,beatriz).

pais(aiden,dallas,tereza).
pais(joana,estevan,gilvana).

% REGRAS
% MAE; PAI; 
/*
mãe(X,Y); pai(X,Y); é_mãe(X); é_pai(X); progenitor(X,Y); filho(X,Y); filha(X,Y) 
irmâo(X,Y); irmâ(X,Y); irmãos(X,Y); avô(X,Y); avó(X,Y); bisavô(X,Y); trisavô(X,Y) 
antepassado(X,Y); tio(X,Y); sobrinho(X,Y); primos(X,Y) 
*/

mae(Mae,Filho) :- pais(Filho, _, Mae), mulher(Mae).
pai(Pai,Filho) :- pais(Filho, Pai, _), homem(Pai).
eh_mae(Mae) :- mae(Mae,_).
eh_pai(Pai) :- pai(Pai,_).
progenitor(Parente, Filho) :- mae(Parente, Filho);pai(Parente, Filho).
filho(Filho, Pais) :- homem(Filho), (mae(Pais,Filho);pais(Filho,Pais,_)).
filha(Filha, Pais) :- mulher(Filha), (mae(Pais,Filha);pai(Pais,Filha)).
irmao(Filho1, Filho2) :- homem(Filho1), (pais(Filho1,Mae,Pai),(pais(Filho2,Mae,Pai))), Filho1 \= Filho2.
irma(Filho1, Filho2) :- mulher(Filho1), (pais(Filho1,Mae,Pai),(pais(Filho2,Mae,Pai))), Filho1 \= Filho2.
irmaos(Filho1, Filho2) :- pais(Filho1,Pai,Mae), pais(Filho2,Pai,Mae), Filho1 \= Filho2.
% avoh(AvoH) :- progenitor(AvoH, Pais), progenitor(Pais,_), homem(AvoH).
avoh(AvoH, Neto) :- progenitor(AvoH, Pais), progenitor(Pais,Neto), homem(AvoH).
% avom(AvoM) :- progenitor(AvoM, Pais), progenitor(Pais,_), mulher(AvoM).
avom(AvoM, Neto) :- progenitor(AvoM, Pais), progenitor(Pais,Neto), mulher(AvoM).
% bisavoh
bisavoh(BisavoH, BisNeto) :- avoh(BisavoH, Pais), progenitor(Pais, BisNeto).
trisavoH(TrisavoH, TrisNeto) :- bisavoh(TrisavoH, Pais), progenitor(Pais, TrisNeto).

antepassado(X,Y) :- progenitor(X,Y).
antepassado(X,Y) :- progenitor(X,T), antepassado(T,Y).

tio(Tio, Sobrinho) :- irmao(Tio, Parente), progenitor(Parente, Sobrinho), homem(Tio).
sobrinho(Sobrinho, Tio) :- progenitor(Parente, Sobrinho), irmaos(Tio, Parente).
primos(Filho, Primo) :- progenitor(Pais,Filho), irmaos(Pais,Tio), (filho(Primo,Tio);filha(Primo,Tio)	).

