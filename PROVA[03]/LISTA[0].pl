/*
 1. Crie uma base de dados de livros em Prolog, onde cada livro deve conter informações sobre o autor principal,
título e ano de publicação. Elabore as seguintes consultas:

Quais os livros de um determinado autor?
Quais os livros escritos nos anos 1997 e 1998?
 */
livro('J.K. Rowling', 'Harry Potter e a Pedra Filosofal', 1997).
livro('J.K. Rowling', 'Harry Potter e a Câmara Secreta', 1998).
livro('George Orwell', '1984', 1949).
livro('J.R.R. Tolkien', 'O Senhor dos Anéis', 1954).

%livro(Autor, Livro, _).
%livro(_, Livro, 1997);livro(_, Livro, 1998);



/*
2. Considere uma base de dados para representação de relações familiares 
com os seguintes tipos de fatos:
homem(Homem). 			 Homem é um homem 
mulher(Mulher). 		 Mulher é uma mulher 
pais(Filho, Pai, Mãe). 	 Os pais de Filho são o Pai e a Mãe 

Escreva regras para:
mãe(X,Y) 		 X é mãe de Y 
pai(X,Y)		 X é pai de Y 
é_mãe(X) 		 X é mãe 
é_pai(X) 		 X é pai 
progenitor(X,Y)  X é progenitor de Y 
filho(X,Y)		 X é filho de Y 
filha(X,Y) 		 X é filha de Y 
irmâo(X,Y)		 X é irmão de Y 
irmâ(X,Y) 		 X é irmã de Y 
irmãos(X,Y) 	 X e Y são irmãos 
avô(X,Y) 		 X é avô de Y 
avó(X,Y) 		 X é avó de Y 
bisavô(X,Y) 	 X é bisavô de Y 
trisavô(X,Y) 	 X é trisavô de Y 
antepassado(X,Y) X é antepassado de Y
tio(X,Y) 		 X é tio de Y 
sobrinho(X,Y) 	 X é sobrinho de Y 
primos(X,Y) 	 X e Y são primos 
*/


homem(joao).
homem(miguel).
homem(carlos).
homem(pedro).

mulher(maria).
mulher(ana).
mulher(carla).

pais(pedro, joao, maria).
pais(carla, joao, maria).
pais(joana, miguel, ana).

mae(X, Y) :- pais(Y, _, X), mulher(X).
pai(X, Y) :- pais(Y, X, _), homem(X).
eh_mae(X) :- pais(_,_,X).
eh_pai(X) :- pais(_,X,_).
progenitor(X,Y) :- pais(Y, X, _); pais(Y, _, X).
filho(X,Y) :- pais(X, Y,_), homem(X); pais(X, _, Y), homem(X).
filha(X,Y) :- pais(X, Y,_), mulher(X); pais(X, _, Y), mulher(X).
irmao(X, Y) :- pais(X, P, M), pais(Y, P, M), homem(X), X \= Y.
irma(X, Y) :- pais(X, P, M), pais(Y, P, M), mulher(X), X \= Y.
irmaos(X, Y) :- pais(X, P, M), pais(Y, P, M), X \= Y.
avô(X,Y) :- progenitor(X, P), progenitor(P, Y), homem(X).
avó(X,Y) :- progenitor(X, P), progenitor(P, Y), mulher(X).
bisavô(X, Y) :- progenitor(X, P), avo(P, Y).
trisavo(X, Y) :- progenitor(X, P), bisavo(P, Y).
antepassado(X, Y) :- progenitor(X, Y).
antepassado(X, Y) :- progenitor(X, P), antepassado(P, Y).
tio(X, Y) :- irmao(X, P), progenitor(P, Y).
sobrinho(X, Y) :- progenitor(P, X), irmao(Y, P), homem(X).
primos(X,Y) :- tio(P, X), tio(T,Y), irmaos(P,T).



/*
Escreva os seguintes predicados utilizando os operadores aritméticos e de comparação.
3. numero_natural(X) 			 X é um número natural 
4. numero_par(X) 				 X é um número natural par 
5. numero_impar(X)				 X é um número natural ímpar 
6. soma(X,Y,Z)					 Z é a soma dos números naturais X e Y 
7. fatorial(X,F) 				 O fatorial de X é F 
8. minimo(N1,N2,Min)			 O mínimo de N1 e N2 é Min 
9. mod(X,Y,Z) 					 Z é o resto da divisão inteira de X por Y 
*/


numero_natural(0).
numero_natural(X) :-
    X > 0,
    Y is X - 1, 
    numero_natural(Y).


numero_par(X) :- X mod 2 =:= 0.
numero_impar(X):- X mod 2 =\= 0.

soma(X,Y,Z) :- Z is X + Y, numero_natural(X),numero_natural(Y).


fatorial(0, 1). 
fatorial(Numero, Resultado) :-
    Numero > 0,
    Numero1 is Numero - 1,
    fatorial(Numero1, Resultado_parcial),
    Resultado is Numero * Resultado_parcial, !.



min(N1,N2,N1) :- N1 =< N2.
min(N1,N2,N2) :- N2 =< N1.

mod(X, Y, Z) :- Z is X mod Y.



/*
Escreva os seguintes predicados para manipulação de listas. Para cada predicado procure encontrar mais do que
uma solução.

10. list(L) 					 L é uma lista 
11. member(X,L) 				 X é um elemento da lista L
12. not_member(X,L) 			 X não é um elemento da lista L 
13. prefix(Prefix,List) 		 Prefix é um prefixo de List 
14. sufix(Sufix,List)			 Sufix é um sufixo de List 
15. sublist(Sub,List) 			 Sub é uma sublista de List 
16. append(L1,L2,List) 			 List é o resultado da concatenação de L1 e L2 
17. reverse(List,Rev) 			 Rev é o resultado de reverter List 
18. adjacent(X,Y,List)			 X e Y são elementos adjacentes em List 
19. length(List,N) 				 N é o número de elementos de List 
20. first(X,List) 				 X é o primeiro elemento de List 
21. last(X,List) 				 X é o último elemento de List
22. nth(X,N,List) 				 X é o N-ésimo elemento de List 
23. double(L, LL)				 Cada elemento de L aparece em LL em duplicado 
24. sum(Xs,Sum) 				 Sum é a soma dos elementos de Xs 
25. delete(L1,X,L2) 			 L2 resulta da eliminação de todos os X de L1 
26. select(X,L1,L2) 			 L2 resulta da eliminação de um X de L1 
27. insert(X,L1,L2)				 L2 resulta da inserção de X em L1 
28. flatten(L1,L2) 				 L2 é uma lista com todos os átomos de L1 
*/

list([]).
list([_|_]).

member(X, [X | _ ]).
member(X, [_ | R]) :- member(X, R). 

not_member(X,L) :- \+member(X,L).

prefix([], _).
prefix([H|T], [H|T2]) :- prefix(T, T2).


suffix(S, S).
suffix(S, [_|T]) :- suffix(S, T).

sublist(Sub, List) :- prefix(Sub, List).
sublist(Sub, [_|R]) :- sublist(Sub, R).

append([], L, L).
append([H|T], L2, [H|L3]) :- append(T, L2, L3).

reverse([], []).
reverse([H|T], Rev) :- 
    reverse(T, RevT), 
    append(RevT, [H], Rev).

adjacent(X, Y, [X, Y|_]).
adjacent(X, Y, [Y, X|_]).
adjacent(X, Y, [_|T]) :- adjacent(X, Y, T).

first([X|_], X).

last(X, [X]).
last(X, [_|T]) :- last(X, T).

nth(X, 1, [X|_]).
nth(X, N, [_|T]) :- 
    N > 1, 
    N1 is N - 1, 
    nth(X, N1, T).

double([], []).
double([H|T], [H,H|Resto]) :-
    double(T, Resto). 

sum([], 0). 
sum([H|T], Sum) :-
    sum(T, RestSum),
    Sum is H + RestSum. 

delete([], _, []).
delete([X|T], X, L2) :- 
    delete(T, X, L2).
delete([H|T], X, [H|T2]) :- 
    H \= X,
    delete(T, X, T2).


select(X, [X|T], T).
select(X, [H|T], [H|T2]) :- 
    select(X, T, T2).

insert(X, L, [X|L]). % Insere no início da lista.
insert(X, [H|T], [H|T2]) :- % Insere no restante da lista recursivamente.
    insert(X, T, T2).

flatten([], []). % Caso base: lista vazia permanece vazia.
flatten([H|T], Flat) :-
    is_list(H), % Se o elemento atual for uma lista, achata-o.
    flatten(H, FlatH),
    flatten(T, FlatT),
    append(FlatH, FlatT, Flat).
flatten([H|T], [H|FlatT]) :- % Se o elemento atual não for uma lista, inclui-o diretamente.
    \+ is_list(H),
    flatten(T, FlatT).


/*
29. union(L1,L2,L3)  L3 resulta da união de L1 com L2 
30. intersection(L1,L2,L3)  L3 resulta da intersecção de L1 com L2 
31. diference(L1,L2,L3) L3 resulta da diferença de L1 com L2 
32. equivalence(L1,L2)  L1 é um conjunto equivalente a L2 
33. subset(L1,L2) L1 é um subconjunto de L2 
*/
union([], L, L). % Caso base: unir uma lista vazia com outra resulta na própria lista.
union([H|T], L, L3) :-
    member(H, L), % Se o elemento já está na segunda lista, ignora-o.
    union(T, L, L3).
union([H|T], L, [H|L3]) :- % Caso contrário, adiciona o elemento à lista de união.
    \+ member(H, L),
    union(T, L, L3).


intersection([], _, []). % Caso base: interseção com uma lista vazia é vazia.
intersection([H|T], L, [H|L3]) :-
    member(H, L), % Se o elemento está na segunda lista, inclui-o na interseção.
    intersection(T, L, L3).
intersection([_|T], L, L3) :- % Caso contrário, ignora o elemento.
    intersection(T, L, L3).


difference([], _, []). % Caso base: diferença com uma lista vazia é vazia.
difference([H|T], L, L3) :-
    member(H, L), % Se o elemento está na segunda lista, ignora-o.
    difference(T, L, L3).
difference([H|T], L, [H|L3]) :- % Caso contrário, inclui-o na diferença.
    \+ member(H, L),
    difference(T, L, L3).


subset([], _). % Uma lista vazia é subconjunto de qualquer lista.
subset([H|T], L) :-
    member(H, L), % Cada elemento de L1 deve estar em L2.
    subset(T, L).


equivalence(L1, L2) :-
    subset(L1, L2), % L1 é subconjunto de L2
    subset(L2, L1). % e L2 é subconjunto de L1.



/*
34. Implemente um conjunto de predicados em Prolog para representar pontos, retas e
circunferências. Além disso, desenvolva predicados para calcular a distância entre dois
pontos e a área de uma circunferência.

Definições: Um ponto é representado como point(X, Y), onde X e Y são as coordenadas no
plano cartesiano. Uma reta é representada por dois pontos line(Point1, Point2). Uma
circunferência é representada como circle(Center, Radius), onde Center é um ponto e Radius
é um número positivo. 

Predicados a Implementar

Distância entre dois pontos :
distance(Point1, Point2, Distance): calcula a distância entre `Point1` e `Point2`.

Área de uma circunferência:
area(Circle, Area)`: calcula a área da circunferência dada.
*/
% Distância entre dois pontos
distance(point(X1, Y1), point(X2, Y2), Distance) :-
    DX is X2 - X1,
    DY is Y2 - Y1,
    Distance is sqrt(DX^2 + DY^2).

% Área da circunferência
area(circle(_, Radius), Area) :-
    Area is 3.14159 * Radius^2.



/*
35. Apresente os resultados das unificações a seguir

?- 1+2 == +(1,2).				 % true.
?- 1+2 == 2+1.					 % false.
?- 1+2 == 1+X.					 % false.
?- 1+2 = 1+X.					 % X = 2.
?- 1+2 = X. 					 % = 1+2.

?- 1+2 =:= 2+1. 			       
?- 1+2 =:= 2+X.								% false, X não foi instanciado.
?- X is 1+2. 								% X = 3.
?- 1+2 is X. 								% Erro, is requer que X seja instanciado.
?- [a, b, c] = X.				 			% X = [a, b, c].
?- [H|T] = [a, b, c]. 						% H = a, T = [b, c].
?- [X, [Y, Z]] = [a, [b, c]].				% X = a, Y = b, Z = c.
?- [X, Y, Z] = [1, 2, 3].	 				% X = 1, Y = 2, Z = 3.
?- [X, Y] = [1, 2, 3]. 						% false.
?- [a, X, b] = [a, y, Z].					% X = y, Z = b.
?- [] = [X | Y]. 							% false.
?- [X, Y | Z] = [1, 2, 3, 4, 5]. 			% X = 1, Y = 2, Z = [3, 4, 5].
?- [X, X, Y] = [a, a, b].				 	% X = a, Y = b.
?- [X, Y, Z | Rest] = [1, 2, 3, 4, 5, 6]. 	% X = 1, Y = 2, Z = 3, Rest = [4, 5, 6].
*/









































