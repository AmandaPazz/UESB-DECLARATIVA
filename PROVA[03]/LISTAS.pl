cons(X,Y, [X|Y]).

membro(X, [X|_] ).
membro(X, [_|C]) :- membro(X,C).

conc([], L, L).
conc([X|L1], L2, [X| L3]) :- conc(L1,L2,L3).

inverter([ ],[ ]).
inverter([X|Y],Z) :- 
    inverter(Y,Y1),
    conc(Y1,[X],Z).


tamanho([ ],0).
tamanho([_|R],N) :- 
    tamanho(R,N1),
    N is N1+1.

produto([ ],1).
produto([X|R],N) :-
    produto(R, N1),
    N is N1 * X.


inserir(E,[ ],[E]).

inserir(E,[H|T],[H|L]) :- 
    E>=H,
    inserir(E,T,L).

inserir(E,[H|T],[E,H|T]) :- 
    E<H.


ordenar([ ],[ ]).
ordenar([H|T],LO) :-
    ordenar(T,TO),
    inserir(H,TO,LO).

enesimo(1,X,[X|_]).
enesimo(N,X,[_|T]) :- 
    N1 is N-1,
    enesimo(N1,X,T).
   


conexao(0,1).
conexao(1,2).
conexao(2,3).
conexao(3,1).
conexao(0,4).
conexao(4,3).
conexao(3,4).

% caminho(Inicio,Fim,Visitados,Caminho)
caminho(X,X,_,[X]).
caminho(X,Y,V,[X|C2]) :-
  conexao(X,T),
  not(member(T,V)),
  caminho(T,Y,[X|V],C2).
        
        
        
        
        
        
        
        
