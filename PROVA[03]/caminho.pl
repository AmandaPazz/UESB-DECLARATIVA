conexao(0,1).
conexao(1,2).
conexao(2,3).
conexao(3,1).
conexao(0,4).
conexao(4,3).
conexao(3,4).

% caminho(Inicio,Fim,Visitados,Caminho)

caminho(Elemento,Elemento,_,[Elemento]).
caminho(Inicio,Destino,Visitado,[Inicio|ListaCaminho]) :-
	conexao(Inicio,Intermediario),
	not(member(Intermediario,Visitado)),
	caminho(Intermediario,Destino,[Inicio|Visitado],ListaCaminho).


%?- caminho(0,3,[ ],L).
%L = [0, 1, 2, 3] ;
%L = [0, 4, 3] ;
