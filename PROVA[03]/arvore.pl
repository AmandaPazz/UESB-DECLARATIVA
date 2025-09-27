inserir(Elemento, arvore(vazio,Elemento,vazio)).
inserir(Elemento, arvore(Esquerda,Valor,_)) :-
    Elemento < Valor,
    inserir(Elemento,Esquerda).
inserir(Elemento, arvore(_,Valor,Direita)) :-
    Elemento >= Valor,
    inserir(Elemento,Direita).


ehMembro(Elemento, arvore(_,Elemento,_)).
ehMembro(Elemento, arvore(Esquerdo,_,_)) :-
    ehMembro(Elemento,Esquerdo).
ehMembro(Elemento, arvore(_,_,Direita)) :-
    ehMembro(Elemento,Direita).
