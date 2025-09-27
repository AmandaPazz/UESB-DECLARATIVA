principal :-
  see('arq.pl'),
  write('Digite o nome do pai: '),
  read(Pai),
  achaPai(Pai),
  see(user).

achaPai(Pai) :- read(Termo),
  acha_primeiro(Termo, Pai).

acha_primeiro(end_of_file,_) :- !.
acha_primeiro(pais(Pai,Filho,Mae),Pai) :-
  write(pais(Pai,Filho,Mae)),
  write('.'),nl,
  achaPai(Pai).
acha_primeiro(_, Pai) :- achaPai(Pai).
