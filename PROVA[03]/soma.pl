soma(A,B,R) :- R is A+B.

main :- write('Digite um numero: '),
    read(A), nl,
    write('Digite outro numero: '),
    read(B),nl,
    write('Resultado: '),
    soma(A,B,R),
    write(R).
