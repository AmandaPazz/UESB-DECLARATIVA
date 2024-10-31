{-
Baseado nas definições do cálculo lambda, compute:
True = (λt.λf.t)
False = (λt.λf.f)
AND = (λp.λq.p q p)
OR = (λp.λq.p p q)
NOT =(λp.p False True)
IF = (λp.λa.λb.p a b)
XOR = (λp.λq.p (NOT q) q)


a) AND True True
(λp.λq.p q p) TRUE TRUE
(λq.TRUE q TRUE) TRUE
TRUE TRUE TRUE 
(λt.λf.t) TRUE TRUE 
TRUE


b) AND True False
(λp.λq.p q p) TRUE FALSE
(λq.TRUE q TRUE) FALSE
TRUE FALSE TRUE
(λt.λf.t) FALSE TRUE
(λf.FALSE) TRUE
FALSE




b) AND True False
(λp.λq.p q p) TRUE FALSE
(λq.TRUE q TRUE) FALSE
TRUE FALSE TRUE 
(λt.λf.t) FALSE TRUE
(λf. FALSE) TRUE
FALSE



c) AND False True
(λp.λq.p q p )FALSE TRUE
(λf.FALSE) TRUE
FALSE




d) AND False False
(λp.λq.p q p) FALSE FALSE
(λq.FALSE q FALSE) FALSE
FALSE FALSE FALSE
(λt.λf.f) FALSE FALSE
FALSE





e) OR True True
(λp.λq.p p q) TRUE TRUE
(λq.TRUE TRUE q) TRUE
TRUE TRUE TRUE
(λt.λf.t) TRUE TRUE
TRUE


f) OR True False
(λp.λq.p p q) TRUE FALSE
(λq.TRUE TRUE q) FALSE
TRUE TRUE FALSE
(λt.λf.t) TRUE FALSE
TRUE



g) OR False True
(λp.λq.p p q) FALSE TRUE
(λq.FALSE FALSE q) TRUE
FALSE FALSE TRUE
(λt.λf.f) FALSE TRUE
TRUE




h) OR False False
(λp.λq.p p q) FALSE FALSE
(λq.FALSE FALSE q) FALSE
FALSE FALSE FALSE
(λt.λf.f) FALSE FALSE
FALSE




i) NOT True
(λp.p False True) TRUE
TRUE FALSE TRUE
(λt.λf.t) FALSE TRUE
FALSE


j) NOT False
(λp.p False True) FALSE
FLASE FALSE TRUE
(λt.λf.F) TRUE
TRUE


l) IF True x y
(λp.λa.λb.p a b) TRUE X Y
(λa.λb.TRUE a b) X Y
(λb.TRUE X b) Y
TRUE X Y
(λt.λf.t) x y
x



m) IF False x y
(λp.λa.λb.p a b) FALSE X Y
(λa.λb.FALSE a b) X Y
(λb.FALSE X b) Y
(FALSE X Y)  
(λt.λf.f) X Y
Y



n) XOR True True
(λp.λq.p (NOT q) q) TRUE TRUE
TRUE (NOT TRUE) TRUE
TRUE ((λp.p False True) TRUE) TRUE
TRUE (TRUE FALSE TRUE) TRUE
TRUE ((λt.λf.t) FALSE TRUE) TRUE
TRUE FALSE TRUE
(λt.λf.t) FALSE TRUE
FALSE



o) XOR True False
(λp.λq.p (NOT q) q) TRUE FALSE
TRUE (NOT FALSE) FALSE
TRUE ((λp.p False True) FALSE) FALSE
TRUE (FALSE FALSE TRUE) FALSE
TRUE ((λt.λf.f) FALSE TRUE) FALSE
TRUE TRUE FALSE
( λt.λf.t) TRUE FALSE
TRUE


p) XOR False True
(λp.λq.p (NOT q) q) FALSE TRUE
(λq.FALSE (NOT q) q) TRUE
FALSE (NOT TRUE) TRUE
FALSE ((λp.p False True) TRUE) TRUE
FALSE (true False True) TRUE
FALSE((λt.λf.t) FALSE TRUE) TRUE
FALSE FALSE TRUE
(λt.λf.f) FALSE TRUE
(λf.f) TRUE
TRUE

q) XOR False False
(λp.λq.p (NOT q) q) FALSE FALSE
(λq.FALSE (NOT q) q) FALSE
(FALSE (NOT FALSE) FALSE)
FALSE TRUE FALSE
(λf.f) FALSE
FALSE




essa definição da função soma'' em Haskell usa a técnica de currying, 
onde uma função recebe um argumento e retorna uma nova função que espera
o próximo argumento. Vamos entender a definição passo a passo:

soma'' :: Int -> (Int -> Int)
soma'' x = \y -> x + y





-}

