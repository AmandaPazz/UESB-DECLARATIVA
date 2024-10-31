{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

{-
module Stack (Stack, push, pop, top, emptyStack, stackEmpty) where

versão 1:
-- Definindo o tipo de dado Stack
data Stack a = EmptyStk | Stk a (Stack a) deriving (Show)

-- Função que cria uma pilha vazia
emptyStack :: Stack a
emptyStack = EmptyStk

-- Função que verifica se uma pilha está vazia
stackEmpty :: Stack a -> Bool
stackEmpty EmptyStk = True
stackEmpty _        = False

-- Função que insere um elemento na pilha
push :: a -> Stack a -> Stack a
push = Stk

-- Função que remove o elemento do topo da pilha
pop :: Stack a -> Stack a
pop EmptyStk   = error "pop from an empty stack"
pop (Stk _ s) = s

-- Função que retorna o elemento do topo da pilha sem removê-lo
top :: Stack a -> a
top EmptyStk   = error "top from an empty stack"
top (Stk x _) = x

pilha :: Stack Int
pilha = Stk 1 (Stk 2 (Stk 3 EmptyStk))

data Stack a = Stk [a] deriving (Show)

emptyStack :: Stack a
emptyStack = Stk []

stackEmpty :: Stack a -> Bool
stackEmpty (Stk []) = True
stackEmpty _        = False

push :: a -> Stack a -> Stack a
push x (Stk xs) = Stk (x:xs)

pop :: Stack a -> Stack a
pop (Stk [])    = error "pop from an empty stack"
pop (Stk (_:xs)) = Stk xs

top :: Stack a -> a
top (Stk [])   = error "top from an empty stack"
top (Stk (x:_)) = x

pilhaV2 :: Stack Int
pilhaV2 = push 1 (push 2 (push 3 emptyStack))

pilhaV3 :: Stack Int
pilhaV3 = Stk [1,2,3,4]

module Queue (Queue, emptyQueue, queueEmpty, enqueue, dequeue, front) where

data Queue a = Q [a] deriving Show

emptyQueue :: Queue a
emptyQueue = Q []

queueEmpty :: Queue a -> Bool
queueEmpty (Q []) = True
queueEmpty (Q _ ) = False

enqueue :: a -> Queue a -> Queue a
enqueue x (Q q) = Q (q ++ [x])

dequeue :: Queue a -> Queue a
dequeue (Q (_:xs)) = Q xs
dequeue (Q []) = error "dequeue: empty queue"

front :: Queue a -> a
front (Q (x:_)) = x
front (Q []) = error "front: empty queue"

fila :: Queue Int
fila = Q [1,2,3,4,5]

fila2 :: Queue String
fila2 = enqueue "paz" (enqueue "amanda" emptyQueue)
-}
data BinTree a = EmptyBT | NodeBT a (BinTree a) (BinTree a) deriving (Show)

emptyTree :: BinTree a
emptyTree = EmptyBT

inTree :: (Ord a) => a -> BinTree a -> Bool
inTree v' EmptyBT = False
inTree v' (NodeBT v lf rt)
  | v' == v = True
  | v' < v = inTree v' lf
  | otherwise = inTree v' rt

addTree :: (Ord a) => a -> BinTree a -> BinTree a
addTree v' EmptyBT = NodeBT v' EmptyBT EmptyBT
addTree v' (NodeBT v lf rt)
  | v' == v = NodeBT v lf rt
  | v' < v = NodeBT v (addTree v' lf) rt
  | otherwise = NodeBT v lf (addTree v' rt)

delTree :: (Ord a) => a -> BinTree a -> BinTree a
delTree _ EmptyBT = EmptyBT
delTree v' (NodeBT v lf EmptyBT)
  | v' == v = lf
delTree v' (NodeBT v EmptyBT rt)
  | v' == v = rt
delTree v' (NodeBT v lf rt)
  | v' < v = NodeBT v (delTree v' lf) rt
  | v' > v = NodeBT v lf (delTree v' rt)
  | v' == v =
      let k = minTree rt
       in NodeBT k lf (delTree k rt)

minTree :: BinTree a -> a
minTree (NodeBT v EmptyBT _) = v
minTree (NodeBT _ lf _) = minTree lf

inorder :: BinTree a -> [a]
inorder EmptyBT = []
inorder (NodeBT v lf rt) = inorder lf ++ [v] ++ inorder rt

arvore :: BinTree Int
arvore = NodeBT 10 EmptyBT EmptyBT

main :: IO ()
main = do
  putStrLn $ "Testando: " ++ show ([(\x -> x *2 + 1) x | x <- [1..10], (\x -> mod x 3 == 0) x])