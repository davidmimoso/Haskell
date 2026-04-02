-- T1: A árvore vazia (Caso base)
t1 :: BTree Int
t1 = Empty

-- T2: A árvore com apenas a raiz (Caso base do nó)
t2 :: BTree Int
t2 = Node 10 Empty Empty

-- T3: Uma árvore "torta" só para a direita (parece uma lista)
-- 1 -> 2 -> 3
t3 :: BTree Int
t3 = Node 1 Empty (Node 2 Empty (Node 3 Empty Empty))

-- T4: Uma árvore equilibrada (Pirâmide perfeita)
--      5
--     / \
--    2   8
t4 :: BTree Int
t4 = Node 5 (Node 2 Empty Empty) (Node 8 Empty Empty)

-- T5: Uma árvore maior e misturada
--       10
--      /  \
--     5    15
--         /
--        12
t5 :: BTree Int
t5 = Node 10 (Node 5 Empty Empty) (Node 15 (Node 12 Empty Empty) Empty)



data BTree a = Empty
              | Node a (BTree a) (BTree a)
    deriving Show


--(a) altura :: BTree a -> Int que calcula a altura da ´arvore.



altura :: BTree a -> Int
altura Empty =0
altura (Node r e d)= 1 + max (altura e) (altura d)


-- (b) contaNodos :: BTree a -> Int que calcula o n´umero de nodos da ´arvore


contaNodos :: BTree a -> Int
contaNodos Empty = 0
contaNodos (Node r e d)= 1 + contaNodos e + contaNodos d


--(c) folhas :: BTree a -> Int, que calcula o n´umero de folhas (i.e., nodos sem de￾scendentes) da ´arvore.

folhas :: BTree a -> Int
folhas Empty = 0
folhas (Node r Empty Empty)= 1
folhas(Node r e d )= folhas e + folhas d



--(d) prune :: Int -> BTree a -> BTree a, que remove de uma ´arvore todos os el￾ementos a partir de uma determinada profundidade.


prune :: Int -> BTree a -> BTree a
prune _ Empty = Empty
prune 0  x= x
prune x (Node r e d)= Node r (prune (x-1) e) (prune (x-1) d)


{- (e) path :: [Bool] -> BTree a -> [a], que dado um caminho (False corresponde
a esquerda e True a direita) e uma ´arvore, d´a a lista com a informa¸c˜ao dos nodos
por onde esse caminho passa. -}


path :: [Bool] -> BTree a -> [a]
path a Empty =[]
path [] a = []
path (h:t) (Node r e d)= if h == True then r: (path t d)
                         else r: (path t e)

--(f) mirror :: BTree a -> BTree a, que d´a a ´arvore sim´etrica.


mirror :: BTree a -> BTree a
mirror Empty = Empty
mirror (Node r e d) = Node r (mirror d) (mirror e)

--(g) zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c que gener￾aliza a fun¸c˜ao zipWith para ´arvores bin´arias.

zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT _ Empty _ = Empty
zipWithBT _ _ Empty =Empty
zipWithBT f (Node r e d) (Node r1 e1 d1)= Node ( f r r1) (zipWithBT f e e1) (zipWithBT f d d1)



{- 
(h) unzipBT :: BTree (a,b,c) -> (BTree a,BTree b,BTree c), que generaliza a
fun¸c˜ao unzip (neste caso de triplos) para ´arvores b
 -}


unzipBT :: BTree (a,b,c) -> (BTree a,BTree b,BTree c)
unzipBT Empty = (Empty,Empty,Empty)
unzipBT (Node (a,b,c) e d)= ((Node a ( prie) (prid)),(Node b (snde) (sndd)), (Node c (trie) (trid ) ))
                           where
                           (prie,snde,trie)= unzipBT e
                           (prid,sndd,trid)=unzipBT d



{- 2. Defina as seguintes fun¸c˜oes, assumindo agora que as ´arvores s˜ao bin´arias de procura:


(a) Defina uma fun¸c˜ao minimo :: Ord a => BTree a -> a que determina o menor
elemento de uma ´arvore bin´aria de procura n˜ao vazia.
 -}

minimo :: Ord a => BTree a -> a
minimo (Node r Empty _)=r
minimo (Node r e d)= minimo e


{- (b) Defina uma fun¸c˜ao semMinimo :: Ord a => BTree a -> BTree a que remove o
menor elemento de uma ´arvore bin´aria de procura n˜ao vazia -}

semMinimo :: Ord a => BTree a -> BTree a
semMinimo Empty = Empty
semMinimo (Node r Empty d)=d
semMinimo (Node r e d)= Node r (semMinimo e) d


{- (c) Defina uma fun¸c˜ao minSmin :: Ord a => BTree a -> (a,BTree a) que calcula,
com uma ´unica travessia da ´arvore o resultado das duas fun¸c˜oes anteriores.
 -}

minSmin :: Ord a => BTree a -> (a, BTree a)
minSmin (Node r Empty d) = (r, d)    -- O teu caso base está perfeito!
minSmin (Node r e d) = (minE, Node r newE d)
    where (minE, newE) = minSmin e


{- 
(d) Defina uma fun¸c˜ao remove :: Ord a => a -> BTree a -> BTree a que remove
um elemento de uma ´arvore bin´aria de procura, usando a fun¸c˜ao anterior.
 -}

remove :: Ord a => a -> BTree a -> BTree a
remove _ Empty = Empty
remove x (Node r e d)
    | x < r = Node r (remove x e) d      -- Correto: reconstrói descendo à esquerda
    | x > r = Node r e (remove x d)      -- Corrigido: reconstrói descendo à direita
    | otherwise = case d of              -- Estamos no caso x == r
        Empty -> e                       -- Se não há direita, o substituto é a esquerda
        _     -> let (m, semd) = minSmin d
                 in Node m e semd



type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL deriving Show
data Classificacao = Aprov Int
                    | Rep
                    | Faltou
   deriving Show
type Turma = BTree Aluno -- ´arvore bin´aria de procura (ordenada por n´umero)


{- (a) inscNum :: Numero -> Turma -> Bool, que verifica se um aluno, com um dado
n´umero, est´a inscrito.
 -}


inscNum :: Numero -> Turma -> Bool
inscNum _ Empty = False
inscNum x ( Node (a ,_,_,_) e d )
                                     | x == a = True
                                     |x > a = inscNum x d
                                     | otherwise = inscNum x e


{- (b) inscNome :: Nome -> Turma -> Bool, que verifica se um aluno, com um dado
nome, est´a inscrito. -}


inscNome :: Nome -> Turma -> Bool
inscNome _ Empty=False
inscNome x ( Node (_,a,_,_) e d )
                                | x == a = True
                                | otherwise = inscNome x e || inscNome x d



{- (c) trabEst :: Turma -> [(Numero,Nome)], que lista o n´umero e nome dos alunos
trabalhadores-estudantes (ordenados por n´umero). -}

trabEst :: Turma -> [(Numero,Nome)]
trabEst Empty = []
trabEst (Node (num,name,TE,_) l r) = trabEst l ++ [(num,name)] ++ trabEst r
trabEst (Node (num,name,_,_) l r) = trabEst l ++ trabEst r



{- (d) nota :: Numero -> Turma -> Maybe Classificacao, que calcula a classifica¸c˜ao
de um aluno (se o aluno n˜ao estiver inscrito a fun¸c˜ao deve retornar Nothing). -}


nota :: Numero -> Turma -> Maybe Classificacao
nota x Empty = Nothing
nota x ( Node (a ,_,_,c) e d )
                             |x == a = Just c
                             |x<a = nota x e
                             |otherwise= nota x d


--(e) percFaltas :: Turma -> Float, que calcula a percentagem de alunos que fal￾taram `a avalia¸c˜ao.



contaFaltas :: Turma -> Int
contaFaltas Empty=0
contaFaltas ( Node (_,_,_,Faltou) e d )=1 + contaFaltas e+ contaFaltas d
contaFaltas (Node (_) e d ) =contaFaltas e+ contaFaltas d


percFaltas :: Turma -> Float
percFaltas Empty=0
percFaltas (Node r e d) = (fromIntegral faltas/ fromIntegral total) *100
                       where
                        faltas = contaFaltas (Node r e d)
                        total = contaNodos (Node r e d)

{- (f) mediaAprov :: Turma -> Float, que calcula a m´edia das notas dos alunos que
passaram. -}


-- 1. Somar apenas as notas positivas (Corrigido para não crashar)
somaNotas :: Turma -> Int 
somaNotas Empty = 0
somaNotas (Node (_,_,_,Aprov x) e d) = x + somaNotas e + somaNotas d
somaNotas (Node _ e d) = somaNotas e + somaNotas d -- Ignora Rep e Faltou

-- 2. Contar apenas os alunos aprovados (Nova função auxiliar)
contaAprov :: Turma -> Int
contaAprov Empty = 0
contaAprov (Node (_,_,_,Aprov _) e d) = 1 + contaAprov e + contaAprov d
contaAprov (Node _ e d) = contaAprov e + contaAprov d

-- 3. Calcular a média
mediaAprov :: Turma -> Float
mediaAprov turma = if total == 0 then 0 else (notas / total)
    where 
        notas = fromIntegral (somaNotas turma)
        total = fromIntegral (contaAprov turma) -- Usamos contaAprov aqui!
        
--(g) aprovAv :: Turma -> Float, que calcula o r´acio de alunos aprovados por avali￾ados. Implemente esta fun¸c˜ao fazendo apenas uma travessia da ´arvore.
aprovAv :: Turma -> Float
aprovAv t = aprov / aval
            where (aprov, aval) = aprovAvAux t

aprovAvAux :: Turma -> (Float, Float)
aprovAvAux Empty = (0,0)
aprovAvAux (Node (_,_,_,Aprov _) l r) = (1 + aprovL + aprovR, 1 + avalL + avalR)
                            where (aprovL, avalL) = aprovAvAux l
                                  (aprovR, avalR) = aprovAvAux r
aprovAvAux (Node (_,_,_,Rep) l r)     = (aprovL + aprovR, 1 + avalL + avalR)
                            where (aprovL, avalL) = aprovAvAux l
                                  (aprovR, avalR) = aprovAvAux r
aprovAvAux (Node (_,_,_,Faltou) l r)  = (aprovL + aprovR, avalL + avalR)
                            where (aprovL, avalL) = aprovAvAux l
                                  (aprovR, avalR) = aprovAvAux r
-- não olhei nem fiz esta ultima
















