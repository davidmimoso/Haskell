{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
data ExpInt = Const Int
            | Simetrico ExpInt
            | Mais ExpInt ExpInt
            | Menos ExpInt ExpInt
            | Mult ExpInt ExpInt


calcula :: ExpInt -> Int
calcula (Const x)      = x
calcula (Simetrico e)  = - (calcula e)
calcula (Mais e1 e2)   = (calcula e1) + (calcula e2)
calcula (Menos e1 e2)  = (calcula e1) - (calcula e2)
calcula (Mult e1 e2)   = (calcula e1) * (calcula e2)

infixa :: ExpInt -> String
infixa (Const x)       = show x
infixa (Simetrico e)   = "-(" ++ infixa e ++ ")"
infixa (Mais e1 e2 )   = "(" ++ infixa e1 ++ "+" ++ infixa e2 ++ ")"
infixa (Menos e1 e2)   = "(" ++ infixa e1 ++ "-" ++ infixa e2 ++ ")"
infixa (Mult e1 e2 )   = "(" ++ infixa e1 ++ "*" ++ infixa e2 ++ ")"


posfixa :: ExpInt -> String
posfixa (Const x)     = show x
posfixa (Simetrico e) = posfixa e ++ " -"             -- O sinal vem no fim
posfixa (Mais e1 e2)  = posfixa e1 ++ " " ++ posfixa e2 ++ " +"
posfixa (Menos e1 e2) = posfixa e1 ++ " " ++ posfixa e2 ++ " -"
posfixa (Mult e1 e2)  = posfixa e1 ++ " " ++ posfixa e2 ++ " *"

data RTree a = R a [RTree a]

soma :: Num a => RTree a -> a
soma (R a filhos) = a + sum (map soma filhos)


altura :: RTree a -> Int
altura (R _ [] )= 1
altura (R a filhos) = 1 + maximum (map altura filhos)



prune :: Int -> RTree a -> RTree a
prune x (R a [])=(R a [] )
prune 0 (R a _)= (R a [])
prune x (R a filhos)= R a (map (prune (x-1)) filhos)


mirror :: RTree a -> RTree a
mirror (R a filhos) = (R a filhos)
mirror (R a filhos) = R a (reverse (map mirror filhos))


postorder :: RTree a -> [a]
postorder (R a filhos)= concat (map (postorder) filhos) ++ [a]

data BTree a = Empty | Node a (BTree a) (BTree a)
data LTree a = Tip a | Fork (LTree a) (LTree a)



ltSum :: Num a => LTree a -> a
ltSum (Tip x) = x
ltSum (Fork e d)= ltSum ( e) + ltSum d


listaLT :: LTree a -> [a]
listaLT (Tip x)    = [x]
listaLT (Fork e d) = listaLT e ++ listaLT d

ltHeight :: LTree a -> Int
ltHeight (Tip _) = 1
ltHeight (Fork e d) =1 + max (ltHeight e)  (ltHeight d)


data FTree a b = Leaf b | No a (FTree a b) (FTree a b)


splitFTree :: FTree a b -> (BTree a, LTree b)
splitFTree (Leaf b) = (Empty,Tip b)
splitFTree (No a e d) = (Node a (maise) (maisd) ,Fork (restoe) restod )
                     where
                        (maise,restoe)=splitFTree e
                        (maisd,restod)=splitFTree d



joinTrees :: BTree a -> LTree b -> Maybe (FTree a b)

-- CASO 1: As duas acabam ao mesmo tempo. Sucesso!
joinTrees Empty (Tip b) = Just (Leaf b )

-- CASO 2: As duas continuam ao mesmo tempo. Temos de ver os filhos.
joinTrees (Node a be bd) (Fork le ld) =
    case (joinTrees be le, joinTrees bd ld) of
        (Just esq, Just dir) -> Just ( No a esq dir ) -- As duas filhas encaixaram! Constrói o No.
        _                    -> Nothing      -- Uma delas falhou, logo tudo falha.

-- CASO 3: Formas incompatíveis (tudo o resto)
joinTrees _ _ = Nothing
