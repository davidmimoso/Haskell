import Distribution.Fields.ConfVar (parseConditionConfVarFromClause)
import Data.List (sortOn, groupBy)

{-  (a) any :: (a -> Bool) -> [a] -> Bool que teste se um predicado ´e verdade para
algum elemento de uma lista; por exemplo:
any odd [1..10] == True
 -}

myany :: (a -> Bool) -> [a] -> Bool
myany test []=False
myany test (h:t)=test h || myany test t


{- (b) zipWith :: (a->b->c) -> [a] -> [b] -> [c] que combina os elementos de
duas listas usando uma fun¸c˜ao espec´ıfica; por exemplo:
zipWith (+) [1,2,3,4,5] [10,20,30,40] == [11,22,33,44].
 -}

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f [] _ = []          -- Se a 1ª lista acaba, para tudo.
myZipWith f _ [] = []          -- Se a 2ª lista acaba, para tudo.
myZipWith f (h:t) (x:xs) = f h x : myZipWith f t xs


{- (c) takeWhile :: (a->Bool) -> [a] -> [a] que determina os primeiros elementos
da lista que satisfazem um dado predicado; por exemplo:
takeWhile odd [1,3,4,5,6,6] == [1,3].
 -}

mytakeWhile :: (a->Bool) -> [a] -> [a]
mytakeWhile f [] = []
mytakeWhile f (h:t)=if f h then h: mytakeWhile f t 
                    else []



{- (d) dropWhile :: (a->Bool) -> [a] -> [a] que elimina os primeiros elementos da
lista que satisfazem um dado predicado; por exemplo:
dropWhile odd [1,3,4,5,6,6] == [4,5,6,6].

 -}

mydropWhile :: (a->Bool) -> [a] -> [a]
mydropWhile f [] = []
mydropWhile f (h:t)=if f h then mydropWhile f t else (h:t)

{- (e) span :: (a-> Bool) -> [a] -> ([a],[a]), que calcula simultaneamente os dois
resultados anteriores. Note que apesar de poder ser definida `a custa das outras
duas, usando a defini¸c˜ao
span p l = (takeWhile p l, dropWhile p l)
nessa defini¸c˜ao h´a trabalho redundante que pode ser evitado. Apresente uma
defini¸c˜ao alternativa onde n˜ao haja duplica¸c˜ao de trabalho
 -}

myspan :: (a-> Bool) -> [a] -> ([a],[a])
myspan f [] =([],[])
myspan f (h:t) 
          | f h = let (ta,d)=myspan f t
                  in (h:ta,d)         
          |otherwise =([],(h:t))


{- deleteBy :: (a -> a -> Bool) -> a -> [a] -> [a] que apaga o primeiro el￾emento de uma lista que ´e “igual” a um dado elemento de acordo com a fun¸c˜ao
de compara¸c˜ao que ´e passada como parˆametro. Por exemplo:
deleteBy (\x y -> snd x == snd y) (1,2) [(3,3),(2,2),(4,2)]
 -}

mydeleteBy :: (a -> a -> Bool) -> a -> [a] -> [a]
mydeleteBy _ x [] = [] 
mydeleteBy cd x (h:t) 
                   |cd x h = t
                   |otherwise = h: mydeleteBy cd x t



{- (g) sortOn :: Ord b => (a -> b) -> [a] -> [a] que ordena uma lista compara￾ndo os resultados de aplicar uma fun¸c˜ao de extrac¸c˜ao de uma chave a cada ele￾mento de uma lista. Por exemplo:
sortOn fst [(3,1),(1,2),(2,5)] == [(1,2),(2,5),(3,1)]
 -}

mysortOn :: Ord b => (a -> b) -> [a] -> [a]
mysortOn cond [] = []
mysortOn cond (x:xs) = 
    let 
        -- Alinha o início destas duas linhas perfeitamente!
        menores = [y | y <- xs, cond y < cond x]      -- y é menor que x
        maiores = [y | y <- xs, cond y >= cond x]     -- y é maior ou IGUAL a x
    in 
        mysortOn cond menores ++ [x] ++ mysortOn cond maiores


{- 2. Relembre a quest˜ao sobre polin´omios introduzida na Ficha 3, onde um polin´omio era
representado por uma lista de mon´omios representados por pares (coeficiente, expoente)
type Polinomio = [Monomio]
type Monomio = (Float,Int)
Por exemplo, [(2,3), (3,4), (5,3), (4,5)] representa o polin´omio 2x^3+3x^4+5x^3+4x^5
. Redefina as fun¸c˜oes pedidas nessa ficha, usando agora fun¸c˜oes de ordem
superior (definidas no Prelude ou no Data.List) em vez de recursividade expl´ıcita:

(a) selgrau :: Int -> Polinomio -> Polinomio que selecciona os mon´omios com
um dado grau de um polin´omio
 -}


type Polinomio = [Monomio]
type Monomio = (Float,Int)


selgrau :: Int -> Polinomio -> Polinomio
selgrau n p = filter (\m -> snd m == n) p


{- (b) conta :: Int -> Polinomio -> Int de forma a que (conta n p) indica quan￾tos mon´omios de grau n existem em p. -}


conta :: Int -> Polinomio -> Int
conta x l = length (selgrau x l )


--grau :: Polinomio -> Int que indica o grau de um polin´omio.

grau :: Polinomio -> Int
grau l = maximum(map (snd) l )


--(d) deriv :: Polinomio -> Polinomio que calcula a derivada de um polin´omio.

deriv :: Polinomio -> Polinomio
deriv pol=map(\(coef,expo) -> if expo >0 then (coef*fromIntegral (expo),expo-1)else (0,0)) pol

{- (e) calcula :: Float -> Polinomio -> Float que calcula o valor de um polin´omio
para uma dado valor de x
 -}


calcula :: Float -> Polinomio -> Float
calcula x l =sum (map(\(coef,expo)-> (coef*(x)^expo)) l)


{- (f) simp :: Polinomio -> Polinomio que retira de um polin´omio os mon´omios de
coeficiente zero. -}

simp :: Polinomio -> Polinomio
simp l =filter (\(coef,expo)->expo/=0)l


--(g) mult :: Monomio -> Polinomio -> Polinomio que calcula o resultado da mul￾tiplica¸c˜ao de um mon´omio por um polin´omio.

mult :: Monomio -> Polinomio -> Polinomio
mult (coefm, expom) p = map (\(coef, expo) -> (coefm * coef, expom + expo)) p



--(h) ordena :: Polinomio -> Polinomio que ordena um polon´omio por ordem cres￾cente dos graus dos seus mon´omios.

ordena :: Polinomio -> Polinomio
ordena p = sortOn snd p



{- (i) normaliza :: Polinomio -> Polinomio que dado um polin´omio constr´oi um
polin´omio equivalente em que n˜ao podem aparecer varios mon´omios com o mesmo
grau -}

normaliza :: Polinomio -> Polinomio
normaliza l= map somacoefs (groupBy(\ x y -> snd x == snd y)(ordena l))

somacoefs:: Polinomio->Monomio
somacoefs l = (sum (map fst l),snd (head l)) 

{- (j) soma :: Polinomio -> Polinomio -> Polinomio que faz a soma de dois polin´omios
de forma que se os polin´omios que recebe estiverem normalizados produz tamb´em
um polin´omio normalizado.
 -}

soma :: Polinomio -> Polinomio -> Polinomio
soma p l =normaliza (p ++ l)

{- (k) produto :: Polinomio -> Polinomio -> Polinomio que calcula o produto de
dois polin´omios -}

produto :: Polinomio -> Polinomio -> Polinomio
produto p l = normaliza(concatMap(\ x -> mult x l )p )  


{- (l) equiv :: Polinomio -> Polinomio -> Bool que testa se dois polin´omios s˜ao
equivalentes. -}


equiv :: Polinomio -> Polinomio -> Bool
equiv p1 p2 = normaliza p1 ==  normaliza p2

{-  VERSÃO DO GABS 
equiv :: Polinomio -> Polinomio -> Bool
equiv pol1 pol2 = filter (\(coef, expo) -> coef /= 0) (ordena (normaliza pol1))
                == filter (\(coef, expo) -> coef /= 0) (ordena (normaliza pol2))
 -}

type Mat a = [[a]]
{- 
(a) dimOK :: Mat a -> Bool que testa se uma matriz est´a bem constru´ıda (i.e., se
todas as linhas tˆem a mesma dimens˜ao). -}

dimOK :: Mat a -> Bool
dimOK []=True
dimOK [x] = True
dimOK (h:y:t)
             | length h == length y = dimOK (y:t)
             |otherwise= False 

--(b) dimMat :: Mat a -> (Int,Int) que calcula a dimens˜ao de uma matriz.

dimMat :: Mat a -> (Int,Int)
dimMat []=(0,0)
dimMat t= let 
              l=length t
              c=length(head t)
          in (l,c)


--(c) addMat :: Num a => Mat a -> Mat a -> Mat a que adiciona duas matrizes

addMat :: Num a => Mat a -> Mat a -> Mat a
addMat (a1:a2) (m1:m2) = (zipWith (+) a1 m1) : addMat a2 m2 

--(d) transpose :: Mat a -> Mat a que calcula a transposta de uma matriz

transpose :: Mat a -> Mat a
transpose ([]:_) = []
transpose m = map head m : transpose (map tail m )

--(e) multMat :: Num a => Mat a -> Mat a -> Mat a que calcula o produto de duas
--matrizes.


multMat :: Num a => Mat a -> Mat a -> Mat a
multMat [] _ =[]
multMat m1 m2 =let mt2=transpose m2 
               in map(\linha -> map(\coluna -> sum (zipWith (*) linha coluna ))mt2)m1


{- (f) zipWMat :: (a -> b -> c) -> Mat a -> Mat b -> Mat c que, `a semelhan¸ca
do que acontece com a fun¸c˜ao zipWith, combina duas matrizes. Use essa fun¸c˜ao
para definir uma fun¸c˜ao que adiciona duas matrizes.
 -}

zipWMat :: (a -> b -> c) -> Mat a -> Mat b -> Mat c
zipWMat f m1 m2 = zipWith(zipWith (f)) m1 m2 


--(g) triSup :: Num a => Mat a -> Bool que testa se uma matriz quadrada ´e trian￾gular superior (i.e., todos os elementos abaixo da diagonal s˜ao nulos).


triSup ::(Num a, Eq a) => Mat a -> Bool
triSup m1 = auxiliar 0 m1


auxiliar::(Num a, Eq a)=> Int-> Mat a -> Bool
auxiliar n [] = True
auxiliar n (h:t)= if verificaZeros n h then auxiliar (n+1) t 
                  else False

verificaZeros::(Num a, Eq a) => Int -> [a] -> Bool
verificaZeros 0 _=True
verificaZeros _ []=False
verificaZeros n (h:t) = if h ==0  then verificaZeros (n-1) t
                        else  False 



{- --[[1,2,3], [0,4,5], [0,0,6]]  --> [[1,0,0],[2,4,0],[3,5,6]]
versao do gabs 
-- g) Testa se uma matriz quadrada é triangular superior

-- O "Eq a" define que os elementos "a" são comparáveis, uma vez que os operadores "==" e "/=" estão associados a operações numéricas
triSup :: (Eq a, Num a) => Mat a -> Bool
triSup [] = True
triSup (h:t) = all (\x -> x == 0) col && triSup (map tail t)
            where col = map head t -}

{- (h) rotateLeft :: Mat a -> Mat a que roda uma matriz 90o para a esquerda. Por
exemplo, o resultado de rodar a matriz acima apresentada deve corresponder `a
matriz


3 5 6
2 4 0
1 0 0

 -}


rotateLeft :: Mat a -> Mat a
rotateLeft ([]:_) = [] 
rotateLeft m = map last m : rotateLeft (map init m )
       
