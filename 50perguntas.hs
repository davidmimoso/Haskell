
                        {-     Programação Funcional
                            1º Ano – LCC/LEF/LEI
                                Questões -}



{- 1. Apresente uma defini¸c˜ao recursiva da fun¸c˜ao (pr´e-definida) enumFromTo :: Int -> Int ->
[Int] que constr´oi a lista dos n´umeros inteiros compreendidos entre dois limites.
Por exemplo, enumFromTo 1 5 corresponde `a lista [1,2,3,4,5]
 -}

enumFromTo_ :: Int -> Int ->[Int]
enumFromTo_ x y | x == y+1 = []
                | otherwise = x : enumFromTo_ (x+1) y

{- 2. Apresente uma defini¸c˜ao recursiva da fun¸c˜ao (pr´e-definida) enumFromThenTo :: Int -> Int
-> Int -> [Int] que constr´oi a lista dos n´umeros inteiros compreendidos entre dois limites
e espa¸cados de um valor constante.
Por exemplo, enumFromThenTo 1 3 10 corresponde `a lista [1,3,5,7,9].
 -}


enumFromThenTo_ :: Int -> Int-> Int -> [Int]
enumFromThenTo_ x y z
                   | x > z = []
                   |otherwise= x: enumFromThenTo_ y (y+w) z
                    where w=y-x


{- 3. Apresente uma defini¸c˜ao recursiva da fun¸c˜ao (pr´e-definida) (++) :: [a] -> [a] -> [a]
que concatena duas listas.
Por exemplo, (++) [1,2,3] [10,20,30] corresponde `a lista [1,2,3,10,20,30].
 -}

(+++) :: [a] -> [a] -> [a]
(+++) x []=x
(+++) a l = a ++ l


{- 4. Apresente uma defini¸c˜ao recursiva da fun¸c˜ao (pr´e-definida) (!!) :: [a] -> Int -> a que
dada uma lista e um inteiro, calcula o elemento da lista que se encontra nessa posi¸c˜ao (assume￾se que o primeiro elemento se encontra na posi¸c˜ao 0).
Por exemplo, (!!) [10,20,30] 1 corresponde a 20. -}

(!!!) :: [a] -> Int -> a
(!!!) [] x = error "nao existe"
(!!!) (h:t) x
              |x==0 = h
              |otherwise = (!!!) t (x-1)


{- 5. Apresente uma defini¸c˜ao recursiva da fun¸c˜ao (pr´e-definida) reverse :: [a] -> [a] que
dada uma lista calcula uma lista com os elementos dessa lista pela ordem inversa.
Por exemplo, reverse [10,20,30] corresponde a [30,20,10]. -}


reverse_ :: [a]-> [a]
reverse_ [] = []
reverse_ [x]=[x]
reverse_ (h:t)= last t : reverse_ (h:init t)


 {- reverse_ :: [a] -> [a]
reverse_ [] = []
reverse_ (h:t) = reverse_ t ++ [h] -}


{- 6. Apresente uma defini¸c˜ao recursiva da fun¸c˜ao (pr´e-definida) take :: Int -> [a] -> [a] que
dado um inteiro n e uma lista l calcula a lista com os (no m´aximo) n primeiros elementos de
l.
A lista resultado s´o ter´a menos de que n elementos se a lista l tiver menos do que n elementos.
Nesse caso a lista calculada ´e igual `a lista fornecida.
Por exemplo, take 2 [10,20,30] corresponde a [10,20]. -}


take_ :: Int -> [a]->[a]
take_ x [] = []
take_ x (h:t)
             |x>0 =h:take_ (x-1) t
             |otherwise=[]


 {- 7. Apresente uma defini¸c˜ao recursiva da fun¸c˜ao (pr´e-definida) drop :: Int -> [a] -> [a] que
dado um inteiro n e uma lista l calcula a lista sem os (no m´aximo) n primeiros elementos de
l.
Se a lista fornecida tiver n elementos ou menos, a lista resultante ser´a vazia.
Por exemplo, drop 2 [10,20,30] corresponde a [30]. -}


drop_::Int -> [a] -> [a]
drop_ x []=[]
drop_ x (h:t)
            |x==length (h:t) =[]
            |x==0= h:t
            |otherwise= drop_ (x-1) t

{- 8. Apresente uma defini¸c˜ao recursiva da fun¸c˜ao (pr´e-definida) zip :: [a] -> [b] -> [(a,b)]
const´oi uma lista de pares a partir de duas listas.
Por exemplo, zip [1,2,3] [10,20,30,40] corresponde a [(1,10),(2,20),(3,30)]. -}

zip_ :: [a] -> [b] -> [(a,b)]
zip_ [] _ = []
zip_ _ []= []
zip_ (h:t) (x:xs)= (h,x):zip_ t xs


{-   9. Apresente uma defini¸c˜ao recursiva da fun¸c˜ao (pr´e-definida) replicate :: Int -> a ->
[a] que dado um inteiro n e um elemento x const´oi uma lista com n elementos, todos iguais
a x.
Por exemplo, replicate 3 10 corresponde a [10,10,10].
 -}
replicate_ :: Int -> a ->[a]
replicate_ 0 a = []
replicate_ x a = a:replicate_ (x-1) a

{- 10. Apresente uma defini¸c˜ao recursiva da fun¸c˜ao (pr´e-definida) intersperse :: a -> [a] ->
[a] que dado um elemento e uma lista, constr´oi uma lista em que o elemento fornecido ´e
intercalado entre os elementos da lista fornecida.
Por exemplo, intersperce 1 [10,20,30] corresponde a [10,1,20,1,30]. -}


intersperse_ :: a -> [a] ->[a]
intersperse_ x [] = []
intersperse_ x [a]=[a]
intersperse_ x (h:t)= h:x:intersperse_ x t

{- 11. Apresente uma defini¸c˜ao recursiva da fun¸c˜ao (pr´e-definida) group :: Eq a => [a] -> [[a]] que
agrupa elementos iguais e consecutivos de uma lista.
Por exemplo, group [1,2,2,3,4,4,4,5,4] corresponde a [[1],[2,2],[3],[4,4,4],[5],[4]]. -}

--Feito pelo chat
group_ :: Eq a => [a] -> [[a]]
group_ [] = []
group_ (h:t) = (h : iguais) : group_ resto
    where
       -- Pega em todos da cauda (t) que sejam iguais à cabeça (h)
       iguais = takeWhile (== h) t

       -- O resto da lista para a próxima recursão são os que SOBRAM
       resto  = dropWhile (== h) t

{- 12. Apresente uma defini¸c˜ao recursiva da fun¸c˜ao (pr´e-definida) concat :: [[a]] -> [a] que
concatena as listas de uma lista.
Por exemplo, concat [[1],[2,2],[3],[4,4,4],[5],[4]] corresponde a [1,2,2,3,4,4,4,5,4]. -}

concat_ :: [[a]] -> [a]
concat_ [] = []                    -- Se não há mais listas, devolve vazio
concat_ (h:t) = h ++ concat_ t



{-  13. Apresente uma defini¸c˜ao recursiva da fun¸c˜ao (pr´e-definida) inits :: [a] -> [[a]] que
calcula a lista dos prefixos de uma lista.
Por exemplo, inits [11,21,13] corresponde a [[],[11],[11,21],[11,21,13]]. -}

inits_ :: [a] -> [[a]]
inits_ [] = [[]]
inits_ (h:t)= []: map (h:) (inits_ t)
            


{- 14. Apresente uma defini¸c˜ao recursiva da fun¸c˜ao (pr´e-definida) tails :: [a] -> [[a]] que
calcula a lista dos sufixos de uma lista.
Por exemplo, tails [1,2,3] corresponde a [[1,2,3],[2,3],[3],[]]. -}



tails_ :: [a] -> [[a]]
tails_ [] = [[]]
tails_ (h:t) = (h:t) : tails_ t


{- 15. Defina a fun¸c˜ao heads :: [[a]] -> [a] que recebe uma lista de listas e produz a lista com
o primeiro elemento de cada lista.
Por exemplo, heads [[2,3,4],[1,7],[],[8,5,3]] corresponde a [2,1,8].
 -}


heads :: [[a]] -> [a]
heads [[]] = []
heads [[x]] = [x]
heads [(h):t]= head [h] : heads [t]



{- 16. Defina a fun¸c˜ao total :: [[a]] -> Int que recebe uma lista de listas e conta o total de
elementos (de todas as listas)
Por exemplo, total [[2,3,4],[1,7],[],[8,5,3]] corresponde a 8 -}


total :: [[a]] -> Int
total [] = 0
total (h:t)=length h + total t

{- total :: [[a]] -> Int
total = foldr ((+) . length) 0 -}


{- 17. Defina a fun¸c˜ao fun :: [(a,b,c)] -> [(a,c)] que recebe uma lista de triplos e produz a
lista de pares com o primeiro e o terceiro elemento de cada triplo.
Por exemplo, fun [("rui",3,2), ("maria",5,2), ("ana",43,7)] corresponde a
[("rui",2), ("maria",2), ("ana",7)]. -}

fun :: [(a,b,c)] -> [(a,c)]
fun[]=[]
fun ((x,y,z):t)= (x ,z): fun t 

{- 18. Defina a fun¸c˜ao cola :: [(String,b,c)] -> String que recebe uma lista de triplos e con￾catena as strings que est˜ao na primeira componente dos triplos.
Por exemplo, cola [("rui",3,2), ("maria",5,2), ("ana",43,7)] corresponde a "ruimariaana".
 -}

cola :: [(String,b,c)] -> String
cola [] = " "
cola ((x,_,_): t)= x ++ cola t

{- 19. Defina a fun¸c˜ao idade :: Int -> Int -> [(String,Int)] -> [String] que recebe o ano,
a idade e uma lista de pares com o nome e o ano de nascimento de cada pessoa, e devolve a
listas de nomes das pessoas que nesse ano atingir˜ao ou j´a ultrapassaram a idade indicada.
Por exemplo, idade 2021 26 [("rui",1995), ("maria",2009), ("ana",1947)] corresponde
a ["rui","ana"]. -}


idade :: Int -> Int -> [(String,Int)] -> [String]
idade _ _ [] = []
idade x y ((n,a):t)
                   | (x-a)<=y = n: idade x y t
                   |otherwise= idade x y t


powerEnumFrom :: Int -> Int -> [Int]
powerEnumFrom n m = aux n m 0
      where 
        aux n m k 
           | k < m     = (n^k) : aux n m (k+1) -- Parêntesis obrigatórios aqui!
           | otherwise = []

isPrime :: Int -> Bool
isPrime n 
    | n < 2     = False      -- 0 e 1 não são primos
    | n == 2    = True       -- 2 é o único par primo
    | otherwise = auxPrime n 2

auxPrime :: Int -> Int -> Bool
auxPrime n m
    | m * m > n      = True             -- Já passámos a raiz quadrada? Então é Primo!
    | mod n m == 0   = False            -- Encontrámos um divisor? Então NÃO é primo.
    | otherwise      = auxPrime n (m+1) -- Não divide? Tenta o próximo número.

{- 22. Apresente uma defini¸c˜ao recursiva da fun¸c˜ao (pr´e-definida) isPrefixOf :: Eq a => [a]
-> [a] -> Bool que testa se uma lista ´e prefixo de outra.
Por exemplo, isPrefixOf [10,20] [10,20,30] corresponde a True enquanto que isPrefixOf
[10,30] [10,20,30] corresponde a False. -}



_isPrefixOf :: Eq a => [a]-> [a] -> Bool 
_isPrefixOf [] a = True
_isPrefixOf (h:t) (x:xs) 
                         |h==x = _isPrefixOf t xs 
                         |otherwise= False



{- 23. Apresente uma defini¸c˜ao recursiva da fun¸c˜ao (pr´e-definida) isSuffixOf :: Eq a => [a]
-> [a] -> Bool que testa se uma lista ´e sufixo de outra.
Por exemplo, isSuffixOf [20,30] [10,20,30] corresponde a True enquanto que isSuffixOf
[10,30] [10,20,30] corresponde a False.
 -}

_isSuffixOf :: Eq a => [a]-> [a] -> Bool
_isSuffixOf []  a =True
_isSuffixOf (t) (a) 
                        | last t == last a = _isSuffixOf (init t) (init a)
                        |otherwise = False   



{- 24. Apresente uma defini¸c˜ao recursiva da fun¸c˜ao (pr´e-definida) isSubsequenceOf :: Eq a =>
[a] -> [a] -> Bool que testa se os elementos de uma lista ocorrem noutra pela mesma
ordem relativa.
Por exemplo, isSubsequenceOf [20,40] [10,20,30,40] corresponde a True enquanto que
isSubsequenceOf [40,20] [10,20,30,40] corresponde a False. -}


_isSubsequenceOf :: Eq a =>[a] -> [a] -> Bool
_isSubsequenceOf [] a = True
_isSubsequenceOf a [] = False
_isSubsequenceOf (h:t) (x:xs)
                             |h== x = _isSubsequenceOf t xs 
                             |h /= x = _isSubsequenceOf (h:t) xs
                             |otherwise= False

{- 25. Apresente uma defini¸c˜ao recursiva da fun¸c˜ao (pr´e-definida) elemIndices :: Eq a => a ->
[a] -> [Int] que calcula a lista de posi¸c˜oes em que um dado elemento ocorre numa lista.
Por exemplo, elemIndices 3 [1,2,3,4,3,2,3,4,5] corresponde a [2,4,6]. -}



_elemIndices :: Eq a => a ->[a] -> [Int]
_elemIndices _ [] = error "Não ocorre na lista"
_elemIndices x l = auxelem x l 0

auxelem :: Eq a => a ->[a]->Int -> [Int]
auxelem x [] _=[]
auxelem x (h:t) y
                 |x==h = y: auxelem x t (y+1)
                 |otherwise= auxelem x t (y+1)


{- 26. Apresente uma defini¸c˜ao recursiva da fun¸c˜ao (pr´e-definida) nub :: Eq a => [a] -> [a] que
calcula uma lista com os mesmos elementos da recebida, sem repeti¸c˜oes.
Por exemplo, nub [1,2,1,2,3,1,2] corresponde a [1,2,3]. -}


_nub :: Eq a => [a] -> [a]
_nub []=[]
_nub (h:t) = h:_nub (auxnub h t )


auxnub :: Eq a => a -> [a] -> [a]
auxnub _ [] =[]
auxnub x (h:t)
               |x== h =  auxnub x t
               |otherwise= h:auxnub x t  

{- _nub :: Eq a => [a] -> [a]
_nub l = foldr (\x acc -> if x `elem` acc then acc else x:acc) [] l


_nub :: Eq a => [a] -> [a]
_nub [] = []
_nub (x:xs) = x : _nub (filter (/= x) xs)
 -}

{- 27. Apresente uma defini¸c˜ao recursiva da fun¸c˜ao (pr´e-definida) delete :: Eq a => a -> [a]
-> [a] que retorna a lista resultante de remover (a primeira ocorrˆencia de) um dado elemento
de uma lista.
Por exemplo, delete 2 [1,2,1,2,3,1,2] corresponde a [1,1,2,3,1,2]. Se n˜ao existir nen￾huma ocorrˆencia a fun¸c˜ao dever´a retornar a lista recebida.
 -}

delete :: Eq a => a -> [a]-> [a] 
delete x (h:t) 
              |x== h = t 
              |otherwise = h: delete x t


{- 28. Apresente uma defini¸c˜ao recursiva da fun¸c˜ao (pr´e-definida) (\\):: Eq a => [a] -> [a]
-> [a] que retorna a lista resultante de remover (as primeiras ocorrˆencias) dos elementos da
segunda lista da primeira.
Por exemplo, (\\)[1,2,3,4,5,1] [1,5] corresponde a [2,3,4,1]. -}


(\\):: Eq a => [a] -> [a]-> [a]
(\\) [] x= [] 
(\\) x [] = x 
(\\) (h:t) (x:xs)
                  | x== h = (\\) t xs 
                  |otherwise= h:(\\) t (x:xs)  


{- 29. Apresente uma defini¸c˜ao recursiva da fun¸c˜ao (pr´e-definida) union :: Eq a => [a] -> [a]
-> [a] que retorna a lista resultante de acrescentar `a primeira lista os elementos da segunda
que n˜ao ocorrem na primeira.
Por exemplo, union [1,1,2,3,4] [1,5] corresponde a [1,1,2,3,4,5]. -}


_union :: Eq a => [a] -> [a] -> [a]
_union l [] = l         -- Se a segunda lista acabou, o resultado é a primeira (que foi acumulando)
_union l (h:t)
    | existe h l = _union l t          -- Já existe? Ignora o 'h' e continua com a mesma lista 'l'
    | otherwise  = _union (l ++ [h]) t -- É novo? Junta ao FIM de 'l' e continua

existe :: Eq a => a -> [a] -> Bool
existe _ [] = False
existe x (h:t) 
               |x==h = True
               |otherwise= existe x t


{-30. Apresente uma defini¸c˜ao recursiva da fun¸c˜ao (pr´e-definida) intersect :: Eq a => [a] ->
[a] -> [a] que retorna a lista resultante de remover da primeira lista os elementos que n˜ao
pertencem `a segunda.
Por exemplo, intersect [1,1,2,3,4] [1,3,5] corresponde a [1,1,3].  -}



_intersect :: Eq a => [a] ->[a] -> [a]
_intersect []  l  = [] 
_intersect (h:t) l
    | existe h l = h:(_intersect l t)         
    | otherwise  = _intersect (l) t



{- 31. Apresente uma defini¸c˜ao recursiva da fun¸c˜ao (pr´e-definida) insert :: Ord a => a -> [a]
-> [a] que dado um elemento e uma lista ordenada retorna a lista resultante de inserir
ordenadamente esse elemento na lista.
Por exemplo, insert 25 [1,20,30,40] corresponde a [1,20,25,30,40]. -}


_insert :: Ord a => a -> [a] -> [a]
_insert x [] = [x]
_insert x (h:t)
    | x > h     = h : _insert x t   -- x é maior? O h passa, continuamos à procura do lugar.
    | otherwise = x : h : t         -- x é menor ou igual? Entra aqui e o resto da lista segue atrás.



{- 32. Apresente uma defini¸c˜ao recursiva da fun¸c˜ao (pr´e-definida) unwords :: [String] -> String que
junta todas as strings da lista numa s´o, separando-as por um espa¸co.
Por exemplo, unwords ["Programacao", "Funcional"] corresponde a "Programacao Funcional".
 -}

_unwords :: [String] -> String
_unwords [] = ""
_unwords (h:t) = h ++" "++ _unwords t 

{- 33. Apresente uma defini¸c˜ao recursiva da fun¸c˜ao (pr´e-definida) unlines :: [String] -> String que
junta todas as strings da lista numa s´o, separando-as pelo caracter ’\n’.
Por exemplo, unlines ["Prog", "Func"] corresponde a "Prog\nFunc\n". -}

_unlines :: [String] -> String
_unlines [] = ""
_unlines (h:t) = h ++"\n"++ _unlines t 


{- 34. Apresente uma defini¸c˜ao recursiva da fun¸c˜ao pMaior :: Ord a => [a] -> Int que dada
uma lista n˜ao vazia, retorna a posi¸c˜ao onde se encontra o maior elemento da lista. As posi¸c˜oes
da lista come¸cam em 0, i.e., a fun¸c˜ao dever´a retornar 0 se o primeiro elemento da lista for o
maior -}

pMaior :: Ord a => [a] -> Int 













