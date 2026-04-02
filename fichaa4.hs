import Data.Char
import Data.List
import Prelude
--Devolve o par de strings em qu euma tem so letras e a outra so numeros
digitAlpha::String->(String,String)
digitAlpha [] =([],[])
digitAlpha (h:t)=
    let (n,l)=digitAlpha t
    in if isDigit h then (h:n,l)
        else (n,h:l)


nzp :: [Int] -> (Int,Int,Int)
nzp []= (0,0,0)
nzp(h:t)=
    let (n,z,p)=nzp t
   in if h>0 then (n,z,p+1)
   else if h==0 then (n,z+1,p)
   else (n+1,z,p)


myDivMod :: Integral a => a -> a -> (a, a)
myDivMod _ 0 = error "Divisao por zero"
myDivMod x y
    | x < y     = (0, x)         -- Caso base: não dá para dividir mais.
    | otherwise =
        let (q, r) = myDivMod (x - y) y  -- Chamada recursiva com o valor subtraído
        in (q + 1, r)



-- Função principal que chama a auxiliar
fromDigits :: [Int] -> Int
fromDigits l = fromDigitsAux l 0

-- Função auxiliar com acumulador
fromDigitsAux :: [Int] -> Int -> Int
fromDigitsAux [] acc = acc        -- Se a lista acabou, devolvemos o que acumulámos
fromDigitsAux (h:t) acc = fromDigitsAux t (acc * 10 +h)

--maxSumInit :: (Num a, Ord a) => [a] -> a
--maxSumInit l = maximum [sum m | m <- inits l]





maxSumInit :: (Num a, Ord a) => [a] -> a
maxSumInit l = mymaxSumInitAux l 0 0


mymaxSumInitAux ::(Num a, Ord a) => [a]->a->a->a
mymaxSumInitAux [] _ y = y
mymaxSumInitAux (h:t) somaAtual maximo=
    let novaSoma =somaAtual + h
        novoMaximo= max novaSoma maximo
    in  mymaxSumInitAux t novaSoma novoMaximo


--6. Optimize a seguinte defini¸c˜ao recursiva da fun¸c˜ao que calcula o n-´esimo n´umero da
--sequˆencia de Fibonacci, usando uma fun¸c˜ao auxiliar com 2 acumuladores que represen￾tam, respectivamente, o n-´esimo e o n+1-´esimo n´umeros dessa sequˆencia.
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)


myfib::Int->Int
myfib n = fibAux n 0 1

fibAux:: Int->Int->Int->Int
fibAux 0 x y= x
fibAux n x y = fibAux (n-1) y (x+y)

{- 7. Defina a fun¸c˜ao intToStr :: Integer -> String que converte um inteiro numa
string. Utilize uma fun¸c˜ao auxiliar com um acumulador onde vai construindo a string
que vai devolver no final. -}

intToStr :: Integer -> String
intToStr x = intToStrAux x []

intToStrAux:: Integer->String->String
intToStrAux 0 acc=acc
intToStrAux x acc = intToStrAux ( div x 10 ) (intToDigit(fromInteger (mod x 10)) : acc )


{- shift+alt+A -}

{- 8. Para cada uma das express˜oes seguintes, exprima por enumera¸c˜ao a lista correspon￾dente. Tente ainda, para cada caso, descobrir uma outra forma de obter o mesmo
resultado. -}

{-
a) 6;12;18
   [x | x <- [1..20], mod x 6 == 0]
b)[x | x <-[2,4,6,8,10,12,14,16,18,20], mod x 3 == 0]-->6,12,18
   [x | x <- [1..20], mod x 6 == 0]
c)(10,20)(11,19)(12,18)(13,17)(14,16)(15,15)(16,14)(17,13)(18,12)(19,11)(20,10)
   [(x,30-x) | x <- [10..20]]
d)1,1,4,4,9,9,16,16,25,25
  [x^2|x<-[1..5],y<-[1,2]]
 -}

{-
9. Defina cada uma das listas seguintes por compreens˜ao
a)[2^x|x<-[0..10]]
b)[(x,6-x)|x<-[1..5]]
c)[[x |x<-[1..y]]| y<-[1..5]]
d)[[1 |x<-[1..y]]| y<-[1..5]]
e)[product [1..y] | y <- [1..6]]


 -}






























































































