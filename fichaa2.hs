import Data.Char
import System.Win32 (COORD(yPos))
import Data.List





-- 1. Indique como ´e que o interpretador de Haskell avalia as express˜oes das al´ıneas que se
-- seguem, apresentando a cadeia de redu¸c˜ao de cada uma dessas express˜oes (i.e., os v´arios
-- passos interm´edios at´e se chegar ao valor final).
-- (a) Considere a seguinte defini¸c˜ao:
-- funA :: [Double] -> Double
-- funA [] = 0
-- funA (y:ys) = y^2 + (funA ys)
-- Diga, justificando, qual ´e o valor de funA [2,3,5,1].
--R:39

-- (b) Considere seguinte defini¸c˜ao:
-- funB :: [Int] -> [Int]
-- funB [] = []
-- funB (h:t) = if (mod h 2)==0 then h : (funB t)
-- else (funB t)
-- Diga, justificando, qual ´e o valor de funB [8,5,12].
--R:[8,12]
-- (c) Considere a seguinte defini¸c˜ao:
-- funC (x:y:t) = funC t
-- funC [x] = [x]
-- funC [] = []
-- Diga, justificando, qual ´e o valor de funC [1,2,3,4,5].
--[5]

-- (d) Considere a seguinte defini¸c˜ao:
-- funD l = g [] l
-- g acc [] = acc
-- g acc (h:t) = g (h:acc) t
-- Diga, justificando, qual ´e o valor de funD "otrec".
--

dobros :: [Float] -> [Float]
dobros[]=[]
dobros (h:t)=(2*h:dobros t)

numOcorre :: Char -> String -> Int
numOcorre _ []=0
numOcorre x (h:t)=if x==h then 1 + numOcorre x t
                  else numOcorre x t


positivos :: [Int] -> Bool
positivos []=False
positivos [x] |x>0 = True
              |otherwise = False
positivos (h:t)=if h>0 then positivos t
                else False

soPos :: [Int] -> [Int] 
soPos []=[]
soPos (h:t)= if h>0 then (h:soPos t)
            else soPos t

somaNeg :: [Int] -> Int
somaNeg []=0
somaNeg (h:t)= if h<0 then (abs h)+ somaNeg t
            else somaNeg t

tresUlt :: [a] -> [a]
tresUlt[]=[]
tresUlt (h:t) |length(h:t) <= 3 = (h:t)
              |otherwise = tresUlt t



segundos :: [(a,b)] -> [b]
segundos []=[]
segundos((x,y):t)=(y:segundos t) 

nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros _ []=False
nosPrimeiros x ((h1,h2):t)=if x==h1 then True
                        else nosPrimeiros x t

sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c) 
sumTriplos []=(0,0,0)
sumTriplos ((x,y,z):t)=(x+t1,y+t2,z+t3)
          where (t1,t2,t3)=sumTriplos t

soDigitos :: [Char] -> [Char]
soDigitos[]=[]
soDigitos(h:t)= if isDigit h then (h:soDigitos t)
                else soDigitos t


minusculas :: [Char] -> Int
minusculas []=0
minusculas (h:t)= if isLower h then 1+ minusculas t
                  else minusculas t

nums :: String -> [Int]
nums[]=[]
nums (h:t)=if isDigit h then (digitToInt h:nums t)
                else nums t


type Polinomio = [Monomio]
type Monomio = (Float,Int)


conta :: Int -> Polinomio -> Int 
conta n [] = 0 
conta n ((x,y):t) | n == y = 1 + (conta n t)
                  | otherwise = conta n t 

grau :: Polinomio -> Int 
grau [] = 0 
grau ((x,y):t) = grauAux y t

grauAux::Int->Polinomio->Int
grauAux x []=x
grauAux h ((x,y):t) | h>=y = grauAux h t
                    |otherwise = grauAux y t

selgrau :: Int -> Polinomio -> Polinomio
selgrau x []=[]
selgrau h ((x,y):t) | h==y = ((x,y):selgrau h t)
                    |otherwise = selgrau h t

deriv :: Polinomio -> Polinomio
deriv []=[] 
deriv ((x,y):t) |y==0 =deriv t 
                |otherwise = ((x* fromIntegral y,y):deriv t)

calcula :: Float -> Polinomio -> Float
calcula _ []=0
calcula h ((x,y):t)= (x*(h^y)) + calcula h t


simp :: Polinomio -> Polinomio 
simp[]=[]
simp ((x,y):t)= if x ==0 then (simp t)
                else ((x,y): simp t)

mult :: Monomio -> Polinomio -> Polinomio
mult _ [] = []
mult (coef, expo) ((h1,h2):t) = ((coef * h1), (expo + h2)) : mult (coef, expo) t


--Daqui para baixo é copiado

-- h) Dado um polinómio calcula um polinómio equivalente que não contenha varios monomios do mesmo grau
    -- Analisem esta função com cuidado que pode ser um pouco confusa
normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza ((h1,h2):t)
  | h1 == 0 = normaliza t
  | hasEqualExponents (h1,h2) t && coef == 0 = normaliza (polWOsumMons (h1,h2) t)
  | hasEqualExponents (h1,h2) t = (coef, expo) : normaliza (polWOsumMons (h1,h2) t)
  | otherwise = (h1,h2) : normaliza t
  where (coef, expo) = sumMons (h1,h2) t


hasEqualExponents :: Monomio -> Polinomio -> Bool
hasEqualExponents _ [] = False
hasEqualExponents (coef,expo) ((x,y):t)
  | expo == y  = True
  | otherwise  = hasEqualExponents (coef,expo) t


sumMons :: Monomio -> Polinomio -> Monomio
sumMons (coef,expo) [] = (coef,expo)
sumMons (coef,expo) ((x,y):t)
  | expo == y  = sumMons (coef + x, expo) t
  | otherwise  = sumMons (coef, expo) t


polWOsumMons :: Monomio -> Polinomio -> Polinomio
polWOsumMons _ [] = []
polWOsumMons (coef,expo) ((x,y):t)
  | expo == y  = polWOsumMons (coef,expo) t
  | otherwise  = (x,y) : polWOsumMons (coef,expo) t


soma :: Polinomio -> Polinomio -> Polinomio
soma [] [] = []
soma p []=p 
soma [] p=p
soma p1 p2 =normaliza (p1 ++ p2)

produto :: Polinomio -> Polinomio -> Polinomio
produto [] p = []
produto p [] = []
produto p1 p2 = normaliza (produtoAux p2 p1 p2) -- O uso da função "normaliza" aqui é opcional

produtoAux :: Polinomio -> Polinomio -> Polinomio -> Polinomio
produtoAux p2 [x] [] = []        -- O primeiro polinomio serve de back-up para que sempre que o segundo polinomio do input original chegar ao fim seja possivel reinicia lo
produtoAux p2 ((h1,h2):t) [] = produtoAux p2 t p2     
produtoAux p2 ((h1,h2):t) ((x1,x2):t2) = ((h1*x1),(h2+x2)) : produtoAux p2 ((h1,h2):t) t2

-- Esta função é um pouco confuso, recomendo que vejam com atenção
ordena :: Polinomio -> Polinomio
ordena [] = []
ordena ((coef, expo): t) = ordenaAux ((coef, expo):t) expo ((coef, expo): t)

-- Os inputs desta auxiliar representam a lista Ordenada (resposta), o expoente que vamos comparar com o expoente do próximo elemento, e o polinómio que estamos a percorrer
ordenaAux :: Polinomio -> Int -> Polinomio -> Polinomio
ordenaAux lOrd _ [] = lOrd               -- Quando conseguirmos percorrer a lista até ao fim é porque os elementos já estão todos ordenados
ordenaAux lOrd x ((coef,expo):t) | expo >= x = ordenaAux lOrd expo t
                                 | otherwise = ordenaAux list expo list
                                     where list = ([(coef,expo)] ++ (delete (coef,expo) lOrd))

-- l) Esta função testa se 2 polinomios são equivalentes

equiv :: Polinomio -> Polinomio -> Bool
equiv p1 p2 = ordena(normaliza p1) == ordena(normaliza p2)   -- Testei bastantes casos e parece me que funciona em todas as situações
                                                             -- Tive de fazer alterações à função "normaliza" para ser mais restritiva para a poder utilizar desta forma




























