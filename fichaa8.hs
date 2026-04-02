import Data.List
data Frac = F Integer Integer


{- (a) Defina a fun¸c˜ao normaliza :: Frac -> Frac, que dada uma frac¸c˜ao calcula uma
frac¸c˜ao equivalente, irredut´ıvel, e com o denominador positivo. Por exemplo,
normaliza (F (-33) (-51)) deve retornar F 11 17 e normaliza (F 50 (-5))
deve retornar F (-10) 1. Sugere-se que comece por definir primeiro a fun¸c˜ao
mdc :: Integer -> Integer -> Integer que calcula o m´aximo divisor comum
entre dois n´umeros, baseada na seguinte propriedade (atribuida a Euclides):
mdc x y == mdc (x+y) y == mdc x (y+x) -}



normaliza :: Frac -> Frac
normaliza (F 0 _) = F 0 1
normaliza (F x y) = F (sinal * (x `div` divisor)) (abs (y `div` divisor))
    where 
        divisor = mdc x y 
        sinal = if y < 0 then (-1) else 1

mdc :: Integer -> Integer -> Integer
mdc x 0 = abs x
mdc x y = mdc y (x `mod` y)


--(b) Defina Frac como instˆancia da classe Eq.

instance Eq Frac where
    -- f1 é a primeira fração, f2 é a segunda
     f1 == f2 = (n1==n1') && (n2==n2')
              where
        (F n1 n2)  = normaliza f1
        (F n1' n2')= normaliza f2 

--(c) Defina Frac como instˆancia da classe Ord
instance Ord Frac where
    f1 <= f2 
        | d1 == d2  = n1 <= n2        -- CASO 1: Se os denominadores são iguais, só comparamos o cima
        | otherwise = n1 * d2 <= n2 * d1 -- CASO 2: Regra da multiplicação cruzada
        where
            (F n1 d1) = normaliza f1  -- Tens sempre de normalizar primeiro!
            (F n2 d2) = normaliza f2



{- (d) Defina Frac como instˆancia da classe Show, de forma a que cada frac¸c˜ao seja
apresentada por (numerador/denominador). -}


instance Show Frac where
    show f |d ==1 = show n 
           |otherwise = "(" ++ show n ++"/" ++ show d ++ ")"
            where 
              (F n d) = normaliza f


{- (e) Defina Frac como instˆancia da classe Num. Relembre que a classe Num tem a
seguinte defini¸c˜ao
class (Eq a, Show a) => Num a where
(+), (*), (-) :: a -> a -> a
negate, abs, signum :: a -> a
fromInteger :: Integer -> a
 -}

instance Num Frac where
    (F n1 n2) + (F n1' n2') = normaliza (F (n1*n2'+ n1'*n2) (n2*n2'))
    
    (F n1 n2) - (F n1' n2')=normaliza (F (n1*n2'- n1'*n2) (n2*n2'))
    
    (F n1 n2) * (F n1' n2')=normaliza (F (n1*n1') n2)
    
    negate (F n1 n2) = normaliza (F (-n1) (n2))
    
    abs (F n d)= normaliza (F (abs n) (abs d))

    signum (F n d) |n'>0 = F 1 1
                   |n'<0  =F (-1) ( 1)
                   |otherwise= F (0) (1) 
                   where 
                    (F n' _) = normaliza (F n d) 
    fromInteger x = F x 1



{- (f) Defina uma fun¸c˜ao que, dada uma frac¸c˜ao f e uma lista de frac¸c˜oes l, selecciona
de l os elementos que s˜ao maiores do que o dobro de f. -}


dobrof:: Frac -> [Frac]-> [Frac]
dobrof f [] = [] 
dobrof (F n d) (h:t) = if h > (2*(F n d )) then (h:dobrof (F n d ) t)
                       else dobrof (F n d ) t


data Exp a = Const a
            | Simetrico (Exp a)
            | Mais (Exp a) (Exp a)
            | Menos (Exp a) (Exp a)
            | Mult (Exp a) (Exp a)



--(a) Declare Exp a como uma instˆancia de Show.

instance Show a => Show (Exp a) where
   show (Const a)     = show a
   show (Simetrico e) = "(-" ++ show e ++ ")"
   show (Mais e1 e2)  = "(" ++ show e1 ++ "+" ++ show e2 ++ ")"
   show (Menos e1 e2) = "(" ++ show e1 ++ "-" ++ show e2 ++ ")"
   show (Mult e1 e2)  = "(" ++ show e1 ++ "*" ++ show e2 ++ ")"

--(b) Declare Exp a como uma instˆancia de Eq.


instance Eq a => Eq (Exp a) where
    (Const x) == (Const y)         = x == y
    (Simetrico x) == (Simetrico y) = x == y
    (Mais x y) == (Mais w z)       = (x == w) && (y == z)
    (Menos x y) == (Menos w z)     = (x == w) && (y == z)
    (Mult x y) == (Mult w z)       = (x == w) && (y == z)
    _ == _                         = False


instance Num a => Num (Exp a) where
    -- CORREÇÃO 1: Letra minúscula e converter o valor 'n' para o tipo 'a'
    fromInteger n = Const (fromInteger n) 
    
    x + y = Mais x y 
    x - y = Menos x y 
    x * y = Mult x y 
    
    -- CORREÇÃO 2: A função chama-se 'negate'
    negate x = Simetrico x



data Movimento = Credito Float | Debito Float
data Data = D Int Int Int
data Extracto = Ext Float [(Data, String, Movimento)]


-- Não precisas do (Ord a) => porque Data já é um tipo concreto (Int Int Int)
{- instance Eq Data => Ord Data where
    compare (D ano1 mes1 dia1) (D ano2 mes2 dia2)
        | ano1 > ano2 = GT  -- Se o ano for maior, é maior logo (Greater Than)
        | ano1 < ano2 = LT  -- Se for menor, é menor logo (Less Than)
        -- Anos iguais? Vamos ver os meses:
        | mes1 > mes2 = GT
        | mes1 < mes2 = LT
        -- Meses iguais? Vamos ver os dias:
        | dia1 > dia2 = GT
        | dia1 < dia2 = LT
        -- Tudo igual?
        | otherwise   = EQ -}


{- instance Ord Data where
    (D a1 m1 d1) <= (D a2 m2 d2) = 
        (a1 < a2) ||
        (a1 == a2 && m1 < m2) ||
        (a1 == a2 && m1 == m2 && d1 <= d2) -}



instance Show Data where
    -- show (D ano mes dia) = ...
    show (D a m d) = show a ++ "/" ++ show m ++ "/" ++ show d


{- (c) Defina a fun¸c˜ao ordena :: Extracto -> Extracto, que transforma um ex￾tracto de modo a que a lista de movimentos apare¸ca ordenada por ordem crescente
de data -}
{- pegaData :: (Data, String, Movimento) -> Data
pegaData (d, _, _) = d

ordena :: Extracto -> Extracto
ordena (Ext saldo lista) = Ext saldo (sortOn pegaData lista) -}
                        
--ordena (Ext saldo lista) = Ext saldo (sortOn (\(d, _, _) -> d) lista)


{- (d) Defina Extracto como instˆancia da classe Show, de forma a que a apresenta¸c˜ao do
extracto seja por ordem de data do movimento com o seguinte, e com o seguinte
aspecto
Saldo anterior: 300
---------------------------------------
Data Descricao Credito Debito
---------------------------------------
2010/4/5 DEPOSITO 2000
2010/8/10 COMPRA 37,5
2010/9/1 LEV 60
2011/1/7 JUROS 100
2011/1/22 ANUIDADE 8
---------------------------------------
Saldo actual: 2294,5 -}


-- Função para calcular o saldo final
saldoFinal :: Float -> [(Data, String, Movimento)] -> Float
saldoFinal s [] = s
saldoFinal s ((_, _, Credito x):t) = saldoFinal (s + x) t
saldoFinal s ((_, _, Debito x):t)  = saldoFinal (s - x) t

-- Função para formatar cada linha
formataLinha :: (Data, String, Movimento) -> String
formataLinha (dt, desc, mov) = 
    let dStr = show dt
        -- Ajuste simples de espaçamento (tabulação manual)
        sep1 = replicate (12 - length dStr) ' '
        sep2 = replicate (12 - length desc) ' '
    in case mov of
        Credito x -> dStr ++ sep1 ++ desc ++ sep2 ++ show x ++ "\n"
        Debito x  -> dStr ++ sep1 ++ desc ++ sep2 ++ "          " ++ show x ++ "\n"

-- A Instância Final
instance Show Extracto where
    show (Ext saldoIni lista) = 
        "Saldo anterior: " ++ show saldoIni ++ "\n" ++
        "----------------------------------------------------------\n" ++
        "Data        Descricao   Credito     Debito\n" ++
        "----------------------------------------------------------\n" ++
        concatMap formataLinha lista ++
        "----------------------------------------------------------\n" ++
        "Saldo actual: " ++ show (saldoFinal saldoIni lista)


{- Sem problema. Já que o código está feito, vamos dissecar cada parte para perceberes a lógica por trás, como se estivéssemos a desmontar um motor.

O objetivo era transformar o Extracto (que é apenas dados) num texto bonito (uma tabela). Para isso, dividi o problema em 3 peças:

Peça 1: A Matemática (saldoFinal)
O extrato guarda o saldo inicial, mas tu precisas de mostrar o saldo final. O Haskell não faz contas sozinho, tens de criar uma função que percorra a lista toda e vá atualizando o valor.

Haskell

saldoFinal :: Float -> [(Data, String, Movimento)] -> Float
saldoFinal s [] = s
saldoFinal s ((_, _, Credito x):t) = saldoFinal (s + x) t
saldoFinal s ((_, _, Debito x):t)  = saldoFinal (s - x) t
s: É o acumulador. Começa com o saldo inicial.

Caso Base []: Se a lista de movimentos acabou, o valor que está em s é o resultado final.

Recursão:

Se encontra um Credito x, chama-se a si própria com s + x.

Se encontra um Debito x, chama-se a si própria com s - x.

Peça 2: A Estética (formataLinha)
Esta é a parte "chata". Tens de garantir que as colunas ficam alinhadas. Recebes um trio: (Data, Descrição, Movimento).

Haskell

formataLinha :: (Data, String, Movimento) -> String
formataLinha (dt, desc, mov) = 
    let dStr = show dt
        sep1 = replicate (12 - length dStr) ' '
        sep2 = replicate (12 - length desc) ' '
    in ...
O Truque do Alinhamento (replicate): Imagina que queres que a coluna da Data tenha 12 espaços de largura.

Se a data for "2023/1/1" (8 letras), precisas de 4 espaços extra.

A conta é: 12 - length "2023/1/1".

O replicate cria esses espaços vazios para encher o buraco.

O case do Movimento:

Haskell

    case mov of
        Credito x -> ... ++ show x ++ "\n"
        Debito x  -> ... ++ "          " ++ show x ++ "\n"
Crédito: Escreves logo a seguir à descrição (Coluna 3).

Débito: Tens de dar um "salto" (espaços em branco) para passar por cima da coluna dos créditos e escrever na Coluna 4.

Peça 3: A Montagem (instance Show)
Aqui é só colar tudo numa String gigante.

Haskell

instance Show Extracto where
    show (Ext saldoIni lista) = 
        -- 1. O Cabeçalho
        "Saldo anterior: " ++ show saldoIni ++ "\n" ++
        "----------------------------------\n" ++
        "Data        Descricao   Credito   Debito\n" ++
        "----------------------------------\n" ++
        
        -- 2. O Corpo (As linhas todas)
        concatMap formataLinha lista ++
        
        -- 3. O Rodapé (Saldo final calculado)
        "----------------------------------\n" ++
        "Saldo actual: " ++ show (saldoFinal saldoIni lista)
O que é o concatMap? É uma função que faz duas coisas de uma vez:

Aplica o formataLinha a cada elemento da lista (transforma os trios em Strings).

Cola (concat) essas Strings todas numa só, para não ficares com uma lista de frases soltas.

Resumo:

Calculaste o dinheiro que sobra.

Desenlhaste cada linha com espacinhos para alinhar.

Juntaste cabeçalho, linhas e rodapé. -}



