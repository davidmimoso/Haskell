import Data.Char
import Data.Fixed



-- perimetro:: Double->Double
-- perimetro r=2*pi*r

-- dist::(Double,Double)->(Double,Double)->Double
-- dist (x1,y1)(x2,y2)= sqrt ((x1-x2)^2+(y1-y2)^2)

primUlt::[a]->(a,a)
primUlt a =(head a ,last a)

multiplo:: Int->Int->Bool
multiplo m n = mod m n == 0


truncaImpar:: [a]->[a]
truncaImpar a= if even (length a) then a
else tail a

max2:: Int->Int->Int
max2 = max

max3::Int->Int->Int->Int
max3 m n f = max2 m (max2 n f)

nRaizes::Double->Double->Double->Double
nRaizes a b c =
 let delta=(b^2 - 4*a*c)
 in if delta>0 then 2
    else if delta==0 then 1
    else 0

--  delta>0->2raizes
-- delta=0->1raiz
-- delta<0->0raizes 

raizes::Double->Double->Double->[Double]
raizes a b c |nRaizes a b c ==0=[]
             |nRaizes a b c ==1=[(-b)/(2*a)]
             |nRaizes a b c ==2=[(- b) +  sqrt (b ^ 2 - 4 * a * c) / (2 * a) / (2 * a),(- b) - sqrt (b ^ 2 - 4 * a * c) / (2 * a)]


-- type Hora = (Int,Int)

-- validTime::Hora->Bool
-- validTime (h,m)= 0<=h && h<24 && 0<=m && m<60


-- ordTime::Hora->Hora->Bool
-- ordTime (h1,m1) (h2,m2) |validTime (h1,m1) && validTime (h2,m2)= h1>h2||h1==h2 && m1>m2
--                         |otherwise=False


-- horasMinutos::Hora->Int
-- horasMinutos (h,m)= h*60+m

-- minutosHoras::Int->Hora
-- minutosHoras m =(div m 60,mod m 60)

-- difHoras::Hora->Hora->Int
-- difHoras t1 t2= if validTime t1 && validTime t2 then abs (horasMinutos t1 -horasMinutos t2)
--                 else error "Horas invalidas"

-- addMinutos::Hora->Int->Hora
-- addMinutos t1 x =
--        let totalMinutos=(horasMinutos t1 + x)
--        in if validTime t1 then minutosHoras totalMinutos
--          else error"Hora Invalida"

data Hora = H Int Int deriving (Show,Eq)

validTime::Hora->Bool
validTime (H h m)= 0<=h && h<24 && 0<=m && m<60

ordTime::Hora->Hora->Bool
ordTime (H h1 m1) (H h2 m2) |validTime (H h1 m1) && validTime (H h2 m2)= h1>h2||h1==h2 && m1>m2
                        |otherwise=False

horasMinutos::Hora->Int
horasMinutos (H h m)= h*60+m

minutosHoras::Int->Hora
minutosHoras m = H  (div m 60) (mod m 60)

difHoras::Hora->Hora->Int
difHoras t1 t2= if validTime t1 && validTime t2 then abs (horasMinutos t1 -horasMinutos t2)
                 else error "Horas invalidas"

addMinutos::Hora->Int->Hora
addMinutos t1 x =
        let totalMinutos=(horasMinutos t1 + x)
        in if validTime t1 then minutosHoras totalMinutos
          else error "Hora Invalida"



data Semaforo = Verde | Amarelo | Vermelho deriving (Show,Eq)


next :: Semaforo -> Semaforo
next Verde = Amarelo
next Amarelo = Vermelho
next Vermelho = Verde

stop :: Semaforo -> Bool
stop Vermelho=True
stop _=False

safe :: Semaforo -> Semaforo -> Bool
safe _ Vermelho =True
safe Vermelho _=True
safe _ _ =False

data Ponto = Cartesiano Double Double | Polar Double Double deriving (Show,Eq)

-- De polar (Polar (r, θ)) para cartesiano faz-se (x=r⋅cos(θ))

posx :: Ponto -> Double
posx (Cartesiano x y)  = x
posx (Polar r θ) = r * cos θ

posy :: Ponto -> Double
posy (Cartesiano x y) = y
posy (Polar r θ)= r*sin θ


raio :: Ponto -> Double
raio (Cartesiano x y)= sqrt (x^2+y^2)
raio (Polar r θ)= r


angulo :: Ponto -> Double
angulo (Cartesiano x y) |x<0 && y>= 0 = atan (y/x) + pi
                        |x<0 && y<0 = atan (y/x) - pi
                        |x==0 && y> 0 = pi/2
                        |x==0 && y< 0 = -(pi/2)
                        |x>0          =atan (y/x)
angulo (Polar r θ) =θ

dist :: Ponto -> Ponto -> Double
dist (Cartesiano x1 y1) (Cartesiano x2 y2)= sqrt ((x1-x2)^2 +(y1-y2)^2)
dist (Cartesiano x1 y1) (Polar r2 θ2)=sqrt ((x1-r2*cos θ2)^2+(y1-r2*sin θ2)^2)
dist (Polar r1 θ1) (Cartesiano x2 y2)=sqrt ((r1*cos θ1-x2)^2+(r1*sin θ1-y2)^2)
dist (Polar r1 θ1) (Polar r2 θ2)=sqrt ((r1*cos θ1-r2*cos θ2)^2+(r1*sin θ1-r2*sin θ2)^2)


-- Versão do gabriels:
-- distFunc :: Ponto -> Ponto -> Double
-- distFunc (Cartesiano x1 y1) (Cartesiano x2 y2) = sqrt((x2 - x1)^2 + (y2 - y1)^2)
-- distFunc (Cartesiano x y) (Polar dist angle) = sqrt((posx (Polar dist angle) - x)^2 + (posy (Polar dist angle) - y)^2)
-- distFunc (Polar dist angle) (Cartesiano x y) = sqrt(( x - posx (Polar dist angle))^2 + (y - posy (Polar dist angle))^2)
-- distFunc (Polar dist1 angle1) (Polar dist2 angle2) = sqrt((x2 - x1)^2 + (y2 - y1)^2)
--                                         where x2 = posx (Polar dist2 angle2)
--                                               x1 = posx (Polar dist1 angle1)
--                                               y2 = posy (Polar dist2 angle2)
--                                               y1 = posy (Polar dist1 angle1)

data Figura = Circulo Ponto Double
            | Retangulo Ponto Ponto
            | Triangulo Ponto Ponto Ponto
              deriving (Show,Eq)


poligono :: Figura -> Bool
poligono (Circulo _ _) = False
poligono _ = True

vertice :: Figura -> [Ponto]
vertice (Circulo _ _) = []
vertice (Retangulo p1 p2) = [p1 , p2 , Cartesiano (posx p1) (posy p2) , Cartesiano (posx p2) (posy p1)]
vertice (Triangulo p1 p2 p3) = [p1 , p2 , p3]

area :: Figura -> Double
area (Triangulo p1 p2 p3) =
      let a = dist p1 p2
          b = dist p2 p3
          c = dist p3 p1
          s = (a+b+c) / 2 -- semi-perimetro
      in sqrt (s*(s-a)*(s-b)*(s-c)) -- formula de Heron
area(Circulo p1 r)= pi*r^2
area(Retangulo p1 p2)=(posx p1-posx p2)*(posy p1-posy p2)
--area do retangulo = base * altura 


perimetro :: Figura -> Double
perimetro (Triangulo p1 p2 p3) =
      let a = dist p1 p2
          b = dist p2 p3
          c = dist p3 p1
        in (a+b+c)
perimetro (Circulo p1 r)= 2*pi*r
perimetro (Retangulo p1 p2)=2*abs (posx p1-posx p2)+ 2*abs (posy p1-posy p2)


isLower :: Char -> Bool
isLower c = ord c>=ord 'a' && ord c <= ord 'z'

isDigit :: Char -> Bool
isDigit c = ord c>=ord '0' && ord c <= ord '9'

isAlpha :: Char -> Bool
isAlpha c= (ord c >= ord 'a' && ord c <= ord 'z') || (ord c >= ord 'A' && ord c <= ord 'Z')

toUpper :: Char -> Char
toUpper c = chr((ord 'c') -32)


intToDigit :: Int -> Char
intToDigit c =chr(c+48)

digitToInt :: Char -> Int
digitToInt c = (ord c) -48


































