

data Hora = H Int Int
          deriving Show
type Etapa = (Hora,Hora)
type Viagem = [Etapa]


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


testeEtapa:: Etapa->Bool
testeEtapa (hora1,hora2)= ordTime  hora2 hora1


testeViagem:: Viagem->Bool
testeViagem [(hora1,hora2) ]=testeEtapa (hora2, hora1)
testeViagem ((hora1,hora2):(h1,h2):t) = testeEtapa (hora2 ,hora1) && ordTime h1 hora2&& testeViagem ((h1, h2):t)

calcularHora::Viagem->Etapa
calcularHora [(hora1,hora2)]=  (hora1,hora2)
calcularHora ((hora1,hora2):t)= (hora1,snd (last ((hora1,hora2):t)))


tempoViagem::Viagem->Hora
tempoViagem []=H 0 0
tempoViagem ((hora1,hora2):t)= minutosHoras (difHoras hora1 hora2 + horasMinutos (tempoViagem t))

tempoEspera:: Viagem->Hora 
tempoEspera []=H 0 0
tempoEspera ((hora1,hora2):(h1,h2):t)= minutosHoras (difHoras hora2 h1 + horasMinutos (tempoViagem t))

tempoTotal::Viagem->Hora
tempoTotal []=H 0 0
tempoTotal ((hora1,hora2):(h1,h2):t)=minutosHoras (difHoras x1 x2) 
                                    where (x1,x2)= calcularHora ((hora1,hora2):(h1,h2):t)

--minutosHoras( horasMinutos(tempoViagem((hora1,hora2):(h1,h2):t)) + horasMinutos( tempoEspera((hora1,hora2):(h1,h2):t)))


type Poligonal = [Ponto]
data Ponto = Cartesiano Double Double | Polar Double Double deriving (Show,Eq)


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



comprimento :: Poligonal->Double
comprimento [a]=0
comprimento (h:t)=(dist h (head t)) + comprimento t  

linhaFechada::Poligonal->Bool
linhaFechada []= False
linhaFechada (h:t)= posx h == posx (last t)&& posy h == posy (last t)  


data Figura = Circulo Ponto Double
            | Retangulo Ponto Ponto
            | Triangulo Ponto Ponto Ponto
              deriving (Show,Eq)

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

triangula :: Poligonal -> [Figura] 
triangula (h:snd:t) |length ((h:snd:t)) <=3 = []
                    |otherwise =triangulaAux h (snd:t)


triangulaAux::Ponto->Poligonal->[Figura]
triangulaAux p [x,y]=[]
triangulaAux p (h:snd:t)= (Triangulo p h snd) : triangulaAux p (snd:t)


areaTriangula:: Poligonal->Double
areaTriangula(p)=areaTriangulaAux (triangula p)

areaTriangulaAux::[Figura]->Double
areaTriangulaAux (h:t)=area h+ areaTriangulaAux t


































































