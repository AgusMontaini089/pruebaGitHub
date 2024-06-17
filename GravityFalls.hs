-- Modelo inicial
data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Int,
  precisionJugador :: Int
} deriving (Eq, Show)

-- Jugadores de ejemplo
bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro {

  velocidad :: Int,
  precision :: Int,
  altura :: Int
} deriving (Eq, Show)

type Puntos = Int

-- Funciones útiles
between n m x = elem x [n .. m]

maximoSegun f = foldl1 (mayorSegun f)
mayorSegun f a b
  | f a > f b = a
  | otherwise = b

type Palo = Int -> Tiro -> Tiro

putter :: Palo
putter _ tiroRecibido = UnTiro {
    velocidad = 10,
    precision = ((*2).precision $ tiroRecibido),
    altura = 0
}


madera ::  Palo
madera _ tiroRecibido = UnTiro 100 ((`div`2).precision $ tiroRecibido) 5

-- Los hierros, que varían del 1 al 10 (número al que denominaremos n), 
-- generan un tiro de velocidad igual a la fuerza multiplicada por n, la precisión dividida por n y una altura de n-3 
-- (con mínimo 0). 
-- Modelarlos de la forma más genérica posible.

hierros :: Palo
hierros n tiro = tiro {
    velocidad = (*n).velocidad $ tiro,
    precision = (`div` n).precision $ tiro,
    altura = n-3
}


tiroCualquiera :: Tiro
tiroCualquiera = UnTiro 5 5 5

-- Definir la función golpe que dados una persona y un palo, 
-- obtiene el tiro resultante de usar ese palo con las habilidades de la persona.

-- Un túnel con rampita sólo es superado si la precisión es mayor a 90 yendo al ras del suelo, 
-- independientemente de la velocidad del tiro. Al salir del túnel la velocidad del tiro se duplica, 
-- la precisión pasa a ser 100 y la altura 0.

type Obstaculo = Tiro -> Tiro

tiroNulo = UnTiro 0 0 0

tunel :: Obstaculo
tunel tiro 
    | (>=90).precision $ tiro = UnTiro ((*2).velocidad $ tiro) 100 0
    | otherwise = tiroNulo


-- Una laguna es superada si la velocidad del tiro es mayor a 80 y tiene una altura de entre 1 y 5 metros. 
-- Luego de superar una laguna el tiro llega con la misma velocidad y precisión, pero una altura equivalente 
-- a la altura original dividida por el largo de la laguna.

laguna :: Int -> Obstaculo
laguna largoLaguna tiro 
    | ((>80).velocidad $ tiro) && ((>=1).altura $ tiro) && ((<=5).altura $ tiro) = tiro{ altura = (`div` largoLaguna).altura $ tiro}
    | otherwise = tiroNulo

-- Un hoyo se supera si la velocidad del tiro está entre 5 y 20 m/s yendo al ras del suelo con una precisión mayor a 95. 
-- Al superar el hoyo, el tiro se detiene, quedando con todos sus componentes en 0.

hoyo :: Obstaculo
hoyo tiro 
    | ((>=5).velocidad $ tiro) && ((<=20).velocidad $ tiro) && ((>95).precision $ tiro) = tiroNulo
    | otherwise = tiroNulo 

-- Definir palosUtiles que dada una persona y un obstáculo, permita determinar qué palos le sirven para superarlo.
palosUtiles :: Jugador -> Obstaculo -> [Palos ]-> [Palos] 