module Library where
import PdePreludat
import Data.Char (toUpper)

--Parte 1
oro = UnTesoro { anioDesc = 2000, precio = 500}
plata = UnTesoro { anioDesc = 1500, precio = 600}

data Tesoro = UnTesoro { 
anioDesc :: Number,
precio :: Number
} deriving (Show, Eq)

anioActual:: Number
anioActual = 2026

antiguedad :: Tesoro -> Number 
antiguedad tesoro = anioActual - (anioDesc tesoro)

esDeLujo :: Tesoro -> Bool 
esDeLujo tesoro = (precio tesoro) > 1000 || (antiguedad tesoro) > 200 

esTelaSucia :: Tesoro -> Bool
esTelaSucia tesoro = not (esDeLujo tesoro) && (precio tesoro) < 50


esEstandar :: Tesoro -> Bool
esEstandar tesoro = not (esDeLujo tesoro) && not (esTelaSucia tesoro)

valor :: Tesoro -> Number
valor tesoro = (precio tesoro) + (antiguedad tesoro) * 2 


--Parte 2
data Cerradura = UnaCerradura {
clave :: String
}deriving (Show, Eq)


data Herramienta = Martillo | GanzuaGancho | GanzuaRastrillo | GanzuaRombo String |Tensor |LlaveMaestra  deriving (Show,Eq)


martillo :: Cerradura -> Cerradura
martillo cerradura = cerradura{ 
    clave = drop 3 (clave cerradura)}

llaveMaestra :: Cerradura -> Cerradura
llaveMaestra cerradura = cerradura {clave = ""}

ganzuaGancho :: Cerradura -> Cerradura
ganzuaGancho cerradura = cerradura{
    clave = filter (`notElem` ['A'..'Z']) (clave cerradura)}

ganzuaRastrillo :: Cerradura -> Cerradura
ganzuaRastrillo cerradura = cerradura{
    clave = filter (`notElem` ['1'..'9']) (clave cerradura)}

ganzuaRombo :: String -> Cerradura -> Cerradura
ganzuaRombo inscripcion cerradura = cerradura{
    clave = filter (`notElem` inscripcion) (clave cerradura)}

--toUpper funcion q hace mayusc  a las letras
tensor :: Cerradura -> Cerradura
tensor cerradura = cerradura { 
    clave = map toUpper (clave cerradura)
}

tota = UnaCerradura { clave = "Tota123"}

socotroco herramientaUno herramientaDos cerradura = herramientaDos . herramientaUno $ cerradura 


--Nos sirve para hacer la accion de la herramienta poniendola como constructor
usarHerramienta Martillo cerradura = martillo cerradura
usarHerramienta GanzuaGancho        cerradura = ganzuaGancho cerradura
usarHerramienta GanzuaRastrillo    cerradura = ganzuaRastrillo cerradura
usarHerramienta (GanzuaRombo inscripcion)  cerradura = ganzuaRombo inscripcion cerradura
usarHerramienta Tensor cerradura = tensor cerradura
usarHerramienta LlaveMaestra cerradura = llaveMaestra cerradura

--Parte 3

data Ladron = UnLadron { 
    nombre:: String,
    herramientas :: [Herramienta],
    tesorosRob :: [Tesoro]
}deriving (Show,Eq)

yo = UnLadron { nombre ="FRAN", herramientas = [], tesorosRob = [oro,plata]}

experiencia :: Ladron -> Number
experiencia ladron = sum (map precio ( tesorosRob ladron )) 

ladronLegendario :: Ladron -> Bool
ladronLegendario ladron = experiencia ladron > 100 && any esDeLujo (tesorosRob ladron)


--REALIZO FUNCION AUXILIAR QUE ABRA UN COFRE

intentarAbrir :: [Herramienta] -> String -> Tesoro -> Ladron -> Ladron
intentarAbrir herramientas "" cofre ladron = 
    ladron { herramientas = herramientas, tesorosRob = cofre : tesorosRob ladron } -- Éxito: clave vacía

intentarAbrir [] _ _ ladron = 
    ladron { herramientas = [] } -- Fracaso: se quedó sin herramientas

intentarAbrir (h:hs) claveActual cofre ladron = 
    intentarAbrir hs (clave (usarHerramienta h (UnaCerradura claveActual))) cofre ladron

robarCofre ladron cofre cerradura = intentarAbrir (herramientas ladron) (clave cerradura) cofre ladron 


-- Un tesoro de prueba rápido
tesoroCofrePrueba :: Tesoro
tesoroCofrePrueba = UnTesoro { anioDesc = 2020, precio = 300 }

-- Una cerradura con la clave del Ejemplo 1 ("qwERTY")
cerraduraCofrePrueba1 :: Cerradura
cerraduraCofrePrueba1 = UnaCerradura { clave = "qwERTY" }

-- Una cerradura con la clave del Ejemplo 2 ("password")
cerraduraCofrePrueba2 :: Cerradura
cerraduraCofrePrueba2 = UnaCerradura { clave = "password" }

-- Un ladrón de prueba (Manu) con las herramientas del Ejemplo 1
manu :: Ladron
manu = UnLadron { 
    nombre = "Manu", 
    herramientas = [Martillo, GanzuaGancho, GanzuaRastrillo], 
    tesorosRob = [] 
}


atraco :: []
atraco cofres ladron cerraduras