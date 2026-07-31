module Library where
import PdePreludat
import Data.Char (toUpper)


data Chico = Chico{
    nombre :: String,
    edad:: Number,
    habilidades:: [String],
    deseos :: [Chico-> Chico]
}deriving (Show)



aprenderHabilidades habilidadesNuevas chico = chico {
    habilidades = habilidadesNuevas: habilidades chico 
    }

versionesNFS  = "jugar need for speed 1" 

serGrosoEnNeedForSpeed chico = chico {
    habilidades= versionesNFS : habilidades chico  
}

serMayor chico = chico {
    edad = 18
}

modificarEdadEnX cantidad chico = chico {
    edad = edad chico + cantidad
}

wanda chico = modificarEdadEnX 1. head (deseos chico)

cosmo chico= modificarEdadEnX (edad chico / 2)

muffingMagico chico = foldl (\chico deseo -> deseo chico) chico (deseos chico)

tieneHabilidad habilidad chico = elem habilidad (habilidades chico)

esMayor chico = edad chico > 18

esSuperMaduro chico = tieneHabilidad "manejar" chico && esMayor chico

data Chica = Chica {
    nombreChica ::String,
    condicion :: Chico-> Bool
}deriving(Show)

noEsTimmy chico = nombre chico /= "Timmy"

trixie = Chica{
    nombreChica = "Trixie Tang",
    condicion = noEsTimmy
}

vicky = Chica{
    nombreChica= "Vicky",
    condicion = tieneHabilidad "ser un supermodelo noruego"
}



quienConquistaA chica pretendientes
    |length ( filter (\chico -> (condicion chica) chico) pretendientes) > 0 = head( filter (\chico -> (condicion chica) chico) pretendientes)
    |otherwise = last pretendientes

chicaCocina = Chica{
    nombreChica = "La Cooker",
    condicion = tieneHabilidad "Cocinar"
}
-- quienConquistaA chicaCocina pretendientes

habilidadesProhibidas = ["Enamorar","Matar", "Dominar el Mundo"]

tieneHabilidadesProhibidas chico = any (`elem` habilidadesProhibidas) (habilidades chico)

infractorDeDaRules = filter tieneHabilidadesProhibidas

