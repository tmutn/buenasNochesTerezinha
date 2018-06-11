-- PARADIGMAS DE PROGRAMACION - TP 1 --
-- TP Funcional 2018 - Microprocesador --
-- ALUMNOS: INNA CHORNA Y TOMAS MARTINEZ 

module MicroEntrega1
    ( nop  
    , lodv  
    , swap  
    , add  
    , str  
    , lod
    , divide
    , programCounter
    , acumuladorA
    , acumuladorB
    , mensajeError
    , obtenerMemoria
    ) where  


import Text.Show.Functions
import Control.Exception.Base

-- 3
-- 3.1 Punto 1 --

data Micro = Micro {
    memoria :: [Int],
    acumA :: Int,
    acumB :: Int,
    programaContador :: Int,
    ultimoError :: String
} deriving (Show)

xt8088 = Micro {
    memoria = replicate 1024 0,
    acumA = 0,
    acumB = 0,
    programaContador = 0,
    ultimoError = " "
}

fp20 = Micro {
    memoria = replicate 1024 0,
    acumA = 7, 
    acumB = 24, 
    programaContador = 0, 
    ultimoError = " "
}

at8086 = Micro {
    memoria = [1..20],
    acumA = 0, 
    acumB = 0, 
    programaContador = 0, 
    ultimoError = " "
}

-- 3.2 Punto 2 --

avanzarContador :: Micro -> Int
avanzarContador = (+1).programaContador 

nop :: Micro -> Micro
nop micro = micro {
    programaContador = avanzarContador micro 
}

-- 3.3 Punto 3 --

lodv :: Int -> Micro -> Micro
lodv valor micro = micro {
    programaContador = avanzarContador micro,
    acumA = valor
}

swap :: Micro -> Micro
swap micro = micro {
    programaContador = avanzarContador micro,
    acumA = acumB micro,
    acumB = acumA micro
}

sumarAcumuladores :: Micro -> Int
sumarAcumuladores micro = (+) (acumA micro) (acumB micro)

add :: Micro -> Micro
add micro = micro {
    programaContador = avanzarContador micro,
    acumA = sumarAcumuladores micro,
    acumB = 0
}

-- 3.4 Punto 4 --

grabarEnMemoria :: Int -> Int -> [Int] -> [Int]
grabarEnMemoria 0 _ listaDatos = listaDatos
grabarEnMemoria posicion valor listaDatos = (take (posicion - 1) listaDatos) ++ [valor] ++ (drop posicion listaDatos) 

str :: Int -> Int -> Micro -> Micro
str posicion valor micro = micro {
    programaContador = avanzarContador micro,
    memoria = grabarEnMemoria posicion valor (memoria micro)
}

lod :: Int -> Micro -> Micro
lod posicion micro = micro {
    programaContador = avanzarContador micro,
    acumA = (memoria micro) !! (posicion-1)    
}

divide :: Micro -> Micro
divide micro = microConDivision (acumA micro) (acumB micro) micro

microConDivision :: Int -> Int -> Micro -> Micro
microConDivision _ 0 micro =  micro {
    programaContador = avanzarContador micro,
    ultimoError = "DIVISION BY ZERO"
} 
microConDivision valorA valorB micro =  micro {
    programaContador = avanzarContador micro,
    acumA = div valorA valorB,
    acumB = 0
} 


-- Test unitarios --

programCounter :: Micro -> Int
programCounter micro = programaContador micro

acumuladorA :: Micro -> Int
acumuladorA micro = acumA micro

acumuladorB :: Micro -> Int
acumuladorB micro = acumB micro

mensajeError :: Micro -> String
mensajeError micro = ultimoError micro

obtenerMemoria :: Micro -> [Int]
obtenerMemoria micro = memoria micro