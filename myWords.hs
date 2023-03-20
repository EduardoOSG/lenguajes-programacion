texto = "Hola mundo"

myWords :: String -> [String]
myWords "" = [] -- si el texto está vacío, devolver una lista vacía
myWords s = let (word, rest) = span (/=' ') $ dropWhile (==' ') s -- dividir la cadena en la primera palabra y el resto
            in word : myWords (dropWhile (==' ') rest) -- agregar la primera palabra a la lista y continuar recursivamente con el resto del texto

palabras = myWords texto


--Suma imaginarios
--data Imag Float Float
--
--instance Show Imag
--    where 
--        show (Im a b) = 
--            (show a  ) ++ 
--            " + " ++
--            (show b) ++
--            "i"
--
--suma (Im a b ) (Im c d) =
--    Im (a + c) (b + d)

--cuenta lineas de un texto 
--wc "" = 0
--wc (x:xs)
  --      |(x == '\n') = 1 + wc xs
    --    |(otherwise) = wc xs



--Funcion words 


text = "Hola mundo"

array :: string -> [string]

array "" = []

array = lex(x : xs)

--Definicion de la data para el numerador
data fracc = f { numerador :: Integer , denominador :: Integer }

instance Read Imag
    where 
        readsPrec _ s =
            --Guarda el primer digito en cuanto lle la cadena con Lex
            let [(rs0, rs1)] = lex s
                --Convertimos el valor de la x : xs de la cadena s a un numero entero con el read :: Integer
                r = (read rs0) :: Integer
                [(rs2, rs3)] = lex rs1
                [(rs4, rs5)] = lex

--newton Rampshon

--Utilizar la funcion read  de haskell para leer las fracciones

--declaracion de constantes o variables im1 = "3 + 3i"

--2 Haskell avanzado

--1. Defina un tipo de datos que guarde una fraccion
 

--READ


newtonRaphson :: (Fractional a, Ord a) => (a -> a) -> (a -> a) -> a -> a
newtonRaphson f f' x0
  | abs (f x0) < 0 = x0 -- si x0 ya es una raíz aproximada, devolverla o igual podria ser un aproximado a cero un 0.0000001
  | otherwise = newtonRaphson f f' x1 -- si no, continuar con la iteración
  where
    x1 = x0 - f x0 / f' x0 -- la fórmula de Newton-Raphson

-- definir la función f(x) = x^2 - 2 y su derivada f'(x) = 2x
f x = x*x - 2
f' x = 2*x

-- encontrar la raíz cuadrada de 2 usando x0 = 1
sqrt2 = newtonRaphson f f' 1

texto = "Hola mundo"

myWords :: String -> [String]
myWords "" = [] -- si el texto está vacío, devolver una lista vacía
myWords s = let (word, rest) = span (/=' ') $ dropWhile (==' ') s -- dividir la cadena en la primera palabra y el resto
            in word : myWords (dropWhile (==' ') rest) -- agregar la primera palabra a la lista y continuar recursivamente con el resto del texto

palabras = myWords texto




data Fraccion = Fraccion { numerador :: Integer
                         , denominador :: Integer
                         } deriving Show


