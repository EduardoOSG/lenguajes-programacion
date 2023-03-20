--1 Haskell

--1. Escriba una funcion newtonRaphson que dada una función, su derivada y un punto inicial encuentre la raiz (x donde f(x) = 0) de la función.

--ejemplo de ejecucion newtonRaphson (\x -> -x^2) (\x -> -2 * x) (-1) 0.00001  

newtonRaphson :: (Ord t, Fractional t) => (t -> t) -> (t -> t) -> t -> t -> t
newtonRaphson f f' x0 tolerancia = 
    if abs(f x0) >= tolerancia then
      newtonRaphson f f' x1 tolerancia
    else 
      x0
    where
      x1 = x0 - f x0 / f' x0


--2. Escriba una función words que dada un texto lo divida en palabras


myWords :: String -> [String] -- recibe una cadena de texto 
myWords "" = [] -- si el texto está vacío, devolver una lista vacía
myWords s = let (palabra, restoCadena) = span (/=' ') $ dropWhile (==' ') s -- dividir la cadena en la primera palabra y el restaFraccioneso
            in palabra : myWords (dropWhile (==' ') restoCadena) -- agregar la primera palabra a la lista y continuar recursivamente con el restaFraccioneso del texto

---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- 2 Haskell avanzado
--1 Defina un tipo de dato que guarde una fraccion
data Fraction = Frac Int Int



instance Show Fraction where
  show :: Fraction -> String
  show (Frac a b) =
    show a
      ++ " / "
      ++ show b
instance Read Fraction where
  readsPrec :: Int -> ReadS Fraction
  readsPrec _ s =
    let [(rs0, rs1)] = lex s
        num = read rs0 :: Int
        [(rs2, rs3)] = lex rs1
        [(rs4, rs5)] = lex rs3
        den = read rs4 :: Int
     in ([(Frac num den, rs5) | rs2 == "/"])


-- Utilizando la funcion rem obtenemos el reciduo de la divicion de dos numeros enteros a y b 

m1 :: Int -> Int -> Int -> Int
m1 a b x
  | a == b = a
  | rem (a * x) b == 0 = a * x
  | otherwise = m1 a b (x + 1)

--Declaramos la funcion maximoComunMultiplo

maximoComunMultiplo :: Int -> Int -> Int
maximoComunMultiplo a b = m1 a b 1

--2  Defina las operaciones +, −, ×, / para las fracciones que definió.

--Definimos el metodo convertFraction, donde este nos ayudara a que las fracciones tengan el mismo denominador para que podamos operar de manera optima
convertFraction :: Fraction -> Int -> Fraction
convertFraction (Frac num den) mult = 
    Frac ((maximoComunMultiplo den mult `div` den) * num) (maximoComunMultiplo den mult)

--Definicion del metodo suma de fracciones
sumaFracciones :: Fraction -> Fraction -> Fraction
sumaFracciones (Frac a b) (Frac c d) =
  if b == d
    then Frac (a + c) b
    else sumaFracciones (convertFraction (Frac a b) d) (convertFraction (Frac c d) b)


--Definicion del metodo resta  de fracciones
restaFracciones :: Fraction -> Fraction -> Fraction
restaFracciones (Frac a b) (Frac c d) =
  if b == d
    then Frac (a - c) b
    else restaFracciones (convertFraction (Frac a b) d) (convertFraction (Frac c d) b)

--Definicion del metodo multiplicacion de fracciones
multiplicarFracciones :: Fraction -> Fraction -> Fraction
multiplicarFracciones (Frac a b) (Frac c d) = Frac (a * c) (b * d)

--Definicion del metodo division de fracciones
divisionFracciones :: Fraction -> Fraction -> Fraction
divisionFracciones (Frac a b) (Frac c d) = Frac (a * d) (b * c)

--Declaramos dos fracciones { fraccion1 y fraccion2 } para facilitar la ejecucion del programa
--Como ejecutar el programa
-- ghci> ejemploSuma 

fraccion1 :: Fraction
fraccion1 = read "16 / 7" :: Fraction

fraccion2 :: Fraction
fraccion2 = read "14 / 5" :: Fraction


ejemploSuma :: Fraction
ejemploSuma = sumaFracciones fraccion1 fraccion2

ejemploResta :: Fraction
ejemploResta = restaFracciones fraccion1 fraccion2

ejemploMultiplicacion :: Fraction
ejemploMultiplicacion = multiplicarFracciones fraccion1 fraccion2

ejemploDivision :: Fraction
ejemploDivision = divisionFracciones fraccion1 fraccion2

---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- Haskell Avanzado

--Escriba la función mergesort (ordenamiento por mezcla) utilizando comandos con guardia y/o listas por comprehensión.
--Utilizamos la funcion Ord que nos apoya para comparar 
mergesort :: Ord a => [a] -> [a]
mergesort xs
    | length xs < 2 = xs
    | otherwise = merge (mergesort izquierdo) (mergesort derecho)
    where
        --Utilizamos la funcion div que nos divide el length de xs en 2 para poder separar el arreglo en 2,
        -- y se los asignamos a la variable izquierdo y derecho, que seran los arreglos auxiliares que se encargaran de realizar la operacion merge por separado "Divide y venceras"
        --La funcion splitAt nos ayuda a identificar nuestro pivote.
        (izquierdo, derecho) = splitAt (length xs `div` 2) xs

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x <= y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys
