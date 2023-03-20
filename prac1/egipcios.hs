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



main :: IO ()
main = do
    putStrLn "Conversion de numeros !"
    putStrLn "Por favor ingrese una cadena con las operaciones que desea realizar:"
    input <- getLine 
    --operaciones nesesarias
    putStrLn "Seleccione una operación:"
    putStrLn "1. Sumar"
    putStrLn "2. Restar"
    putStrLn "3. Multiplicar"
    putStrLn "4. Dividir"
    choice <- getLine
    let result = case choice of
            "1" -> sumar input
            "2" -> restar input
            "3" -> multiplicar input
            "4" -> dividir input
            _ -> "Opción inválida"
    putStrLn ("El resultado es: " ++ result)

sumar :: String -> String
sumar input = undefined -- Aquí iría la implementación de la función de suma

restar :: String -> String
restar input = undefined -- Aquí iría la implementación de la función de resta

multiplicar :: String -> String
multiplicar input = undefined -- Aquí iría la implementación de la función de multiplicación

dividir :: String -> String
dividir input = undefined -- Aquí iría la implementación de la función de división
