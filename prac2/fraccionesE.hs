data Fraction = Frac Int Int

--Como operar el programa de fracciones egipcias
--El menu lo horienta de manera intuitiva, la manera en como colocar  las fracciones a operar es de la siguiente manera
--Para ingresarlas manualmente tiene que tener la forma { ghci> 2 / 7  }
--Para el arrchivo se deja un archivo con el nombre de "prueba"
--El programa da la libertad al usuario si guardar el archivo o no, el archivo se guardara en la misma carpeta donde esta contenido el proyecto

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

-- Convierte una fracción ordinaria a una fracción egipcia
fraccionAEgipcia :: Fraction -> [Fraction]
fraccionAEgipcia (Frac 0 _) = [] -- si el numerador es cero, devolver una lista vacía
fraccionAEgipcia (Frac num den) =
  --Utilizando la función Ceiling podemos redondear un número al número mas cercano
  let x = ceiling (fromIntegral den / fromIntegral num)
  --Creamos unaa fraccion nueva que tendra la siguente forma 1/x, por lo tanto es importante ir almacenando hasta llegar a una manera 1/x + ...
      nuevaFraccion = Frac 1 x
   in nuevaFraccion : fraccionAEgipcia (restaFracciones (Frac num den) nuevaFraccion)


--Recibe una lista de tipo Fraction e imprime una fracción egipcia
printEgyptianFraction :: [Fraction] -> String
printEgyptianFraction [] = ""
printEgyptianFraction [x] = show x
printEgyptianFraction (x:xs) = show x ++ " + " ++ printEgyptianFraction xs

--Funcion que lee un archivo es importate respetar la estructura mencionada al inicio del archivo

leerFraccionesArchivo :: String -> IO (Fraction, Fraction)
leerFraccionesArchivo archivo = do
  contenido <- readFile archivo
  --Utilizando la función lines nos permitira leer cada fraccion apartir de un salto de pagina ´\n´
  let [frac1, frac2] = lines contenido
  return (read frac1, read frac2)


--Almacenamos las fracciones para poder operar con ellas cuando se ingrese una opcion de menú
menu :: IO ()
menu = do
  putStrLn "¿Cómo desea ingresar las fracciones?"
  putStrLn "1. Manualmente"
  putStrLn "2. Desde un archivo"
  opcionIngreso <- getLine
  (fraccion1, fraccion2) <-
    case opcionIngreso of
      "1" -> do
        putStrLn "Ingrese la primera fracción:"
        frac1 <- getLine
        putStrLn "Ingrese la segunda fracción:"
        frac2 <- getLine
        return (read frac1, read frac2)
      "2" -> do
        putStrLn "Ingrese el nombre del archivo que contiene las fracciones:"
        archivo <- getLine
        fracciones <- leerFraccionesArchivo archivo
        return fracciones
      _ -> error "Opción inválida. Ingrese una opción válida (1, 2)."

  putStrLn "Seleccione la operación a realizar:"
  putStrLn "1. Sumar fracciones"
  putStrLn "2. Restar fracciones"
  putStrLn "3. Multiplicar fracciones"
  putStrLn "4. Dividir fracciones"
  opcion <- getLine
  let resultado =
        case opcion of
          "1" -> sumaFracciones fraccion1 fraccion2
          "2" -> restaFracciones fraccion1 fraccion2
          "3" -> multiplicarFracciones fraccion1 fraccion2
          "4" -> divisionFracciones fraccion1 fraccion2
          _ -> error "Opción inválida. Ingrese una opción válida (1, 2, 3, 4)."
  putStrLn "¿Desea guardar el resultado en un archivo?"
  putStrLn "1. Sí"
  putStrLn "2. No"
  opcionGuardado <- getLine
  case opcionGuardado of
    "1" -> do
      putStrLn "Ingrese el nombre del archivo:"
      archivo <- getLine
      appendFile archivo (show resultado ++ "\n")
    _ -> return ()
  putStrLn ("El resultado es: " ++ show resultado ++ " a tipo de fracción egipcia " ++ printEgyptianFraction (fraccionAEgipcia resultado))
