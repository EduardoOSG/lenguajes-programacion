factorial 0 = 1 
factorial n = n * factorial(n -1 )


mcd a 0 = a
mcd a b = 
    mcd b(mod a b)


sumI (a,b) (c,d)  =  ((a + c) , (d + b))


resI (a,b) (c,d) = ((a - c) , (b - d))


multI (a,b) (c,d) = (((a*c)-(b*d)),((a*d) + (b*c)))  


sumaArreglo [] = 0
sumaArreglo ( x : xs ) = x + sumaArreglo xs 

multiArreglo [x] = x
multiArreglo (x : y:  xs) = x * multiArreglo(y : xs)

progrecion a b = if (succ a == b ) then [ a , b] else a :( progrecion(succ a ) b)  









