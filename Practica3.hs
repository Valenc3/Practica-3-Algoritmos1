
--Ejercicio 1
f :: Int -> Int
f x| x == 1 = 8
   | x == 4 = 131
   | x == 16 = 16 


g :: Int -> Int 
g x| x == 8 = 16
   | x == 16 = 4 
   | x == 131 = 1


h :: Int -> Int 
h x = f (g x) 


k :: Int -> Int 
k x = g (f x)


--Ejercicio 2 
absoluto :: Float -> Float 
absoluto x | x < 0 = (-1)*x
           | x >= 0 = x


maximoabsoluto :: Int -> Int -> Int
maximoabsoluto x y| x > y = x
                  | y >= x = y


maximo3 :: Int -> Int -> Int -> Int
maximo3 x y z| x > y && x > z = x
             | y > x && y > z = y
             | z > y && z > x = z 
             | otherwise = z


algunoEs0 :: Int -> Int -> Bool
algunoEs0 x y| x == 0 || y == 0 = True 

ambosSon0 :: Int -> Int -> Bool
ambosSon0 x y| x == 0 && y == 0 = True

mismoIntervalo :: Int -> Int -> Bool
mismoIntervalo x y| 3 >= x && 3 >= y = True
                  | ((x > 3) && (7 >= x)) && ((y > 3) && (7 >= y)) = True
                  | x > 7 && y > 7 = True
                  | otherwise = False  


sumaDistintos :: Int -> Int -> Int -> Int 
sumaDistintos x y z| z /= y && z /= x = x + y + z
                   | (z == y) ||( x == y ) = x + z 
                   | z == x = z + y
                   | otherwise = z 


esMultiploDe :: Int -> Int -> Bool
esMultiploDe x y| mod y x == 0 = True 
                | otherwise = False 


digitoUnidades :: Int -> Int
digitoUnidades x = mod x 10 

digitoDecenas :: Int -> Int 
digitoDecenas x = div x 10


--Ejercicio 3 

estanRelacionados :: Int -> Int -> Bool
estanRelacionados x y| x == 0 || y == 0 = False 
                     | mod x y == 0 = True 
                     | otherwise = False 


--Ejercicio 4 

prodInt :: (Int,Int) -> (Int,Int) -> Int 
prodInt (x1,y1) (x2,y2) = x1*x2 + y1*y2


todoMenor :: (Int,Int) -> (Int,Int) -> Bool
todoMenor (x1,y1) (x2,y2)| x2 > x1 && y2 > y1 = True 
                         | otherwise = False 


distanciaPuntos :: (Float,Float) -> (Float,Float) -> Float
distanciaPuntos (x1,y1) (x2,y2) = sqrt((x1-x2)^2 + (y1-y2)^2)


sumaTerna :: (Int,Int,Int) -> Int 
sumaTerna (x,y,z) = x + y + z

funcionAuxiliar1 :: Int -> Int -> Int 
funcionAuxiliar1 x y| esMultiploDe x y = y
                    | otherwise = 0 

sumarSoloMultiplos :: (Int,Int,Int) -> Int -> Int 
sumarSoloMultiplos (x,y,z) x1 = funcionAuxiliar1 x1 x + funcionAuxiliar1 x1 y + funcionAuxiliar1 x1 z


posPrimerPar :: (Int,Int,Int) -> Int
posPrimerPar (x,y,z)| even x = x
                    | even y = y 
                    | even z = z
                    | otherwise = 4
                  

crearPar :: a -> b -> (a,b)
crearPar a b = (a,b)


invertir :: (a,b) ->(b,a)
invertir (a,b) = (b,a)


--Ejercicio 5 

f2 :: Int -> Int 
f2 x| x > 7 = x*2 - 1
    | x <= 7 = x^2
     

g2 :: Int -> Int
g2 x| even x = div x 2
    | otherwise = x*3 +1 


todosMenores :: (Int,Int,Int) -> Bool
todosMenores (x,y,z)|f2 x > g2 x && f2 y > g2 y && f2 z > g2 z = True 


--Ejercicio 6 


bisiesto :: Int -> Bool
bisiesto x| mod x 4 /= 0 = False 
          | mod x 100 == 0 && mod x 400 /= 0 = False 
          | otherwise = True


--Ejercicio 7 


distanciaManhattan :: (Float,Float,Float) -> (Float,Float,Float) -> Float
distanciaManhattan (x,y,z) (x2,y2,z2) = abs(x-x2) +abs(y-y2) +abs(z-z2) 


--Ejercicio 8 


sumaUltimosDosDigitos :: Int -> Int 
sumaUltimosDosDigitos x =  mod x 10 + mod (div x 10) 10


comprar :: Int -> Int -> Int 
comprar x y| sumaUltimosDosDigitos x < sumaUltimosDosDigitos y = 1
           | sumaUltimosDosDigitos x > sumaUltimosDosDigitos y = -1
           | sumaUltimosDosDigitos x == sumaUltimosDosDigitos y = 0

         




