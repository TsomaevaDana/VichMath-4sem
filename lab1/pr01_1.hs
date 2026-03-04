-------------------------------------
-- Практические задание 1. Часть 1 --
-------------------------------------

module Pr01_1 where


myHead :: [ls] -> ls
myHead [] = error "nothing in list"
myHead (x:_) = x

myTail :: [ls] -> [ls]
myTail [] = error "nothing in list"
myTail (_:x) = x

myLast :: [ls] -> ls
myLast [] = error "nothing in list"
myLast [x] = x
myLast (_:xs) = myLast (myTail xs)

myTake :: Int -> [ls] -> [ls]
myTake n [] | n > 0 = error "too big number"
myTake n _ | n < 0 = error "bad number"
myTake _ [] = []
myTake 0 _ = []
myTake n (x:xs) = (x) : myTake (n-1) xs

myDrop :: Int -> [ls] -> [ls]
myDrop n [] | n > 0 = error "too big number"
myDrop n _ | n < 0 = error "bad number"
myDrop _ [] = []
myDrop 0 x = x
myDrop n (x:xs) = myDrop (n-1) xs

myProduct :: [Int] -> Int
myProduct [] = error "nothing in list"
myProduct [x] = x
myProduct (x:xs) = x * myProduct xs

myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x,y) : myZip xs ys

myZip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
myZip3 [] _ _ = []
myZip3 _ [] _ = []
myZip3 _ _ [] = []
myZip3 (x:xs) (y:ys) (z:zs) = (x,y,z) : myZip3 xs ys zs

mySndf2 :: (a, b) -> b
mySndf2    (x, y) = y

myFstf2 :: (a, b) -> a
myFstf2   (x, y) = x


myUnzip :: [(a, b)] -> ([a],[b])
myUnzip [] = ([], [])
myUnzip ((x,y): xs) = ((x : myFstf2( myUnzip xs)), (y : mySndf2(myUnzip xs)))




myThrd :: (a,b,c) -> c
myThrd (x,y,z) = z

mySnd :: (a,b,c) -> b
mySnd (x,y,z) = y

myFst :: (a,b,c) -> a
myFst (x,y,z) = x
{-

Напишите реализацию функций myFST, mySND, myTHRD для кортежа (a,b,c)


Напишите реализацию стандартных функции для работы со списками:
-- myHead - определение (через сопоставление с образцом) функции отделения головы списка
-- myTail - функция отделения хвоста списка
-- myTake - взять первые n элементов списка
-- myDrop - отбросить первые n элементов списка
-- myProduct - перемножить все элементы списка
-- myZip - попарное объединение двух списков в список пар, длина итогового списка по длине самого короткого из входных списков
-- myZip3 объединение трех списков в список троек
-- myUnzip - разделение списка пар на пару списков

Напишите реализацию стандартных функции высшего порядка для работы со списками:
-- myFilter - применение предиката к каждому элементу списка (две реализации: с использованием охранных выражений и if-then-else)
-- myMap - применение функции одного аргумента к каждому элементу списка
-- myZipWith - применение функции двух аргументов к двум спискам
-- myZipWith3 - применение функции трех аргументов к трем спискам
-- myAll - проверяет удовлетворяют ли все элементы списка предикату
-- myAny - проверяет удовлетворяют ли хотя бы один элемент списка предикату
-- myComposition - композиция двух функций (.)

-}

myFilter1 :: (a -> Bool) -> [a] -> [a]
myFilter1 _ [] = []
myFilter1 n (x:xs) | (n x) == True = x : myFilter1 n xs
                    | otherwise = myFilter1 n xs

myFilter2 :: (a -> Bool) -> [a] -> [a]
myFilter2 _ [] = []
myFilter2 n (x:xs) = if  (n x) == True 
                    then x : myFilter2 n xs
                    else myFilter2 n xs

myMap :: (a -> a) -> [a] -> [a]
myMap _ [] = []
myMap n (x:xs) = n x : myMap n xs

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith n (x:xs) (y:ys) = n x y : myZipWith n xs ys

myZipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
myZipWith3 _ [] _ _ = []
myZipWith3 _ _ [] _ = []
myZipWith3 _ _ _ [] = []
myZipWith3 n (x:xs) (y:ys) (z:zs) = n x y z: myZipWith3 n xs ys zs

myAll :: (a -> Bool) -> [a] -> Bool
myAll _ [] = True
myAll n (x:xs) | (n x) == True =  myAll n xs
                    | otherwise = False
                    
myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny n (x:xs) | (n x) == True =  True
                    | otherwise = myAny n xs

myComposition :: (b -> c) -> (a -> b) -> a -> c
myComposition x y z = x ( y ( z ))