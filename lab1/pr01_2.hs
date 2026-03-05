-------------------------------------
-- Практические задание 1. Часть 2 --
-------------------------------------

module Pr01_2 where

myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x,y) : myZip xs ys

myLength:: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

mySndf2 :: (a, b) -> b
mySndf2    (x, y) = y

myFstf2 :: (a, b) -> a
myFstf2   (x, y) = x

myZipSave :: [a] -> [b] -> ([(a, b)], Either [a] [b])
myZipSave [] x = ([], Right x)
myZipSave x [] = ([], Left x)
myZipSave (x:xs) (y:ys) = (left,right) where
    left = (x,y) : myFstf2 (myZipSave xs ys)
    right = mySndf2 (myZipSave xs ys)


myUnzip :: [(a, b)] -> ([a],[b])
myUnzip [] = ([], [])
myUnzip ((x,y): xs) = ((x : myFstf2( myUnzip xs)), (y : mySndf2(myUnzip xs)))


myUnzipSave :: ([(a, b)], Either [a] [b]) -> ([a],[b])
myUnzipSave ([], Right x) = ([], x)
myUnzipSave ([], Left x) = (x, [])
myUnzipSave (((x,y): xs), z) = ((x : myFstf2 (myUnzipSave (xs, z))), (y : mySndf2 (myUnzipSave (xs, z))))

myHead :: [ls] -> ls
myHead [] = error "nothing in list"
myHead (x:_) = x

myTail :: [ls] -> [ls]
myTail [] = error "nothing in list"
myTail (_:x) = x

myFoldl1 :: (a -> a -> a) -> [a] -> a 
myFoldl1 f [] = error "nothing in list"  
myFoldl1 f [x] = x  
myFoldl1 f (x:xs) = myFoldl1 f (x `f` (myHead xs) : (myTail xs))

myFoldr1 :: (a -> a -> a) -> [a] -> a 
myFoldr1 f [] = error "nothing in list"  
myFoldr1 f [x] = x  
myFoldr1 f (x:xs) = x `f` (myFoldr1 f xs)

myReverse :: [a] -> [a]
myReverse xs = foldl (\x y -> y : x) [] xs

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile  n [] = []
myTakeWhile  n  (x:xs) = foldr (\x y -> if n x == True then x: y else []) [] (x:xs)

{-mySpan :: (a -> Bool) -> [a] -> ([a],[a])
mySpan n [] = ([],[])
mySpan n (x:xs) = foldr (AAAAAAAAAAA)-}

myMaybe:: (a -> b) -> b -> Maybe a -> b
myMaybe _ x Nothing = x
myMaybe f _ (Just x) = f x



{-allM :: (a -> Bool) -> [a] -> Bool
allM p = foldr (\x b -> p x && b) True-}

{-

Напишите реализацию функций:
-- myZipSave - попарное объединение двух списков в список пар и сохранение хвоста более длинного списка 
-- myUnzipSave - разделение списка пар на пару списков с восстановлением более длинного списка если исходные списки были разного размера
-- myReverse - разворот списка с использованием сверток
-- myFoldl1 - левая свертка для не пустых списков (без инициирующего значения)
-- myFoldr1 - правая свертка для не пустых списков (без инициирующего значения)
-- myTakeWhile - реализовать с использованием сверток
-- mySpan - реализовать с использованием сверток
-- myMaybe - обработка возможно отсутствующего значения или возвращение значение по умолчанию (maybe)
-- myMap - реализуйте функцию map с использованием типа MyList из материалов лекции
-- myUnFoldr - развертка (операция обратная к свертке)

-- Расширьте типы для выпекания тортов из материалов лекции:
    -- Добавить возможность испечь не менее трех типов тортов
    -- Контроль числа и объема используемых ингредиентов
    -- Обработку недостатка или отсутствия ингредиентов

-}
