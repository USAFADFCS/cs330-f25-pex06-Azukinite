-- pex6.hs
-- unKnot Haskell

-- name: Kenneth Wang

{- DOCUMENTATION:
   Used Gavin's test case that he put in the teams chat, sepcifically t06
-}

unKnot :: [(Char, Char)] -> String
unKnot tripCode
   | reduce tripCode == [] = "not a knot"
   | otherwise = "tangle - resulting trip code: " ++ show (reduce tripCode)

-- reduces the tripcode until no more moves
reduce :: [(Char, Char)] -> [(Char, Char)]
reduce trip
   | chooser trip == trip = trip
   | otherwise = reduce (chooser trip)

-- checks both type 1 and 2 steps or returns unchanged not really a chooser im just bad a names
-- if checks are true then removes
chooser :: [(Char, Char)] -> [(Char, Char)]
chooser trip
   | hasType1 trip = removeType1 trip
   | hasType2 trip = removeType2 trip
   | otherwise     = trip


--checks for type 1 knots
hasType1 :: [(Char, Char)] -> Bool
hasType1 [] = False
hasType1 [_] = False
hasType1 ((a,_):(b,_):xs)
   | a == b = True
   | otherwise = hasType1 ((b,'x'):xs)

-- removes type 2 knots 
removeType1 :: [(Char, Char)] -> [(Char, Char)]
removeType1 [] = []
removeType1 [x] = [x]
removeType1 ((a,t1):(b,t2):xs)
   | a == b = xs
   | otherwise = (a,t1) : removeType1 ((b,t2):xs)



-- separator between 1 and 2 stuff




-- same function ideas for the type 1 stuff just different implementation
-- uses extra functions comparatively: findoo and finduu which do as name entails
hasType2 :: [(Char, Char)] -> Bool
hasType2 trip = findOO trip && findUU trip



-- finds adjacent oo pairs
findOO :: [(Char, Char)] -> Bool
findOO [] = False
findOO [_] = False
findOO ((c1,t1):(c2,t2):xs)
   | t1 == 'o' && t2 == 'o' = True
   | otherwise = findOO ((c2,t2):xs)

-- finds adjacent uu pairs
findUU :: [(Char, Char)] -> Bool
findUU [] = False
findUU [_] = False
findUU ((c1,t1):(c2,t2):xs)
   | t1 == 'u' && t2 == 'u' = True
   | otherwise = findUU ((c2,t2):xs)

-- remove type 2 pairs
removeType2 :: [(Char, Char)] -> [(Char, Char)]
removeType2 trip = removeUU (removeOO trip)

-- removes oo paris 
removeOO :: [(Char, Char)] -> [(Char, Char)]
removeOO [] = []
removeOO [x] = [x]
removeOO ((a,'o'):(b,'o'):xs) = xs
removeOO (x:xs) = x : removeOO xs
-- removes uu pairs 
-- this errored and then fixed itself soo.....
removeUU :: [(Char, Char)] -> [(Char, Char)]
removeUU [] = []
removeUU [x] = [x]
removeUU ((a,'u'):(b,'u'):xs) = xs
removeUU (x:xs) = x : removeUU xs


--test cases
main :: IO ()
main = do
   
   let t01 = [('a','o'),('a','u')]
   print("   test case t01 - tripcode: " )
   print(t01)
   print("   result:" ++ unKnot t01)

   let t02 = [('a','o'),('a','u'),('b','u'),('b','o')]
   print("   test case t02 - tripcode: " )
   print(t02)
   print("   result:" ++ unKnot t02)

   let t03 = [('a','o'),('b','o'),('b','u'),('a','u')]
   print("   test case t03 - tripcode: " )
   print(t03)
   print("   result:" ++ unKnot t03)

   let t04 = [('a','o'),('b','o'),('c','o'),('c','u'),('b','u'),('a','u')]
   print("   test case t04 - tripcode: " )
   print(t04)
   print("   result:" ++ unKnot t04)

   let t05 = [('a','o'),('b','o'),('c','u'),('a','u'),('b','u'),('c','o')]
   print("   test case t05 - tripcode: " )
   print(t05)
   print("   result:" ++ unKnot t05)

   --gavin's test case
   let t06 = [('a','o'),('b','u'),('c','u'),('d','o'),('d','u'),('a','u'),('b','o'),('e','u'),('f','o'),('g','o'),('h','u'),('f','u'),('g','u'),('h','o'),('e','o'),('c','o')]
   print("   test case t06 - tripcode: " )
   print(t06)
   print("   result:" ++ unKnot t05)  

