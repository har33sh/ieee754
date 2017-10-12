dectobinbefore :: Int -> [Int]
dectobinbefore 0 =[0]
dectobinbefore 1 =[1]
dectobinbefore num
 | mod num 2 ==0 = dectobinbefore (div num 2) ++ [0]
 | mod num 2 ==1 = dectobinbefore (div num 2) ++ [1]


dectobinafter ::(Eq a,Ord a,Num a)=> a -> [Int]
dectobinafter 0=[0]
dectobinafter num
 | num*2 >= 1 = 1:dectobinafter (num*2 -1)
 | num*2 < 1 = 0:dectobinafter (num*2)


bintodecbefore :: [Int] -> Int
bintodecbefore [] =1
bintodecbefore num = bintodecbeforeutil num 0

bintodecbeforeutil [] trail = trail
bintodecbeforeutil (x:xs) trail= bintodecbeforeutil xs (trail*2 +x)


bintodecafter ::(Fractional a) => [Int] -> a
bintodecafter [] =0
bintodecafter num = bintodecafterutil 1.0 0.0 num

bintodecafterutil ::(Fractional a)=> a-> a->[Int] ->a
bintodecafterutil  trail s []= s
bintodecafterutil  trail s (x:xs)= bintodecafterutil  (trail/2) (if x==0 then s else s+(trail/2)) xs


number1=(0,[1,0,0,0,0,0,1,0],[0,0,0,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1])
number2=(0,[1,0,0,0,0,0,1,0],[0,0,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0])
zero =(0,[0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0])
inf=(0,[1,1,1,1,1,1,1,1],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0])

--Boundary Values

iszero ::(Int,[Int],[Int])->Bool
iszero (a,b,c)=(a==0) && (b==[0,0,0,0,0,0,0,0]) && (c==[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0])

isinf :: (Int,[Int],[Int])->Bool
isinf (a,b,c)= (a==0) && (b==[1,1,1,1,1,1,1,1]) && (c==[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0])

--Gates
iand :: Int ->Int ->Int
iand 0 y =0
iand 1 y =y

ior :: Int ->Int ->Int
ior 1 y = 1
ior 0 y = y

ixor :: Int ->Int ->Int
ixor x y
 | x==y = 0
 | otherwise = 1

inot 0 = 1
inot 1 = 0

--Circuits

--Adders
-- sum carry
halfadder x y = (ixor x y,iand x y )
fulladder x y z =(ixor z (ixor x y), ior  (iand x y) (iand z (ixor x y)))
addnum x y = reverse (iadd (reverse x) (reverse y) 0)

-- when all data bits are consumed
iadd [] [] carry
 | carry ==0 =[]
 | otherwise =[1]


iadd (x:xs) (y:ys) carry = [fst z] ++ iadd xs  ys (snd z)
 where z=fulladder x y carry

--subtractors
-- diff borrow
halfsubtractor x y = (ixor x y,iand (inot x) y )
fullsubtractor x y z =(ixor z (ixor x y), ior  (iand (inot x) y) (iand z (inot (ixor x y))))
subnum x y = reverse (isub (reverse x) (reverse y) 0)

--when all bits are consumed
isub [] [] borrow
 | borrow ==0 =[]
 | otherwise =[1]

isub (x:xs) (y:ys) borrow = [fst z] ++ isub xs  ys (snd z)
 where z=fullsubtractor x y borrow

--Multipliers
prod y x = map (*x) y

imul x y =addall $ cal z (length z)
 where z=multiplier (reverse x) y

multiplier [] y = []
multiplier (x:xs) y = (prod y x):(multiplier xs y)

cal z l =[ x | a<-[0..(l-1)],x<-[(replicate (l-a) 0 )++ z!!a ++ (replicate a 0 )]]
addall arr = foldl1 addnum arr

--multiply first second
multiply (a,b,c) (p,q,r)
 | iszero (a,b,c) =zero
 | iszero (p,q,r) =zero
 | isinf (a,b,c) = inf
 | isinf (p,q,r) =inf
 | otherwise = (ixor a p,normalizeExp b q z  ,normalizeMantisa z)
   where z=imul (1:c) (1:r)

normalizeMantisa (z:zs)
 | z==0 = take 23 (drop 1 zs)
 | otherwise = take 23 (drop 1 (z:zs))

normalizeExp p q (z:zs)
 | z==0 = addnum (subnum p [0,1,1,1,1,1,1,1]) q
 | otherwise =addnum (addnum (subnum p [0,1,1,1,1,1,1,1]) q) [0,0,0,0,0,0,0,1]

icompare [] [] = 0
icompare (x:xs) (y:ys)
 | x >y = 1
 | y>x  = -1
 | x==y  = icompare xs ys







addition (a,b,c) (p,q,r)
 | iszero (a,b,c) =(p,q,r)
 | iszero (p,q,r) =(a,b,c)
 | isinf (a,b,c) = inf
 | isinf (p,q,r) =inf
 | otherwise = iaddition (a,b,c) (p,q,r)

iaddition (a,b,c) (p,q,r)
 | icompare b q == 0 = valadd (a,b,1:c) (p,q,1:r)
 | icompare b q > 0 = valadd (a,b,1:c) (shift (a,b,c) (p,q,r))
 | icompare b q < 0 = valadd (shift (a,b,c) (p,q,r)) (p,q,1:r)

shift (a,b,c) (p,q,r)
 | icompare b q > 0 =(p,b,take 24 (replicate (bintodecbefore (subnum b q) )  0 ++ [1]++ r) )
 | otherwise = (a,q,take 24 (replicate (bintodecbefore (subnum q b) )  0 ++ [1]++ c) )

--shift (a,b,c) (p,q,r)
-- | icompare b q > 0 =(p,b,take 24 ([0]++replicate (bintodecbefore (subnum b q) -1)  0 ++ [1]++ r) )
-- | otherwise = (a,q,take 24 ([0]++replicate (bintodecbefore (subnum q b) -1)  0 ++ [1]++ c) )


valadd (a,b,c) (p,q,r)
 | a==p = (a,addnum b (snd y),(fst y))
 | otherwise = valsub (a,b,c) (p,q,r)
  where y= normalize  (addnum c r) [0,0,0,0,0,0,0,0]

normalize [] val =([],val)
normalize (x:xs) val
 | length (x:xs) >24 = (take 23 xs,[0,0,0,0,0,0,0,1]) --occur only when there is a carry
 | x==0 = normalize (xs++[0]) (addnum val [0,0,0,0,0,0,0,1])
 | x==1 = (xs, val)


valsub (a,b,c) (p,q,r)
 | icompare c r == 0 = zero
 | icompare c r >0 = valminus (a,b,c) (p,q,r)
 | icompare c r <0 = valminus (p,q,r) (a,b,c)

valminus  (a,b,c) (p,q,r) = (a,subnum b (snd y),fst y)
 where y = normalize (subnum c r )  [0,0,0,0,0,0,0,0]

substract (a,b,c) (p,q,r)
 | iszero (a,b,c) =(inot p,q,r)
 | iszero (p,q,r) =(a,b,c)
 | isinf (a,b,c) = inf
 | isinf (p,q,r) =inf
 | otherwise = iaddition (a,b,c) (inot p,q,r)
