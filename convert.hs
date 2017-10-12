
doubleToBinary value = if  value > 0
                          then   getX  (abs value) 0
                          else   getX  (abs value) 0

getX value count
      | value > 1 && value <2 = return  $ decToBinary (127+count) ++ (take 24 $fracToBinary (value-1))
      | value > 1 = getX (value/2) (count +1 )
      | otherwise = getX (value*2) (count-1)


decToBinary :: Int -> [Int]
decToBinary 0 =[0]
decToBinary 1 =[1]
decToBinary num
 | mod num 2 ==0 = decToBinary (div num 2) ++ [0]
 | mod num 2 ==1 = decToBinary (div num 2) ++ [1]


fracToBinary ::(Eq a,Ord a,Num a)=> a -> [Int]
fracToBinary 0=[0]
fracToBinary num
 | num*2 >= 1 = 1:fracToBinary (num*2 -1)
 | num*2 < 1 = 0:fracToBinary (num*2)
--
-- binaryToDouble :: Fractional a => [Int] -> a
binaryToDouble xs =  getY xs
--



















getY xs = 2^(binaryToDec (take 8 xs) -127)

getZ xs = 2^ getY xs

binaryToDec x = sum $map snd $ filter ((==1).fst) $zip (reverse x) $map (2^) [0,1..10]























binaryToFrac ::(Fractional a) => [Int] -> a
binaryToFrac [] =0
binaryToFrac num = binaryToFracutil 1.0 0.0 num

binaryToFracutil ::(Fractional a)=> a-> a->[Int] ->a
binaryToFracutil  trail s []= s
binaryToFracutil  trail s (x:xs)= binaryToFracutil  (trail/2) (if x==0 then s else s+(trail/2)) xs
