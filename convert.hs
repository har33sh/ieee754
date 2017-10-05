doubleToBinary value = if  value > 0
                          then  [0] : getX  (abs value) 0
                          else  [1] : getX  (abs value) 0

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
