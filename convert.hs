











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
