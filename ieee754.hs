
exponent_bits = 7
fractional_bits = 23
----------------------      Convert number to ieee754 format       ----------------------
convert_to_ieee number = (sign,ex,frac)
                        where  sign = if number < 0 then 1 else 0
                               frac = ieee754_fractional f
                               f = head x
                               x = until_one number
                               ex = ieee754_exponent (2^exponent_bits - length x)


until_one number = if number>1 then [number-1] else until_one (number*2) ++ [number]


ieee754_exponent:: Int -> [Int]
ieee754_exponent x =take (exponent_bits - length binary_representation) (repeat 0) ++  binary_representation
                    where binary_representation= int_to_binary x

ieee754_fractional::(Eq a,Ord a,Num a)=> a -> [Int]
ieee754_fractional num = take fractional_bits $ fractional_to_binary num


--convert integer to binary
int_to_binary:: Int -> [Int]
int_to_binary x = if (x==0) then [0] else int_to_binary (div x 2) ++ [(mod x 2)]

fractional_to_binary ::(Eq a,Ord a,Num a)=> a -> [Int]
fractional_to_binary 0=[0]
fractional_to_binary num
 | num*2 >= 1 = 1:fractional_to_binary (num*2 -1)
 | num*2 < 1 = 0:fractional_to_binary (num*2)

---------------------    Convert ieee754 to human readable format   ----------------------

convert_from_ieee (sign,ex,frac) =((-1)^sign) * (2**(binary_to_dec ex-127))  *(1+ binary_to_frac frac)
--convert binary to integer

binary_to_dec :: ( Num a) => [Int] -> a
binary_to_dec x = sum $map snd $ filter ((==1).fst) $zip (reverse x) $map (2^) [0,1..]

binary_to_frac :: (Fractional a) => [Int] -> a
binary_to_frac frac = sum $map snd$ filter ((==1).fst)  $zip frac $ map (1/) $map (2^) [1..]
