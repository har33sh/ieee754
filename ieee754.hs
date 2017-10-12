
--------------------------- Single or Double Precision ------------

exponent_bits = 7
fractional_bits = 23
----------------------      Convert number to ieee754 format       ----------------------
convert_to_ieee number = (sign,ex,frac)
                        where  sign = if number < 0 then 1 else 0
                               frac = ieee754_fractional f
                               ex = ieee754_exponent (2^exponent_bits + e -1)
                               f = head x
                               x = until_one (abs number)
                               e= if (abs number) > 1 then length x-1 else (-1) * length x +1

--get the number of divisions or multiplications required to make the number in format 1.xx * 2**y
until_one number = if number > 1 then get_one number else get_one' number

get_one number
  | number > 1 && number<2  = [number-1]
  |otherwise = get_one (number/2) ++ [number]

get_one' number
  |number>1  = [number-1]
  |otherwise = get_one' (number*2) ++ [number]


ieee754_exponent:: Int -> [Int]
ieee754_exponent x =take (exponent_bits - length binary_representation) (repeat 0) ++  binary_representation
                    where binary_representation= int_to_binary x

ieee754_fractional::(Eq a,Ord a,Num a)=> a -> [Int]
ieee754_fractional num = take fractional_bits $ fractional_to_binary num


--convert integer to binary
int_to_binary:: Int -> [Int]
int_to_binary x = if (x==0) then [] else int_to_binary (div x 2) ++ [(mod x 2)]

fractional_to_binary ::(Eq a,Ord a,Num a)=> a -> [Int]
fractional_to_binary 0=[0]
fractional_to_binary num
 | num*2 >= 1 = 1:fractional_to_binary (num*2 -1)
 | num*2 < 1 = 0:fractional_to_binary (num*2)

---------------------    Convert ieee754 to human readable format   ----------------------

convert_from_ieee (sign,ex,frac) =((-1)^sign) * (2**(binary_to_dec ex-(2^exponent_bits -1)))  *(1+ binary_to_frac frac)

--convert binary to decimal
binary_to_dec :: ( Num a) => [Int] -> a
binary_to_dec x = sum $map snd $ filter ((==1).fst) $zip (reverse x) $map (2^) [0,1..]

--convert binary to fractional
binary_to_frac :: (Fractional a) => [Int] -> a
binary_to_frac frac = sum $map snd$ filter ((==1).fst)  $zip frac $ map (1/) $map (2^) [1..]

----------------------------------------------------------------------------------------------------
