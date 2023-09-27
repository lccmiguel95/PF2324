-- 1.
enum_FromTo :: Int -> Int -> [Int]
enum_FromTo x y
   | x <= y = x : enum_FromTo (x+1) y
   | otherwise = []

-- 2.

-- 3.
concat_Lists :: [a] -> [a] -> [a]
concat_Lists x [] = x
concat_Lists [] y = y 
concat_Lists (x:xs) y = x : concat_Lists xs y

-- 4.
calc_Elem :: [a] -> Int -> a
calc_Elem [] _ = error "The list has no elements!"
calc_Elem (x:xs) 0 = x
calc_Elem (h:t) x = calc_Elem t (x-1)

-- 5.
revert_List :: [a] -> [a]
revert_List [] = []
revert_List (x:xs) = concat_Lists (revert_List xs) [x]

-- 6.
remove_Elems :: Int -> [a] -> [a]
remove_Elems _ [] = []
remove_Elems 0 lis = []
remove_Elems n (x:xs) = x : remove_Elems (n-1) xs

-- 8.
my_Zip :: [a] -> [b] -> [(a,b)]
my_Zip _ [] = []
my_Zip [] _ = []
my_Zip (x:xs) (y:ys) = (x,y) : my_Zip xs ys

-- 9.
replicate_Element :: Int -> a -> [a]
replicate_Element 0 _ = []
replicate_Element num elem = elem : replicate_Element (num-1) elem

-- 10.
between_Element :: a -> [a] -> [a]
between_Element _ (x:[]) = [x]
between_Element elem (x:xs) = x : elem : between_Element elem xs 

-- 11.
group_Lists :: Eq a => [a] -> [[a]]
group_Lists [] = [[]]
group_Lists (h:(x:xs)) =  group_aux [h] h (x:xs) where
    group_aux acc _ [] = [acc]
    group_aux acc h (x:xs)
      | h == x = group_aux (h : acc) h xs
      | otherwise = acc : group_aux [x] x xs 

-- 12.
concat_List :: [[a]] -> [a]
concat_List [] = []
concat_List (x:xs) = x ++ concat_List xs

-- 13.
calculate_Prefixes :: [a] -> [[a]]
calculate_Prefixes [] = []
calculate_Prefixes (x:xs) = calc_aux [] (x:xs) where
  calc_aux acc [] = [acc]
  calc_aux acc (x:xs) = acc : calc_aux (acc ++ [x]) xs

-- 14.
calculate_Suffixes :: [a] -> [[a]]
calculate_Suffixes (x:[]) = [[x], []]
calculate_Suffixes (x:xs) = (x:xs) : calculate_Suffixes xs

-- 15.
heads :: [[a]] -> [a]
heads [] = []
heads ([]:xs) = heads xs
heads (x:xs) = head x : heads xs 

-- 16.
count_Elems :: [a] -> Int
count_Elems [] = 0
count_Elems (x:xs) = 1 + count_Elems xs 

total :: [[a]] -> Int 
total [] = 0
total ([]:xs) = total xs
total (x:xs) = count_Elems x + total xs 

-- 17.
made_Triples :: [(a,b,c)] -> [(a,c)]
made_Triples [] = []
made_Triples ((x,xy,xz):xs) = (x, xz) : made_Triples xs

--18.
bound_Strings :: [(String,b,c)] -> String
bound_Strings [] = ""
bound_Strings ((x,xy,xz):xs) = x ++ bound_Strings xs

-- 19.
same_Age :: Int -> Int -> [(String,Int)] -> [String]
same_Age _ _ [] = []
same_Age year num ((name, yr):xs)
   | year - yr >= num = name : same_Age year num (xs)
   | otherwise = same_Age year num (xs)

-- 20.
powerEnumFrom :: Int -> Int -> [Int]
powerEnumFrom n m
   | m < 0 = []
   | otherwise = powerEnumFrom n (m - 1) ++ [n ^ m]

-- 21.

-- 22.
prefix_Of :: Eq a => [a] -> [a] -> Bool
prefix_Of [] _ = True
prefix_Of (x:xs) [] = False
prefix_Of (x:xs) (y:ys)
   | x == y = prefix_Of xs ys
   | otherwise = False

-- 23.
suffix_Of :: Eq a => [a] -> [a] -> Bool
suffix_Of x y = prefix_Of (reverse x) (reverse y)

-- 24.
occur_In_Order :: Eq a => [a] -> [a] -> Bool
occur_In_Order [] _ = True
occur_In_Order list [] = False
occur_In_Order (x:xs) (y:ys)
   | x == y = occur_In_Order xs ys
   | otherwise = occur_In_Order (x:xs) ys 

-- 25.
occurs_In_List :: Eq a => a -> [a] -> [Int]
occurs_In_List _ [] = []
occurs_In_List elem (x:xs) = occurs_aux 0 elem (x:xs)
  where 
   occurs_aux acc elem [] = []
   occurs_aux acc elem (x:xs) = if (elem == x)
                                then acc : occurs_aux (acc + 1) elem xs
                                else occurs_aux (acc + 1) elem xs

-- 26.

exists_In_List :: Eq a => a -> [a] -> Bool
exists_In_List _ [] = False
exists_In_List elem (x:xs)
   | elem == x = True
   | otherwise = exists_In_List elem xs 


remove_Repeated :: Eq a => [a] -> [a]
remove_Repeated [] = []
remove_Repeated (x:xs) = remove_aux [] (x:xs)
   where 
      remove_aux acc [] = acc
      remove_aux acc (x:xs) = if (exists_In_List x acc)
                              then remove_aux acc xs
                              else remove_aux (concat_Lists acc [x]) xs 

