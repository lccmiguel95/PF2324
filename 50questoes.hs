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
concat_Group :: [[a]] -> [a]
concat_Group [] = []
concat_Group (x:xs) = x ++ concat_Group xs

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

-- 27.
remove_Elem :: Eq a => a -> [a] -> [a]
remove_Elem _ [] = []
remove_Elem n (x:xs)
   | n == x = xs
   | otherwise = x : remove_Elem n xs

-- 28.
remove_Occur :: Eq a => [a] -> [a] -> [a]
remove_Occur [] _ = []
remove_Occur list [] = list
remove_Occur (x:xs) (y:ys)
   | x == y = remove_Occur xs ys
   | otherwise = x : remove_Occur xs (y:ys)

-- 29.
concat_Union :: Eq a => [a] -> [a] -> [a]
concat_Union [] b = b
concat_Union a [] = a
concat_Union (x:xs) (y:ys)
   | x == y = x : concat_Union xs ys
   | otherwise = x : concat_Union xs (y:ys)

-- 30.
check_Elem :: Eq a => a -> [a] -> Bool
check_Elem _ [] = False
check_Elem n (x:xs)
   | n == x = True
   | otherwise = check_Elem n xs 

remove_Intersect :: Eq a => [a] -> [a] -> [a]
remove_Intersect [] _ = []
remove_Intersect _ [] = []
remove_Intersect (x:xs) list
   | (check_Elem x list) = x : remove_Intersect xs list
   | otherwise = remove_Intersect xs list 

-- 31.
insert_Elem :: Ord a => a -> [a] -> [a]
insert_Elem n [] = [n]
insert_Elem n (x:xs)
   | n < x = n : x : xs 
   | otherwise = x : insert_Elem n xs 

-- 32.
unwords_Unlist :: [String] -> String
unwords_Unlist [] = ""
unwords_Unlist (x:[]) = x
unwords_Unlist (x:xs) = x ++ " " ++ unwords_Unlist xs

-- 33.
unlines_Unlist :: [String] -> String
unlines_Unlist [] = ""
unlines_Unlist (x:xs) = x ++ "\n" ++ unlines_Unlist xs

-- 34.
pMaior :: Ord a => [a] -> Int
pMaior [] = error "Empty List"
pMaior (x:xs) = maior_aux 0 0 (x:xs) where
      maior_aux _ pos [] = pos
      maior_aux cnt pos (h:[]) = pos
      maior_aux cnt pos (h:z:zs) = if (h >= z)
                                   then maior_aux (cnt + 1) pos (h:zs)
                                   else maior_aux (cnt + 1) (cnt + 1) (z:zs)

-- 37.
iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (h:[]) = [h]
iSort (h:x:xs)
   | h >= x = x : iSort (h:xs)
   | otherwise = h : iSort (x:xs)