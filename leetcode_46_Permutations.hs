{-- 

Given a collection of distinct integers, return all possible permutations.
Example:
   Input: [1,2,3]
   Output:
             [
              [1,2,3],
              [1,3,2],
              [2,1,3],
              [2,3,1],
              [3,1,2],
              [3,2,1]
                     ]
                     
--}


permutation :: [Int] -> [[Int]]
permutation xs = map fst $ permutation' [([], xs)]

permutation' :: [([Int], [Int])] -> [([Int], [Int])]
permutation' [] = []
permutation' [(xs, [])] = [(xs, [])]
permutation' (x : xs) = permutation' (permutation'' x) ++ permutation' xs

permutation'' :: ([Int], [Int]) -> [([Int], [Int])]
permutation'' (xs, []) = [(xs, [])]
permutation'' (xs, ys) = [(xs ++ [ys !! i], (take i ys) ++ (drop (i + 1) ys)) | i <- [0..(length ys) - 1]]






