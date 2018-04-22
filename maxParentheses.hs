{--

Given a string containing just the characters '(' and ')', find
the length of the longest valid (well-formed) parentheses substring.

Example 1:

Input: "(()"
Output: 2
Explanation: The longest valid parentheses substring is "()"

Example 2:

Input: ")()())"
Output: 4
Explanation: The longest valid parentheses substring is "()()"

--}

-- Our method is using stack.
maxParentheses :: [Char] -> Int
maxParentheses [] = 0
maxParentheses s = (*2).maximum $ maxParentheses' s 0 [' '] [0]

maxParentheses' :: [Char] -> Int -> [Char] -> [Int] -> [Int]
maxParentheses' s i (x:xs) (y:ys)
   | cond0 = (y:ys)
   | cond1 = maxParentheses' s (i+1) (['('] ++ (x:xs)) (y:ys)
   | cond2 = if x == '(' then maxParentheses' s (i+1) xs (y+1:ys)
                         else maxParentheses' s (i+1) [' '] [0]++(y:ys)
   | otherwise = error "Invalid input !"
   where n = length s
         cond0 = i == n
         cond1 = s !! i == '(' && i < n
         cond2 = s !! i == ')' && i < n
