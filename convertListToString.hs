{--

Convert a linked list to a string :
For exemple :

[1,2,3]
... its string representation would be:

"1 -> 2 -> 3 -> null"
And given the following linked list:

[0,1,4,9,16]
... its string representation would be:

"0 -> 1 -> 4 -> 9 -> 16 -> null"

--}

stringify :: [Int] -> String
stringify xs =  (++ "null") . concat . map (\x -> show x ++ " -> ")
