import Data.List
import Data.Char

isIsograms :: String -> Bool
isIsograms xs = length xs == (length.group $ xs)
