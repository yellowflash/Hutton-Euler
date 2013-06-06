-- file: WC.hs
-- Count words in stdin

main = interact wordCount
     where wordCount input = show (length input) ++ " " ++ show (length (words input)) ++ " " ++ show (length (lines input)) ++ "\n"