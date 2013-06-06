main = do
     contents <- getContents
     print (sumAll contents)
     where sumAll = sum . map read . words
     