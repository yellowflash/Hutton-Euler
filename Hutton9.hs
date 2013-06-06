{--
        Conway's game of life
-}
goto (x,y) = putStr ("\ESC[" ++ (show y) ++ ";" ++ (show x) ++ "H")
cls = putStr "\ESC[2J"
width = 40
height = 40

wrap (x,y) = (((x-1) `mod` (width)) +1, ((y-1) `mod` (height)) +1)
neighbours (x,y) = map wrap [(x-1,y-1),(x-1,y),(x-1,y+1),(x,y-1),(x,y+1),(x+1,y-1),(x+1,y),(x+1,y+1)]

numOfNeighbours b = (length . filter (\x -> elem x b) . neighbours)
isAlive b p = noOfNeighbours == 3 || noOfNeighbours == 2
              where noOfNeighbours = numOfNeighbours b p

births b = [(x,y) | x <- [1..width], y <- [1..height],  (not (elem (x,y) b)) && (numOfNeighbours b (x,y)) == 3]
survivors b = [p | p <- b, isAlive b p]
nextgeneration b = (births b) ++ (survivors b)

pointAt p = do
            goto p
            putStr "#"

seqn [] = return ()
seqn (x:xs) = do 
              x
              seqn xs 

printBoard b = do seqn [pointAt p | p <- b]

glider = [(4, 2), (2, 3), (4, 3), (3, 4), (4, 4)]

wait = do seqn [return () | _ <- [1..5000]]

nextGen b = do 
               cls
               printBoard b
               goto (width+1,height+1)
               putStrLn "Next Gen"
               wait
               nextGen (nextgeneration b)



main = do
       nextGen glider