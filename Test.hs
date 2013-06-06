doCases = do
          l <- getLine    
          
          if (read l) /= 42 then do
                                 putStrLn l
                                 main 
                            else return ()
main =do
      doCases

