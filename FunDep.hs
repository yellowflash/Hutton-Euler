data Zero = Zero
data Succ n = Succ n

class Even n where
      isEven :: n -> Bool
      isEven n = True
class Odd n where 
      isOdd :: n -> Bool
      isOdd n = True

instance Even Zero
instance Odd n => Even (Succ n)
instance Even n => Odd (Succ n)