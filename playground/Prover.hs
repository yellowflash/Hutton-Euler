import Data.Set as Set
import Data.Map as Map
import Data.Maybe

data Preposition = Implies Preposition Preposition |
                   Not Preposition |
                   Variable String  deriving  Show


type Valuation = Map String Bool


vocabulary :: Preposition -> Set String
vocabulary (Implies a b) = Set.union (vocabulary a) (vocabulary b)
vocabulary (Not a) = vocabulary a
vocabulary (Variable a) = Set.singleton a


evaluate :: Valuation -> Preposition -> Bool
evaluate valuation (Implies a b) = (not (evaluate valuation a)) || (evaluate valuation b)
evaluate valuation (Not a) = not (evaluate valuation a)
evaluate valuation (Variable a) = fromMaybe True $ Map.lookup a valuation


allValuations :: Preposition -> [Valuation]
allValuations preposition = allValuationsFor (Set.toList (vocabulary preposition))
                            where allValuationsFor [] = [Map.empty]
                                  allValuationsFor (x:xs) = (Prelude.map (Map.insert x True) restOfValuations) ++ (Prelude.map (Map.insert x False) restOfValuations)
                                                            where restOfValuations = allValuationsFor xs

valid :: Preposition -> Bool
valid preposition = Prelude.foldr (\x s -> s && (evaluate x preposition)) True (allValuations preposition)

