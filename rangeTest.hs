-- ranges
import Data.List

data Range = Range Integer Integer

valid :: Range -> Bool
valid (Range x y) = x <= y

validRanges :: [Range] -> [Range]
validRanges = filter valid

toRange :: Integer -> Integer -> Range
toRange x y = Range x y

instance Eq Range where
    (==) (Range a b) (Range c d) = a == c && b == d

instance Ord Range where
    compare (Range a b) (Range c d)
        | a == c    = compare b d
        | otherwise = compare a c


instance Show Range where
    show (Range a b) = pad . concat . space . top . (map show) $ [a,b] where
        pad s = "(" ++ s ++ ")"
        space = intersperse " "
        top = (["R"] ++)

unique :: (Ord a) => [a] -> [a]
unique = unique' . sort

unique' :: (Eq a) => [a] -> [a]
unique' [] = []
unique' [a] = [a]
unique' (a:b:xs)
    | a == b    = a : unique' xs
    | otherwise = a : unique' (b:xs)

totalRanges :: [Range] -> [Range]
totalRanges [] = []
totalRanges [x] = [x]
totalRanges ((Range a b):(Range c d):xs)
    | b >= c    = totalRanges ((Range a (max b d)) : xs) -- a = min a c by unique
    | otherwise = (Range a b) : totalRanges ((Range c d) : xs)

testList = [(7,19),(14,17),(4,13),(19,1),(7,5),(17,10),(13,14),(11,5),
            (8,3),(19,4),(0,5),(2,19),(13,15),(19,10),(3,18),(3,16),
            (4,10),(10,19),(9,11),(18,9),(1,5),(12,13),(8,6),(16,8),
            (9,4),(15,2),(16,2),(4,4),(10,2),(3,14)]

toBig :: (Integer, Integer) -> Bool
toBig (x,y) = (6<) . abs $ y - x

main = print . totalRanges . unique . validRanges . (map $ uncurry toRange) . (filter $ not . toBig) $ testList
-- main = do
    -- print testList
    -- step1 <- return $ (filter $ not . toBig) testList
    -- print step1
    -- step2 <- return $ (map $ uncurry toRange) step1
    -- print step2
    -- step3 <- return $ validRanges step2
    -- print step3
    -- step4 <- return $ unique step3
    -- print step4
    -- step5 <- return $ totalRanges step4
    -- print step5
