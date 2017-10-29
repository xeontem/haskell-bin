module Demo where
---------------------- classes of types ----------------------

class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool

-- class Enum a where
-- 	succ, pred :: a -> a
-- 	toEnum :: Int -> a
-- 	fromEnum a -> Int

five = read "5" :: Int
listFromString = reads "5 rings" :: [(Int,String)]