{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.Char (toLower)
import Data.Functor ((<$>))
import Data.Time
import System.IO (hFlush, stdout)
import System.Random (getStdRandom, randomR)

newtype WeekDay = WeekDay Integer
                  deriving (Ord, Enum, Num)

[monday, tuesday, wednesday, thursday, friday, saturday, sunday] = [WeekDay 0 .. WeekDay 6]

instance Eq WeekDay where
  WeekDay n == WeekDay m = n `mod` 7 == m `mod` 7

instance Show WeekDay where
  show (WeekDay n)
    | n `mod` 7 == 0 = "Monday"
    | n `mod` 7 == 1 = "Tuesday"
    | n `mod` 7 == 2 = "Wednseday"
    | n `mod` 7 == 3 = "Thursday"
    | n `mod` 7 == 4 = "Friday"
    | n `mod` 7 == 5 = "Saturday"
    | n `mod` 7 == 6 = "Sunday"

readDay :: String -> Maybe WeekDay
readDay str
  | istr `elem` ["m", "mon", "monday"] = Just monday
  | istr `elem` ["tue", "tuesday"] = Just tuesday
  | istr `elem` ["w", "wed", "wednesday"] = Just wednesday
  | istr `elem` ["thu", "thursday"] = Just thursday
  | istr `elem` ["f", "fri", "friday"] = Just friday
  | istr `elem` ["sat", "saturday"] = Just saturday
  | istr `elem` ["sun", "sunday"] = Just sunday
  | otherwise = Nothing
  where istr = map toLower str


yearRange = 400
dayRange = 365 * yearRange

randomDate :: IO Day
randomDate = do
  offset <- getStdRandom $ randomR (-dayRange, dayRange)
  addDays offset . utctDay <$> getCurrentTime

anchorDay :: Day -> WeekDay
anchorDay day
  | cycle == 0 = tuesday
  | cycle == 1 = sunday
  | cycle == 2 = friday
  | cycle == 3 = wednesday
  where (year, _, _) = toGregorian day
        cycle = (year `div` 100) `mod` 4

doomsDay :: Day -> WeekDay
doomsDay day = anchorDay day - cycle
  where (year, _, _) = toGregorian day
        cycle = WeekDay $ add . (`div` 2) . add $ year `mod` 100
        add n = if odd n then n + 11 else n

weekDay :: Day -> WeekDay
weekDay day = doomsDay day + WeekDay (day `diffDays` root)
  where (year, _, _) = toGregorian day
        root = fromGregorian year 1 (if isLeapYear year then 4 else 3)

getDay :: IO WeekDay
getDay = do
  input <- getLine
  case readDay input of
   Just day -> return day
   _ -> getDay

runTest :: Integer -> Integer -> IO ()
runTest total yay = do
  putStrLn $ show yay ++ "/" ++ show total

  day <- randomDate
  putStr . (++ "? ") . show $ day
  hFlush stdout

  guess <- getDay
  case guess == weekDay day of
   True -> putStrLn "Correct!" >> next (yay + 1)
   False -> putStrLn ("Wrong! Correct was " ++ show (weekDay day)) >> next yay
   where next = runTest (total + 1)


main = runTest 0 0
