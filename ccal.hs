import Data.List (find, intercalate)
import Data.Time.Calendar
import Data.Time.Format (defaultTimeLocale, months)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitFailure))
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)

dayOfWeekNum :: Num a => Day -> a
dayOfWeekNum day = fromIntegral (fromEnum (dayOfWeek day) `rem` 7)
-- fromEnum Monday = 1, fromEnum Sunday = 7. But I want 0 for Sunday.

-- Compute the nearest Sunday on or before day.
floorSunday :: Day -> Day
floorSunday day = addDays (- (dayOfWeekNum day)) day

-- Compute the nearest Saturday on or after day.
ceilingSaturday :: Day -> Day
ceilingSaturday day = addDays (6 - dayOfWeekNum day) day

-- Compute the contiguous calendar in string form, ready for putStr.
calendar :: Integer -> Int -> Integer -> String
calendar year month1 count = unlines (map makeWeek sundays)
  where
    day1 = fromGregorian year month1 1
    sunday1 = floorSunday day1
    day' = pred (addGregorianMonthsClip count day1)
           -- Eg if day1 = 2021 May 1, add "4 months" = Sep 1, pred -> Aug 31
           -- (btw addGregorianMonthsClip and addGregorianMonthsRollOver agree
           -- on this, they differ when eg May 31 + 4 months = "Sep 31", clip to
           -- Sep 30? rollover to Oct 1?)
    sundays = [sunday1, addDays 7 sunday1 .. day']

    makeWeek sunday = monthName ++ "  " ++ intercalate "  " (map showDay dmns)
      where
        thisweek = [sunday .. addDays 6 sunday `min` day']
        dmns = map (\d -> case toGregorian d of (_,m,n) -> (d,m,n)) thisweek
        inRange d = d >= day1 && d <= day'
        monthName = case dmns of
          (_, m, n) : _ | n <= 7 -> snd (months defaultTimeLocale !! (m - 1))
          _ -> "   "
        showDay (d, _, n) | not (inRange d) = "  "
                          | n < 10 = " " ++ show n
                          | otherwise = show n

data MaybeArgs = NeedHelp
               | NoParse
               | Args Integer Int Integer -- year, 1st month, how many months

main = do
    argsmay <- fmap parseArgs getArgs
    case argsmay of
      NeedHelp -> sendHelp
      NoParse -> do
          hPutStrLn stderr "Illegal arguments. Please see '--help'."
          exitWith (ExitFailure 1)
      Args year month1 count -> putStr (calendar year month1 count)

parseArgs [] = NeedHelp
parseArgs (h : _) | h `elem` ["-h", "-?", "--help"] = NeedHelp
parseArgs (s1 : s2 : s3 : _)
  | Just y <- readMaybe s1
  , Just m1 <- readMaybe s2
  , m1 >= 1, m1 <= 12
  , Just m' <- readMaybe s3
  , m' >= 0 = Args y m1 m'
parseArgs _ = NoParse

sendHelp = putStrLn
  "Prints contiguous calendar over a month range.\n\
  \\n\
  \Syntax: ccal year first-month count-months\n\
  \first-month is numeric, e.g., 1 for Jan, 12 for Dec\n\
  \Example: ccal 2021 5 4\n\
  \(from 2021 May, print 4 months)"
