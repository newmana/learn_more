-- From http://mikehadlow.blogspot.com.au/2011/05/dependency-injection-haskell-style.html
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

data Report = Report Int String deriving (Show)
-- talk to database
type GetReport = Int -> IO Report
-- talk to email
type SendReport = Report -> IO ()
-- process report
type ProcessReport = Int -> IO ()

getReport :: GetReport
getReport id = return $ Report id "Hello"

sendReport :: SendReport
sendReport report = putStrLn $ show report

processReport :: GetReport -> SendReport -> ProcessReport
processReport get send id = do
	r <- get id
	send r

b = processReport getReport sendReport 3

-- type class resolve
class Resolvable a where
	resolve :: a

instance Resolvable GetReport where 
	resolve = getReport

instance Resolvable SendReport where
	resolve = sendReport

instance Resolvable ProcessReport where
	resolve = processReport resolve resolve

let p = resolve :: ProcessReport
