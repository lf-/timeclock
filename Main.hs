module Main where
import Control.Concurrent
import Control.Monad
import Data.Time
import Data.Time.LocalTime
import Data.Fixed
import System.IO
import System.Posix.Signals


printableTime :: FormatTime t => t -> String
printableTime timeDiff =
    formatTime defaultTimeLocale "%02H:%02M:%02S" timeDiff


getElapsed :: ZonedTime -> ZonedTime -> NominalDiffTime
getElapsed now since =
    diffLocalTime (zonedTimeToLocalTime now) (zonedTimeToLocalTime since)


toHours :: NominalDiffTime -> Pico
toHours time =
    nominalDiffTimeToSeconds time / 3600


data OutputRecord = OutputRecord {
    timeStart :: ZonedTime,
    timeEnd   :: ZonedTime,
    hours     :: Pico
} deriving (Show)


-- I really want to make this generic but I failed.
truncateNum :: Real n => n -> n -> n
truncateNum prec num =
    fromIntegral (num `div'` prec) * prec


formatOutputRecord :: OutputRecord -> String
formatOutputRecord record =
    let start = formatTime defaultTimeLocale "%02H:%02M" $ timeStart record
        startDate = formatTime defaultTimeLocale "%d-%b" $ timeStart record
        end   = formatTime defaultTimeLocale "%02H:%02M" $ timeEnd record
        workedHours = showFixed True $ truncateNum 0.01 $ hours record
    in startDate ++ "\t" ++ start ++ "-" ++ end ++ "\t" ++ workedHours


getHoursDone :: ZonedTime -> IO OutputRecord
getHoursDone since = do
    pending <- getPendingSignals

    now <- getZonedTime
    let elapsed = getElapsed now since

    if not $ inSignalSet keyboardSignal pending then do
        putStr $ "\rElapsed: " ++ printableTime elapsed
        hFlush stdout
        threadDelay $ 10^6
        getHoursDone since
    else
        return OutputRecord { timeStart = since, timeEnd = now, hours = toHours elapsed }


main :: IO ()
main = do
    t_start <- getZonedTime
    blockSignals $ addSignal keyboardSignal emptySignalSet
    putStrLn $ "Started " ++ formatTime defaultTimeLocale "%c" t_start
    record <- getHoursDone t_start
    putStrLn ""
    putStrLn $ formatOutputRecord record


