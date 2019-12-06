{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib where
import           Control.Exception
import           Data.Maybe
import           Data.Time
import           Data.Time.Format
import           System.Environment
import           System.FilePath
import           System.IO
import           Text.Read
import           Text.Regex



data KernelLog = KernelLog { timestamp :: String
                           , machine   :: String
                           , uptime    :: String
                           , event     :: String
                           , message   :: String } deriving (Show)

data ParsedLog = ParsedLog { date         :: UTCTime
                           , machineName  :: String
                           , time         :: Double
                           , eventName    :: String
                           , messageReply :: String } deriving (Show)

data CpuLog = CpuLog Int String
            deriving (Show)



kernelRegex :: Regex
kernelRegex = mkRegex "(^.+[0-9]{2}:[0-9]{2}:[0-9]{2}) (\\w+) kernel: \\[([0-9]+\\.[0-9]+)\\] ([^:]+): (.+)"


cpuRegex :: Regex
cpuRegex = mkRegex "^CPU([0-9]+): (.+)$"


parseLogTime :: String -> Maybe UTCTime
parseLogTime timestamp = parseTimeM True defaultTimeLocale "%Y %b  %-d %T" timestamp :: Maybe UTCTime


readLog :: String -> Maybe KernelLog
readLog line =
    case matchRegex kernelRegex line of
        Nothing -> Nothing
        Just [ts, mach, up, e, mess] -> Just KernelLog { timestamp = ts
                                                         , machine = mach
                                                         , uptime = up
                                                         , event = e
                                                         , message = mess}


transformLog :: KernelLog -> Maybe ParsedLog
transformLog log@KernelLog { timestamp,  machine, uptime, event, message} =
    do logDate <- parseLogTime $ "2019 " ++ timestamp
       time <- readMaybe uptime :: Maybe Double
       return $ ParsedLog {date = logDate, machineName = machine, time, eventName = event, messageReply = message}


formatLog :: String -> Maybe ParsedLog
formatLog s = do kLog <- readLog s
                 transformLog kLog


getCpuLog :: ParsedLog -> Maybe CpuLog
getCpuLog log =
    case matchRegex cpuRegex $ messageReply log of
        Nothing -> Nothing
        Just [cpu, state] -> do cpuV <- readMaybe cpu :: Maybe Int
                                Just $ CpuLog cpuV state


cpuLogFilePath :: CpuLog -> FilePath -> FilePath
cpuLogFilePath (CpuLog cpu mess) dir =  dir </> show cpu  <.> ".log"


cpuLogWrite :: CpuLog -> FilePath -> IO ()
cpuLogWrite log dirr = withFile ( cpuLogFilePath log dirr) WriteMode
                             (`hPrint` log)
