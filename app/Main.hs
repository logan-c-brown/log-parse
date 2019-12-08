{-# LANGUAGE DeriveDataTypeable #-}
module Main where
import           Control.Exception
import           Control.Monad
import           Data.Maybe
import           Lib
import           System.Environment
import           System.IO
import           Data.Typeable
import           System.Directory

data ProgramException = InputException String
                      deriving (Show, Typeable)

instance Exception ProgramException

parseMainArgs :: [String] -> IO (Either ProgramException (FilePath, FilePath))
parseMainArgs args = case args of
    []                           -> return $ Left $ InputException "No Arguments Provided"
    [something]                  -> return $ Left $ InputException "Only One Argument Provided"
    (inFile:outDir:something:_)  -> return $ Left $ InputException "Too Many Command Arguments"
    [inFile,outDir]  -> do 
      fileExists      <- doesFileExist inFile
      directoryExists <- doesDirectoryExist outDir
      return $ case (fileExists, directoryExists) of
        (True,  True)  -> Right (inFile, outDir)
        (False, True)  -> Left $ InputException $ "Input File: " ++ inFile ++ " does not exist"
        (True,  False) -> Left $ InputException $ "Output Directory: " ++ outDir ++ " does not exist"
        (False, False) -> Left $ InputException $ "Input File: " ++ inFile ++ " does not exist" 
                                                    ++ " and Output Directory: " ++ outDir ++ " does not exist"



main :: IO ()
main = do 
    args        <- getArgs
    parsedArgs  <- parseMainArgs args
    case parsedArgs of
      Left e                 -> putStrLn "Input Error" >> throw e
      Right (inFile, outDir) -> withFile inFile ReadMode (processFile outDir)
  where 
    processFile dir inHandle = do
        content <- hGetContents inHandle
        let rows = lines content
            cLogs = mapMaybe (readLog >=> transformLog >=> getCpuLog) rows
        mapM_ (`cpuLogWrite` dir) cLogs
