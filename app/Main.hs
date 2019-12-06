module Main where
import           Control.Exception
import           Control.Monad
import           Data.Maybe
import           Lib
import           System.Environment
import           System.IO

main :: IO ()
main = do (inFile:outDirectory:_) <- getArgs
          withFile inFile ReadMode
                  (\inHandle -> do content <- hGetContents inHandle
                                   let rows = lines content
                                       cLogs = mapMaybe (formatLog >=> getCpuLog) rows
                                   mapM_ (`cpuLogWrite` outDirectory) cLogs
                  )
