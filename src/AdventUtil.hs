{-# LANGUAGE CPP #-}

module AdventUtil where
import System.FilePath

inputFilePath :: FilePath -> FilePath
inputFilePath file = (takeDirectory $ takeDirectory __FILE__) </> "input" </> file
