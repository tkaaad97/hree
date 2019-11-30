{-# LANGUAGE OverloadedStrings #-}
module Data.FilePreprocess
    ( preprocessFile
    ) where

import Control.Monad (filterM)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString (break, breakEnd, concat,
                                                      lines, readFile,
                                                      stripPrefix, unlines,
                                                      unpack)
import qualified Data.FileEmbed (bsToExp)
import Data.Maybe (maybe)
import Language.Haskell.TH.Syntax (Exp, Q, Quasi(qAddDependentFile), runIO)
import System.Directory (canonicalizePath, doesFileExist, listDirectory)
import System.FilePath ((</>))

preprocessFile :: FilePath -> FilePath -> Q Exp
preprocessFile filepath includeDir = do
    bs <- runIO $ do
        content <- ByteString.readFile filepath
        let xs = ByteString.lines content
        xs' <- mapM (`preprocessLine` includeDir) xs
        return $ ByteString.unlines xs'
    qAddDependentFile filepath
    mapM_ qAddDependentFile =<< runIO (filterM doesFileExist =<< listDirectory includeDir)
    Data.FileEmbed.bsToExp bs

preprocessLine :: ByteString -> FilePath -> IO ByteString
preprocessLine line includeDir =
    flip (maybe (return line)) (ByteString.stripPrefix "#include" line) $ \remainder -> do
        let includeFile = parseIncludeFilePath remainder
        path <- canonicalizePath $ includeDir </> includeFile
        content <- ByteString.readFile path
        return $ ByteString.concat ["\n", content, "\n"]

parseIncludeFilePath :: ByteString -> FilePath
parseIncludeFilePath a =
    let (_, b) = ByteString.breakEnd (== '<') a
        (c, _) = ByteString.break (== '>') b
    in ByteString.unpack c
