{-# LANGUAGE OverloadedStrings #-}
-- https://github.com/Gabriel439/post-rfc/blob/master/sotu.md#scripting--command-line-applications

module Lib
    ( someFunc
    ) where

import Data.Yaml
import Control.Monad (mzero)
import qualified Data.ByteString.Char8 as BS

data Build = Build { language :: String
                   , install :: [String]}
             deriving (Show)


instance FromJSON Build where
    parseJSON (Object v) = Build <$>
                           -- v .: "id" <*>
                           v .: "language" <*>
                           v .: "install"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _ = mzero


strErr :: Show a => a  -> String
strErr s = "Error: " ++ (show s)

getYaml :: FilePath -> IO BS.ByteString
getYaml = BS.readFile

moo :: Build -> String
moo buildDesc = show buildDesc --(map (\r -> (language r, install r)) buildDesc)

someFunc :: IO ()
someFunc =
    do
      d <- (Data.Yaml.decodeEither <$> (getYaml "build.yml"))
      case d of
        Left err -> print $ strErr err
        Right ps -> print $ moo ps
