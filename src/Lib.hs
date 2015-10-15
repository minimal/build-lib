{-# LANGUAGE OverloadedStrings #-}
-- https://github.com/Gabriel439/post-rfc/blob/master/sotu.md#scripting--command-line-applications

module Lib
    ( someFunc
    ) where

import qualified Data.Yaml as Yaml
import Data.Yaml ((.:), (.:?))
import Control.Monad (mzero)
import qualified Data.ByteString.Char8 as BS

data Build = Build { language :: String
                   , install :: [String]
                   , blurb :: Maybe String}
             deriving (Show)


instance Yaml.FromJSON Build where
    parseJSON (Yaml.Object v) = Build <$>
                                v .: "language" <*>
                                v .: "install" <*>
                                v .:? "blurb"
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
      d <- (Yaml.decodeEither <$> (getYaml "build.yml"))
      case d of
        Left err -> print $ strErr err
        Right ps -> print $ moo ps
