{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where


-- https://github.com/Gabriel439/post-rfc/blob/master/sotu.md#scripting--command-line-applications

import Data.Yaml
import Control.Monad
import qualified Data.ByteString.Char8 as BS

data MyUser = MyUser {id :: Int,
                      name :: String,
                      reputation :: Int}
                      deriving (Show)


instance FromJSON MyUser where
    parseJSON (Object v) = MyUser <$>
                           v .: "id" <*>
                           v .: "name" <*>
                           v .: "reputation"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _ = mzero


strErr :: Show a => a  -> String
strErr s = "Error: " ++ (show s)

getYaml :: FilePath -> IO BS.ByteString
getYaml = BS.readFile

moo :: [MyUser] -> String
moo users = show users

someFunc :: IO ()
someFunc =
    do
      d <- (Data.Yaml.decodeEither <$> (getYaml "users.yml"))
      case d of
        Left err -> print $ strErr err
        Right ps -> print $ moo ps
