{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
-- https://github.com/Gabriel439/post-rfc/blob/master/sotu.md#scripting--command-line-applications

module Lib
    ( someFunc
    ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text             as Text
import           Data.Yaml             (FromJSON)
import qualified Data.Yaml             as Yaml
import           GHC.Generics          (Generic)
import           Turtle                (shell, (<>))
import qualified Turtle                as T

data Build = Build { language :: String
                   , install  :: [T.Text]
                   , blurb    :: Maybe String}
             deriving (Show, Generic, FromJSON)

getYaml :: FilePath -> IO BS.ByteString
getYaml = BS.readFile

runCmd :: T.Text -> IO ()
runCmd cmd = do
  print ("Running: " <> cmd)
  res <- shell cmd T.empty
  case res of
    T.ExitSuccess -> return ()
    T.ExitFailure n -> T.die ("Shell cmd: " <> cmd <> " failed with code: " <> T.repr n)

runCmds cmds = T.sh $  do
  res <- T.select cmds
  T.liftIO (runCmd res)

someFunc :: IO ()
someFunc =
    do
      d <- Yaml.decodeEither <$> getYaml "build.yml"
      case d of
        Left err -> T.die $ Text.pack err
        Right ps -> runCmds (install ps)
