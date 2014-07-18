{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ConfigReader
    (
      readTeams) where

import           Control.Applicative ((<$>), (<*>))
import           Data.Map
import           Data.Text
import           Data.Yaml

import           PelitaRunner

instance FromJSON Team
  where
    parseJSON (String s) = return $ Team url factory
      where
        (url : factory : rest) = splitOn ":" s

data TeamList = TeamList { teamList :: [Team] }

instance FromJSON TeamList
  where
    parseJSON (Object v) =
      TeamList <$> (v .: "teams")


readTeams :: IO (Either ParseException [Team])
readTeams = do
    decodedE <- decodeFileEither "config.yaml"
    return $ fmap teamList decodedE

