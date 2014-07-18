{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PelitaRunner
    (
      runPelita
    , Team(..)) where

import           Control.Applicative          ((<$>), (<*>))

import           Data.List                    (tails)
import qualified Data.PSQueue                 as PSQueue
import           Data.Text
import           Data.Text.Format
import           Data.Text.IO
import qualified Data.Text.Lazy               as L

import           Data.Random
import           Data.Random.Extras
import           Data.Random.Source.DevRandom

type URL = Text
type Factory = Text
data Team = Team URL Factory deriving (Show, Eq, Ord)
data Match = Match Team Team deriving Show
data Result = Draw | First | Second deriving Show
type FinishedMatch = ( Match, Result )

data RoundRobin = RoundRobin { hasPlayed :: [ FinishedMatch ]
                             , toPlay    :: [ Match ]
                             , ranking   :: [ Team ]
                             }

playGame :: Match -> IO FinishedMatch
playGame match = do
    result <- runRVar (choice [ Draw, First, Second ]) DevRandom
    return ( match, result )

rankMatches :: [FinishedMatch] -> PSQueue.PSQ Team Int
rankMatches [] = PSQueue.empty
rankMatches (match : other) = go match
  where
    go (Match t1 t2, Draw)  = PSQueue.alter (inc 1) t1 $ PSQueue.alter (inc 1) t2 (rankMatches other)
    go (Match t1 _, First)  = PSQueue.alter (inc 2) t1 (rankMatches other)
    go (Match _ t2, Second) = PSQueue.alter (inc 2) t2 (rankMatches other)

    inc :: Int -> Maybe Int -> Maybe Int
    inc i Nothing = Just i
    inc i (Just n) = Just (i + n)

genMatches :: [Team] -> [Match]
genMatches teams = do
    (team : others) <- Data.List.tails teams
    other <- others
    return $ Match team other

shuffleMatches :: [Match] -> IO [Match]
shuffleMatches matches = runRVar (Data.Random.Extras.shuffle matches) DevRandom

playMatches :: [Match] -> IO [FinishedMatch]
playMatches = mapM $ \m -> do
    Data.Text.IO.putStrLn $ prepareGameMsg m
    fm <- playGame m
    Data.Text.IO.putStrLn $ finishedGameMsg fm
    return fm

prepareGameMsg :: Match -> Text
prepareGameMsg (Match t1 t2) = L.toStrict $ format "Starting game between teams ‘{}’ and ‘{}’." ((name t1), (name t2))
  where
    name (Team url factory) = format "{}:{}" (url, factory)

finishedGameMsg :: FinishedMatch -> Text
finishedGameMsg (Match t1 t2, result) = L.toStrict $ (format "Finished game between teams ‘{}’ and ‘{}’." ((name t1), (name t2)))
                                                     `L.append` " " `L.append` outcomeMsg
  where
    name (Team url factory) = format "{}:{}" (url, factory)
    outcomeMsg = outcome result t1 t2

    outcome Draw _ _ = L.fromStrict "It was a draw."
    outcome First t1 _ = format "{} won." (Only $ name t1)
    outcome Second _ t2 = format "{} won." (Only $ name t2)

runPelita :: [Team] -> IO ()
runPelita teams = do
    matches <- shuffleMatches $ genMatches teams
    (Prelude.putStrLn . show) matches
    Prelude.putStrLn ""
    (playMatches matches) >>= (Prelude.putStrLn . show)
    Prelude.putStrLn ""
    rankMatches <$> (playMatches matches) >>= (Prelude.putStrLn . show)

