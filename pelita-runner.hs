
import           ConfigReader
import           PelitaRunner

main :: IO ()
main = do
    teamsE <- readTeams
    case teamsE of
        Left e -> putStrLn $ "problem" ++ (show e)
        Right teams -> runPelita teams

