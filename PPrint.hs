module PPrint where
import Data.List

writeln :: String -> IO ()
writeln = putStrLn

showsPair :: Show a => (String, a) -> ShowS
showsPair (k,v) = (++) (k ++ ": " ++ show v)

pprH, pprV :: [ShowS] -> ShowS
pprV = intercalateS ("\n" ++)
pprH = intercalateS (" " ++)

intercalateS :: ShowS -> [ShowS] -> ShowS
intercalateS sep list = (++) (intercalate (sep "") [ s "" | s <- list])

pprListWith :: (a -> ShowS) -> [a] -> ShowS
pprListWith f = pprV . map f

runShows :: ShowS -> IO ()
runShows = putStrLn . ($"")
