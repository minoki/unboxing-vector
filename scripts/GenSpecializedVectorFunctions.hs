import Data.List
import System.IO
import System.Environment

findDoubleColon :: String -> Maybe String
findDoubleColon (' ':':':':':' ':xs) = Just "" -- " :: " ++ xs
findDoubleColon (x:xs) = (x:) <$> findDoubleColon xs
findDoubleColon [] = Nothing

doLine :: String -> [String]
doLine line = case findDoubleColon line of
  Just name@('(':xs) -> [ line
                        , name ++ " = (G." ++ xs
                        , "{-# INLINE " ++ name ++ " #-}"
                        ]
  Just name -> let param = if name == "create" || name == "createT" || name == "modify"
                           then "" -- " p"
                           else ""
               in [ line
                  , name ++ param ++ " = G." ++ name ++ param
                  , "{-# INLINE " ++ name ++ " #-}"
                  ]
  Nothing -> [line]

getSymbolName :: String -> [String]
getSymbolName line = case findDoubleColon line of
  Just name -> [name]
  Nothing -> []

doFile :: FilePath -> IO ()
doFile file = do
  content <- readFile file
  putStrLn $ unlines (concatMap doLine (lines content))
  putStrLn $ "-- " ++ intercalate "," (concatMap getSymbolName (lines content))

main :: IO ()
main = do
  args <- getArgs
  case args of
    fn:_ -> do
      doFile fn
    _ -> hPutStrLn stderr "Usage: runhaskell GenSpecializedVectorFunctions.hs <template file name>"
