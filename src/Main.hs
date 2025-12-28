module Main (main) where

import App
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, isEOF, stderr)
import Text.Read (readMaybe)
import Types

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Left err -> do
      hPutStrLn stderr err
      printUsage
      exitFailure
    Right opts -> runProgram opts

runProgram :: Options -> IO ()
runProgram opts = loop [] (initialAlgoStates (optAlgorithms opts))
  where
    step = optStep opts

    loop :: [Point] -> [AlgoState] -> IO ()
    loop points states = do
      eof <- isEOF
      if eof
        then
          mapM_ (putStrLn . renderLine) (snd (advanceAlgorithms step points states))
        else do
          line <- getLine
          case parsePoint line of
            Left err -> do
              hPutStrLn stderr err
              exitFailure
            Right Nothing -> loop points states
            Right (Just point) ->
              if not (ordered points point)
                then do
                  hPutStrLn stderr "Входные данные должны быть отсортированы по возрастанию x"
                  exitFailure
                else do
                  let newPoints = points ++ [point]
                      (newStates, outputs) = advanceAlgorithms step newPoints states
                  mapM_ (putStrLn . renderLine) outputs
                  loop newPoints newStates

    ordered :: [Point] -> Point -> Bool
    ordered [] _ = True
    ordered xs p = px p > px (last xs)

    renderLine :: Output -> String
    renderLine (alg, x, y) = formatLine alg x y

parsePoint :: String -> Either String (Maybe Point)
parsePoint raw =
  let cleaned = map replaceDelimiter raw
      tokens = words cleaned
   in case tokens of
        [] -> Right Nothing
        [sx, sy] ->
          case (readMaybe sx, readMaybe sy) of
            (Just x, Just y) -> Right (Just (Point x y))
            _ -> Left ("Невозможно прочитать точку: " ++ raw)
        _ -> Left ("Некорректный формат строки: " ++ raw)
  where
    replaceDelimiter ';' = ' '
    replaceDelimiter other = other

printUsage :: IO ()
printUsage = do
  putStrLn "Использование: my_lab3 [--linear] [--newton N] [--step S]"
  putStrLn "  --linear        линейная интерполяция"
  putStrLn "  --newton N      интерполяция Ньютона по N точкам (N >= 2)"
  putStrLn "  --step S        шаг по оси X (положительное число, по умолчанию 1.0)"