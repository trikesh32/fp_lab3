module Main (main) where

import App (Options(..), AlgoState, Output, parseArgs, initialAlgoStates, advanceAlgorithms, formatLine)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, isEOF, stderr)
import Text.Read (readMaybe)
import Types (Point(Point, px, py))

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
runProgram opts = do
  inputPoints <- readAllInput []
  processStreaming opts inputPoints
  where
    readAllInput :: [Point] -> IO [Point]
    readAllInput acc = do
      eof <- isEOF
      if eof
        then return (reverse acc)
        else do
          line <- getLine
          case parsePoint line of
            Left err -> do
              hPutStrLn stderr err
              exitFailure
            Right Nothing -> readAllInput acc
            Right (Just point) ->
              if not (ordered acc point)
                then do
                  hPutStrLn stderr "Входные данные должны быть отсортированы по возрастанию x"
                  exitFailure
                else readAllInput (point : acc)

    ordered :: [Point] -> Point -> Bool
    ordered [] _ = True
    ordered (p : _) newP = px newP > px p

processStreaming :: Options -> [Point] -> IO ()
processStreaming opts points = go [] (initialAlgoStates (optAlgorithms opts)) points
  where
    step = optStep opts

    go :: [Point] -> [AlgoState] -> [Point] -> IO ()
    go _ _ [] = return ()
    go accumulated states (p : rest) = do
      -- Print input point
      putStrLn $ "< " ++ show (px p) ++ " " ++ show (py p)
      
      -- Add point to accumulated
      let newAccumulated = accumulated ++ [p]
      
      -- Generate outputs for all algorithms
      let (newStates, outputs) = advanceAlgorithms step newAccumulated states
      
      -- Print all outputs
      mapM_ printOutput outputs
      
      -- Continue with next point
      go newAccumulated newStates rest

    printOutput :: Output -> IO ()
    printOutput (alg, x, y) = putStrLn $ "> " ++ formatLine alg x y

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