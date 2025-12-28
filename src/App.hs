module App
  ( Options (..),
    AlgoState (..),
    Output,
    parseArgs,
    initialAlgoStates,
    advanceAlgorithms,
    simulateSequential,
    formatLine,
  )
where

import Interpolation
import Text.Read (readMaybe)
import Types (Point (..))

data Options = Options
  { optStep :: Double,
    optAlgorithms :: [Algorithm]
  }
  deriving (Show)

data AlgoState = AlgoState Algorithm (Maybe Double)
  deriving (Eq, Show)

type Output = (Algorithm, Double, Double)

parseArgs :: [String] -> Either String Options
parseArgs args =
  case walk args defaultOptions of
    Left err -> Left err
    Right opts -> checkResult opts
  where
    defaultOptions = Options {optStep = 1.0, optAlgorithms = []}

    walk :: [String] -> Options -> Either String Options
    walk [] opts = Right opts
    walk ("--linear" : rest) opts =
      walk rest (addAlgorithm AlgLinear opts)
    walk ("--newton" : value : rest) opts =
      case readMaybe value of
        Just n | n >= 2 -> walk rest (addAlgorithm (AlgNewton n) opts)
        _ -> Left "Флаг --newton требует целое число >= 2"
    walk ["--newton"] _ = Left "Флаг --newton требует параметр"
    walk ("--step" : value : rest) opts =
      case readMaybe value of
        Just s | s > 0 -> walk rest (opts {optStep = s})
        _ -> Left "Флаг --step требует положительное число"
    walk ["--step"] _ = Left "Флаг --step требует параметр"
    walk ("-h" : _) _ = Left ""
    walk ("--help" : _) _ = Left ""
    walk (unknown : _) _ = Left ("Неизвестный аргумент: " ++ unknown)

    addAlgorithm :: Algorithm -> Options -> Options
    addAlgorithm alg opts
      | alg `elem` optAlgorithms opts = opts
      | otherwise = opts {optAlgorithms = optAlgorithms opts ++ [alg]}

    checkResult :: Options -> Either String Options
    checkResult opts
      | null (optAlgorithms opts) = Left "Нужно указать хотя бы один алгоритм (например, --linear)"
      | optStep opts <= 0 = Left "Шаг дискретизации должен быть положительным"
      | otherwise = Right opts

initialAlgoStates :: [Algorithm] -> [AlgoState]
initialAlgoStates algorithms = [AlgoState alg Nothing | alg <- algorithms]

advanceAlgorithms :: Double -> [Point] -> [AlgoState] -> ([AlgoState], [Output])
advanceAlgorithms _ _ [] = ([], [])
advanceAlgorithms step pts (state : rest) =
  let (state', outputs) = advanceOne step pts state
      (rest', outputsRest) = advanceAlgorithms step pts rest
   in (state' : rest', outputs ++ outputsRest)

advanceOne :: Double -> [Point] -> AlgoState -> (AlgoState, [Output])
advanceOne _ [] st = (st, [])
advanceOne step pts@(first : _) (AlgoState kind nextX) =
  let startX = case nextX of
        Nothing -> px first
        Just x -> x
      lastX = px (last pts)
      loop current outputs
        | current > lastX = (AlgoState kind (Just current), outputs)
        | otherwise =
            case calculate kind pts current of
              Nothing -> (AlgoState kind (Just current), outputs)
              Just value ->
                let nextValue = current + step
                    newOutputs = outputs ++ [(kind, current, value)]
                 in loop nextValue newOutputs
   in loop startX []

calculate :: Algorithm -> [Point] -> Double -> Maybe Double
calculate AlgLinear = linearValue
calculate (AlgNewton n) = newtonValue n

formatLine :: Algorithm -> Double -> Double -> String
formatLine alg x y = algorithmName alg ++ ": " ++ show x ++ " " ++ show y

simulateSequential :: Double -> [Algorithm] -> [Point] -> [Output]
simulateSequential step algorithms = go [] (initialAlgoStates algorithms)
  where
    go :: [Point] -> [AlgoState] -> [Point] -> [Output]
    go _ _ [] = []
    go seen states (p : rest) =
      let seen' = seen ++ [p]
          (states', outputs) = advanceAlgorithms step seen' states
       in outputs ++ go seen' states' rest