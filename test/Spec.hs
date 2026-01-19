module Main (main) where

import Control.Monad (unless, zipWithM_)
import Test.HUnit
import qualified Test.QuickCheck as QC

import App (simulateSequential)
import Interpolation
import Types (Point(Point, px, py))

main :: IO ()
main = runTestTTAndExit tests

(~=) :: Double -> Double -> Bool
(~=) a b = abs (a - b) < 1e-4

approxFailure :: String -> Double -> Double -> Assertion
approxFailure label expected actual =
  assertBool label (actual ~= expected)

assertApprox :: String -> Double -> Maybe Double -> Assertion
assertApprox label expected actual =
  case actual of
    Nothing -> assertFailure (label ++ ": ожидалось значение, получили Nothing")
    Just value -> approxFailure (label ++ ": ожидалось " ++ show expected ++ ", получили " ++ show value) expected value

assertNothing :: String -> Maybe Double -> Assertion
assertNothing label actual =
  case actual of
    Nothing -> pure ()
    Just value -> assertFailure (label ++ ": ожидалось Nothing, получили " ++ show value)

assertApproxPair :: String -> (Double, Double) -> (Double, Double) -> Assertion
assertApproxPair label (expectedX, expectedY) (actualX, actualY) = do
  approxFailure (label ++ " по x") expectedX actualX
  approxFailure (label ++ " по y") expectedY actualY

pointsLinear :: [Point]
pointsLinear =
  [ Point 0 0
  , Point 2 2
  ]

pointsNewton :: [Point]
pointsNewton =
  [ Point 0 0
  , Point 1 1
  , Point 2 4
  , Point 3 9
  ]

linearTests :: Test
linearTests = TestLabel "linear interpolation" $ TestCase $ do
  assertApprox "точка внутри" 1.0 (linearValue pointsLinear 1)
  assertApprox "левая граница" 0.0 (linearValue pointsLinear 0)
  assertApprox "правая граница" 2.0 (linearValue pointsLinear 2)
  assertNothing "нет сегмента" (linearValue pointsLinear (-1))

newtonTests :: Test
newtonTests = TestLabel "newton interpolation" $ TestCase $ do
  assertNothing "недостаточно точек" (newtonValue 5 pointsNewton 1.5)
  assertApprox "квадратичная функция" 2.25 (newtonValue 4 pointsNewton 1.5)
  assertApprox "значение в узле" 9.0 (newtonValue 4 pointsNewton 3)

qcTest :: String -> QC.Property -> Test
qcTest label prop = TestLabel label $ TestCase $ do
  result <- QC.quickCheckWithResult QC.stdArgs { QC.chatty = False } prop
  unless (QC.isSuccess result) $ assertFailure (QC.output result)

-- генератор для линейного интервала и линейной функции

genLineCase :: QC.Gen (Double, Double, Double, Double, Double)
genLineCase = do
  x1 <- QC.choose (-10.0, 10.0)
  dx <- QC.choose (1e-3, 5.0)
  a <- QC.choose (-5.0, 5.0)
  b <- QC.choose (-5.0, 5.0)
  t <- QC.choose (0.0, 1.0)
  pure (x1, dx, a, b, t)

prop_linearInterpolatesLine :: QC.Property
prop_linearInterpolatesLine = QC.forAll genLineCase $ \(x1, dx, a, b, t) ->
  let x2 = x1 + dx
      x = x1 + t * dx
      y1 = a * x1 + b
      y2 = a * x2 + b
      expected = a * x + b
      actual = linearValue [Point x1 y1, Point x2 y2] x
  in case actual of
       Nothing -> QC.counterexample "linearValue вернула Nothing" False
       Just value -> QC.counterexample ("ожидалось " ++ show expected ++ ", получили " ++ show value) (value ~= expected)

prop_linearOutside :: QC.Property
prop_linearOutside = QC.forAll genLineCase $ \(x1, dx, a, b, _) ->
  let x2 = x1 + dx
      y1 = a * x1 + b
      y2 = a * x2 + b
      outsideLeft = x1 - dx
      outsideRight = x2 + dx
      pts = [Point x1 y1, Point x2 y2]
  in QC.conjoin
       [ QC.counterexample "значение слева" (linearValue pts outsideLeft == Nothing)
       , QC.counterexample "значение справа" (linearValue pts outsideRight == Nothing)
       ]


genNewtonCase :: QC.Gen (Int, [Point], Double, [Double])
genNewtonCase = do
  n <- QC.chooseInt (2, 5)
  x0 <- QC.choose (-10.0, 10.0)
  deltas <- QC.vectorOf (n - 1) (QC.choose (1e-3, 3.0))
  let xs = scanl (+) x0 deltas
  coeffs <- QC.vectorOf n (QC.choose (-3.0, 3.0))
  let polyValue x = sum [c * x ^ i | (c, i) <- zip coeffs [0 ..]]
      pts = [Point x (polyValue x) | x <- xs]
  xtest <- QC.choose (-10.0, 10.0)
  pure (n, pts, xtest, coeffs)

prop_newtonMatchesPolynomial :: QC.Property
prop_newtonMatchesPolynomial = QC.forAll genNewtonCase $ \(n, pts, xtest, coeffs) ->
  let xs = map px pts
      polyValue x = sum [c * x ^ i | (c, i) <- zip coeffs [0 ..]]
      expected = polyValue xtest
      actual = newtonValue n pts xtest
  in case actual of
       Nothing -> QC.counterexample "newtonValue вернула Nothing" False
       Just value -> QC.counterexample ("ожидалось " ++ show expected ++ ", получили " ++ show value) (value ~= expected)

tests :: Test
tests = TestList
  [ linearTests
  , newtonTests
  , qcTest "linear interpolates affine line" prop_linearInterpolatesLine
  , qcTest "linear outside interval" prop_linearOutside
  , qcTest "newton matches polynomial" prop_newtonMatchesPolynomial
  , simultaneousAlgorithmsTest
  ]

simultaneousAlgorithmsTest :: Test
simultaneousAlgorithmsTest = TestLabel "simultaneous algorithms" $ TestCase $ do
  let pts =
        [ Point 0 0
        , Point 1 1
        , Point 2 4
        , Point 3 9
        ]
      outputs = simulateSequential 0.5 [AlgLinear, AlgNewton 4] pts
      linearOutputs = [ (x, y) | (AlgLinear, x, y) <- outputs ]
      newtonOutputs = [ (x, y) | (AlgNewton _, x, y) <- outputs ]
      expectedLinear =
        [ (0.0, 0.0)
        , (0.5, 0.5)
        , (1.0, 1.0)
        , (1.5, 2.5)
        , (2.0, 4.0)
        , (2.5, 6.5)
        , (3.0, 9.0)
        ]
      expectedNewton =
        [ (0.0, 0.0)
        , (0.5, 0.25)
        , (1.0, 1.0)
        , (1.5, 2.25)
        , (2.0, 4.0)
        , (2.5, 6.25)
        , (3.0, 9.0)
        ]
  assertEqual "линейный алгоритм выдал нужное число значений" (length expectedLinear) (length linearOutputs)
  assertEqual "алгоритм Ньютона выдал нужное число значений" (length expectedNewton) (length newtonOutputs)
  zipWithM_ (assertApproxPair "линейный алгоритм") expectedLinear linearOutputs
  zipWithM_ (assertApproxPair "алгоритм Ньютона") expectedNewton newtonOutputs