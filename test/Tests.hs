module Tests where


import Distribution.TestSuite (Test)
import Distribution.TestSuite.QuickCheck
import Test.QuickCheck ( (===)
                       , (.&&.)
                       , chooseInt
                       , chooseEnum
                       , counterexample
                       , listOf1
                       , Testable)
import Math.NumberTheory.Primes (factorBack)

import MyLib

tests :: IO [Test]
tests = return tests'

normalSettings :: TestArgs
normalSettings = stdTestArgs { verbosity = Verbose }

getNormalPT :: Testable prop => PropertyTest prop -> Test
getNormalPT = getPropertyTestWith normalSettings


hardcoded :: (Eq a, Show a) => a -> a -> Test
hardcoded value answer = getNormalPT PropertyTest {
    name = "hardcoded-" ++ show value ++ "-equals-" ++ show answer,
    tags = [],
    property = value === answer
    }

tests' :: [Test]
tests' = [ hardcoded (child 1) 1
         , hardcoded (child 2) 3
         , hardcoded (child 3) 4
         , hardcoded (child 4) 4
         , hardcoded (child 60) 96
         , hardcoded (child 30) 72
         , squarefreeGrows
         , hardcoded (child 24) 20
         , hardcoded (child 20) 24
         , hardcoded (child 6) 12
         , hardcoded (child 12) 16
         , hardcoded (child 16) 6
         , hardcoded (child 90) 90
         , hardcoded (child 120) 120
         , reasonableSteps
         , shortcutSteps
         , junkySteps
         ]

squarefreeGrows :: Test
squarefreeGrows = getNormalPT PropertyTest {
    name = "squarefreeGrows",
    tags = [],
    property = do factors <- listOf1 $ (,1) . toEnum <$> chooseInt (1, 1000)
                  return $ factorBack factors < buildChildFrom factors
    }

reasonableSteps :: Test
reasonableSteps = getNormalPT PropertyTest {
    name = "reasonableSteps",
    tags = [],
    property = do n <- chooseEnum (1, 1000 * 1000 * 1000)
                  return $ stepsToTerminal n <= (1000 * 1000)
    }

shortcutSteps :: Test
shortcutSteps = getPropertyTestWith normalSettings{maxSuccess = 1000} PropertyTest {
    name = "shortcutSteps",
    tags = [],
    property = do n <- chooseEnum (1, 1000 * 1000 * 1000 * 1000)
                  let (steps, remainder) = stepsBack n n
                  return $ counterexample (show n) $ (remainder <= n) .&&. (steps <= stepsToTerminal n)
    }

junkySteps :: Test
junkySteps = getNormalPT PropertyTest {
    name = "junkySteps",
    tags = [],
    property = do n <- chooseEnum (easyCutoff + 1, 1000 * 1000 * 1000 * 1000)
                  let (steps, _) = stepsBack n n
                  return $ counterexample (show n) $ steps === stepsBackX n
    }

