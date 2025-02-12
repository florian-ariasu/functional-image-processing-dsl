module TestPP 
  ( quickCheck, vmCheck, vmCheckIO
  , testCond, testVal, testValApprox, testManually, testSet
  , testCondW, testValW, testManuallyW, testSetW
  , test, test_, tests, tests_
  , TestData, TestCase
  ) where

import qualified Control.Exception as E (catch, evaluate, SomeException (..))
import Data.List
import Debug.Trace

-- |Defines one test case, internally.
data TestCase = TestCase
  { 
    -- |the number of the excercise
    testIndex   :: Int
    
    -- |description or name of the test
  , testDescr   :: String
  
  {-|
    weight of the test case
    for exercises with multiple tests, this is initially the weight of each
     test, and is later transformed into the actual score of the test (becoming a
     weight with respect to the whole activity).
    for exercises with only one test, this is initially ignored, and is later 
     transformed into the score of the test, the same with the score of the exercise
     (becoming a weight with respect to the whole activity).
    -}
  , testWeight  :: Float 
  
  -- |the error message, if the test fails
  , testError   :: String
  
  -- |the test itself, evaluating to a Bool when it is evaluated
  , testPassed  :: Bool   -- has passed? also lazily contains the actual test
  }


-- |Information for testing an entire exercise (is a list of test cases).
newtype TestData = TD {getTestCases :: [TestCase]}

instance Show TestData where
{-|
  Printing a test also checks it
  If only an individual check (e.g. check1) is performed, this will also evaluate the test.
  If this is part of the global check, the test has already been evaluated.
 -}
  show = concat . intersperse "\n" . map (testMessage . testResult) . getTestCases

-- |shows a fractional number rounded to 2 digits.
showPt :: RealFrac a => a       -- ^the number
                    -> String   -- ^a String of the form "12.45"
showPt = show . (* 0.01) . fromIntegral . round . (* 100)

{-|
    Shows the number of an exercise, just as [1] or as [ex 1/15] 
    depending on whether points are shown or not.
 -}
showEx :: Bool      -- ^ are points shown?
         -> Int     -- ^ the total number of exercises
         -> Int     -- ^ the index of the exercise
         -> String  -- ^ the String to show
showEx showPoints nEx index = "[" ++ (if showPoints then "" else "ex ")
         ++ show index ++ (if showPoints then "" else "/" ++ show nEx) ++ "]"

-- |Contains information obtained from running one test case.
data TestCaseResult = TestCaseResult
  { 
    -- |the number of the exercise
    testExercise       :: Int
    
    -- |full output of the test
  , testMessage        :: String
    
    -- |how much the test actually scored
  , testScore          :: Float
    
    -- |was the implementation undefined?
  , testSubjectUndefined   :: Bool
    
    -- |has the test resulted in an exception?
  , testExcepted       :: Bool
  } deriving Show

{-|
    Builds test data for an exercise, based on the descriptions of
    individual test cases. Use this for when having multiple test cases 
    in an exercise, and use 'test' for when having a single test case.
    
    This version also contains the number of points associated with the 
    exercise. For a version with no points, use 'tests_'.
    
    In the constructed 'TestData', individual test cases will contain a score
    which is the given number of points for the exercise, divided among
    test cases according to their respective weights.
-}
tests :: Int            -- ^ the number of the exercise
         -> Float       -- ^ the number of points for the whole exercise
         -> [TestCase]  -- ^ the test cases used for testing the exercise
         -> TestData    -- ^ the resulting 'TestData' instance
tests ex points testList = TD $ map tt testList
      where
        total = foldl (+) 0 $ map testWeight testList
        tt (TestCase _ d p e b) = TestCase ex d (points * p / total) e b

{-|
    Builds test data for an exercise (see 'tests') with multiple test cases.
    
    This version gives 0 points for the exercise, resulting in no points associated
    with the test cases.
-}
tests_ :: Int           -- ^ the number of the exercise
         -> [TestCase]  -- ^ the test cases used for testing the exercise
         -> TestData    -- ^ the resulting 'TestData' instance
tests_ ex = tests ex 0

{-|
    Builds test data for an exercise, based on the description of
    a single test case. Use 'tests' for when having multiple test cases.
    
    The 'testWeight' field in the test case is ignored.
-}
test :: Int             -- ^ the number of the exercise
         -> Float       -- ^ the number of points for the whole exercise
         -> TestCase    -- ^ the test case used for testing the exercise
         -> TestData    -- ^ the resulting 'TestData' instance
test ex points tc = TD [tc { testIndex = ex, testWeight = points }]

{-|
    Builds test data for an exercise (see 'test') with a single test case.
    
    This version gives 0 points for the exercise, resulting in no points associated
    with the test cases.
-}
test_ :: Int            -- ^ the number of the exercise
         -> TestCase    -- ^ the test cases used for testing the exercise
         -> TestData    -- ^ the resulting 'TestData' instance
test_ ex = test ex 0

{-|
    Describes a test case which evaluates a boolean value. 
    
    WARNING: use this only when you truly want to test a boolean condition, which is
    not very often. DO NOT use this test to test, for instance, equality of values,
    as this will not give appropriate feedback, it will just say "boolean test failed"
    
    This version gives a weight to the test. In an exercise with multiple test cases,
    this weight decides the score of the test, depending on the weights of the other
    tests and on the score of the whole exercise.
       
    In an exercise where this is the only test, this value is ignored.
-}
testCondW :: String      -- ^ the description or name of the test
             -> Float    -- ^ the weight or number of points of the test
             -> Bool     -- ^ the condition to test
             -> TestCase -- ^ the resulting 'TestCase' instance
testCondW descr points bool = buildTest descr points "conditie neindeplinita" bool

-- |The same as 'testCondW', but with a unitary weight.
testCond :: String       -- ^ the description or name of the test
             -> Bool     -- ^ the condition to test
             -> TestCase -- ^ the resulting 'TestCase' instance
testCond descr = testCondW descr 1

{-|
    Describes a test case which evaluates the equality of two
     values (expected and given).

    It must be that the values are of types instantiating Show and Eq.

    This version gives a weight to the test. In an exercise with multiple test cases,
    this weight decides the score of the test, depending on the weights of the other
    tests and on the score of the whole exercise.
    
    In an exercise where this is the only test, this value is ignored.
-}

testValW :: (Show a, Eq a)
         => String   -- ^ the description or name of the test
         -> Float    -- ^ the weight or number of points of the test
         -> a        -- ^ the expected value
         -> a        -- ^ the given value
         -> TestCase -- ^ the resulting 'TestCase' instance
testValW descr points ve vg = testWithW descr "valoarea" "este egala" points ve (==) vg

testValWApprox :: (Show a, Eq a, Ord a)
         => String   -- ^ the description or name of the test
         -> Float    -- ^ the weight or number of points of the test
         -> (a,a)        -- ^ the expected / exact value
         -> (a,a)        -- ^ the given value
         -> a        -- ^ the tolerance value 
         -> a        -- ^ the difference between the exact value and it's given approximation
         -> TestCase -- ^ the resulting 'TestCase' instance
testValWApprox descr points ve vg tol diff = testWithWApprox descr "valoarea" "este egala" points ve vg diff (<=) tol

-- |The same as 'testValW', but with a unitary weight.
testVal :: (Show a, Eq a)
         => String   -- ^ the description or name of the test
         -> a        -- ^ the expected value
         -> a        -- ^ the given value
         -> TestCase -- ^ the resulting 'TestCase' instance
testVal descr = testValW descr 1

-- |The same as 'testValWApprox', but with a unitary weight.
testValApprox :: (Show a, Eq a, Ord a, Num a, Fractional a, Floating a)
         => String   -- ^ the description or name of the test
         -> (a, a)        -- ^ the expected value
         -> (a, a)        -- ^ the given value
         -> TestCase -- ^ the resulting 'TestCase' instance
testValApprox descr ve vg = testValWApprox descr 1 ve vg 1e-4 (sqrt (valFst + valSnd))
  where
    valFst = ((fst ve) - (fst vg)) ** 2
    valSnd = ((snd ve) - (snd vg)) ** 2


{-|
    Describes a test case which evaluates the equality of two
     sets (expected and given).
     
    It must be that the values in the sets are of types instantiating Show and Eq.
     
    The equality is computed with 'sameElements'. If there are duplicates, they must
    appear in the same number in both sets. For example:
    
    [1,2,3] and [3,2,1] have the same elements (the test passes)
    
    [1,2,3,2] and [3,2,2,1] have the same elements (the test passes)
    
    [1,2,3] and [3,2,2,1] do NOT have the same elements (the test fails)
    
    
    This version gives a weight to the test. In an exercise with multiple test cases,
    this weight decides the score of the test, depending on the weights of the other
    tests and on the score of the whole exercise.
    
    In an exercise where this is the only test, this value is ignored.
-}
testSetW :: (Show a, Eq a) 
         => String   -- ^ the description or name of the test
         -> Float    -- ^ the weight or number of points of the test
         -> [a]      -- ^ the expected set of elements
         -> [a]      -- ^ the given set of elements
         -> TestCase -- ^ the resulting 'TestCase' instance
testSetW descr points ve vg = testWithW descr "multimea" "are aceleasi elemente" points ve sameElements vg

-- |The same as 'testSetW', but with a unitary weight.
testSet :: (Show a, Eq a)
        => String   -- ^ the description or name of the test
         -> [a]      -- ^ the expected set of elements
         -> [a]      -- ^ the given set of elements
         -> TestCase -- ^ the resulting 'TestCase' instance
testSet descr = testSetW descr 1


{-|
    Describes a test case which evaluates the relation of two values (expected and
    given), using an arbitrary function.
    
    It must be that the values are of types instantiating Show.
    
    vname and opname are used in creating the output message. e.g.
    "<multimea> data <...> nu <este echivalenta> cu <multimea> asteptata <...>
    
    This version gives a weight to the test. In an exercise with multiple test cases,
    this weight decides the score of the test, depending on the weights of the other
    tests and on the score of the whole exercise.
    
    In an exercise where this is the only test, this value is ignored.
-}
testWithW :: (Show a1, Show a) 
            => String       -- ^ the description or name of the test
            -> String       -- ^ what the compared values are (e.g. set, graph, etc)
            -> String       -- ^ what the operation is called (e.g. equal, equivalent, etc)
            -> Float        -- ^ the weight or number of points of the test
            -> a            -- ^ the expected value
            -> (a -> a1 -> Bool)    -- ^ the comparison function seying if the condition is met
            -> a1           -- ^ the given value
            -> TestCase     -- ^ the resulting 'TestCase' instance
testWithW descr vname opname points ve op vg = buildTest descr points err (ve `op` vg)
  where err = (vname ++ " data " ++ show vg ++ " nu " ++ opname ++ " cu " ++ vname ++ " asteptata " ++ show ve)


testWithWApprox :: (Show a) 
            => String       -- ^ the description or name of the test
            -> String       -- ^ what the compared values are (e.g. set, graph, etc)
            -> String       -- ^ what the operation is called (e.g. equal, equivalent, etc)
            -> Float        -- ^ the weight or number of points of the test
            -> (a, a)            -- ^ the expected value
            -> (a, a)            -- ^ the given value
            -> a            -- ^ the difference between the exact value and it's approximation
            -> (a -> a -> Bool)    -- ^ the comparison function seying if the condition is met
            -> a           -- ^ the tolerance
            -> TestCase     -- ^ the resulting 'TestCase' instance
testWithWApprox descr vname opname points ve vg diff op tol = buildTest descr points err (diff `op` tol)
  where err = (vname ++ " data " ++ show diff ++ " nu " ++ opname ++ " cu " ++ vname ++ " asteptata " ++ show ve
              ++ " cu toleranta " ++ show tol)

-- |The same as 'testWithW', but with a unitary weight.
testWith :: (Show a1, Show a)
            => String       -- ^ the description or name of the test
            -> String       -- ^ what the compared values are (e.g. set, graph, etc)
            -> String       -- ^ what the operation is called (e.g. equal, equivalent, etc)
            -> a            -- ^ the expected value
            -> (a -> a1 -> Bool)    -- ^ the comparison function seying if the condition is met
            -> a1           -- ^ the given value
            -> TestCase     -- ^ the resulting 'TestCase' instance
testWith descr vname opname = testWithW descr vname opname 1


{-|
    Describes a testCase which must be tested manually.
    
    The recommended manner to use this is the following:
    
    A boolean value is defined close to the exercise:
    test8_OK = True
    
    The value is used in a manual test:
    testManually "manual test" test8_OK
    
    This version gives a weight to the test. In an exercise with multiple test cases,
    this weight decides the score of the test, depending on the weights of the other
    tests and on the score of the whole exercise.
    
    In an exercise where this is the only test, this value is ignored.
-}
testManuallyW :: String     -- ^ the description or name of the test
            -> Float        -- ^ the weight or number of points of the test
            -> Bool         -- ^ whether the test passes or not.
            -> TestCase     -- ^ the resulting 'TestCase' instance
testManuallyW descr points bool = buildTest ("Test MANUAL pentru " ++ descr) points
      "test manual nevalidat" bool

-- |The same as 'testManuallyW', but with a unitary weight.
testManually :: String      -- ^ the description or name of the test
            -> Bool         -- ^ whether the test passes or not.
            -> TestCase     -- ^ the resulting 'TestCase' instance
testManually descr = testManuallyW descr 1


{-| 
    Tests if two sets have the same elements.
    See 'testSetW'.
-}
sameElements :: (Eq a) => [a] -> [a] -> Bool
sameElements xs ys = null (xs \\ ys) && null (ys \\ xs)

{-|
    Builds the description of a a test case. Arguments are exactly the fields in 
    'TestCase', with the exception with the number of the exercise, which is not
    currently known.
    
    See 'TestCase' for more details.
-}
buildTest :: String         -- ^ the message in case the test fails
            -> Float        -- ^ the weight of the test
            -> String       -- ^ the name of the test
            -> Bool         -- ^ the actual test
            -> TestCase     -- ^ the resulting 'TestCase' instance
buildTest = TestCase 0

{-|
    Builds a TestCaseResult based on running a TestCase.
    The test is evaluated (if it was not already evaluated), elements are added to the
    output message (e.g. exercise number, result information), and the score is put into the
    result if the test passed.
    
    Exceptions are not handled here, but in runTestCase if multiple tests are run, or otherwise
    they are unhandled, if a single exercise is checked.
-}
testResult :: TestCase -> TestCaseResult
testResult tc = TestCaseResult (testIndex tc) -- index
                ((if tp then "[OK] " else "[--] ")
                  ++ testDescr tc ++ " " ++
                  putResult tp (testError tc)) -- message
                (if tp then testWeight tc else 0) -- score
                False -- is undefined
                False -- is excepted
  where
    tp = testPassed tc
    putResult p e = if p then "rezolvat." else "esuat: " ++ e
            

{-|
    Runs a test case, handling a potential exception.
    
    It is guaranteed that the test will run no later than in this function.
 -}
runTestCase :: Bool         -- ^ if True, information on points will be added to the output
                -> Int      -- ^ the total number of exercises, which may be needed in the output
                -> TestCase -- ^ the test case to run
                -> IO TestCaseResult    -- ^ the result of the test.
runTestCase showPoints nEx tc = do
  E.catch (do
             putStr $ if testPassed tc then "" else "" -- force generating exception    
             return $ let res = testResult tc in
                    res { testMessage = showEx showPoints nEx (testIndex tc) ++ testMessage res ++
                      (if testPassed tc && showPoints then " +" ++ showPt (testScore res) else "") } )
          (\ (E.SomeException e) -> do
             return $
              TestCaseResult 
                  (testIndex tc) -- index
                  (showEx showPoints nEx (testIndex tc) ++ "[-/] " ++ testDescr tc 
                    ++ ": EXCEPTIE: " ++ (intercalate "\n\t\t" $ map (drop 1) $ groupBy (\a b -> b /= '\n') $ " " ++ show e)) -- message
                  0 -- score
                  (take (length "Prelude.undefined") (show e) == "Prelude.undefined")
                  True) -- excepted

{-|
    Runs a list of 'TestData', using 'runTestCase'.
    
    In vmchecker mode (@vmchecker@ = True), it prepends points and displays all output.
    
    If not in vmchecker mode, points are shown only at the end and output is shortened.
    It displays the results of at most @showDefaults@ exercises where all tests throw the
    undefined exception. It displays the output of all the other exercices.
 -}
runTests :: Bool        -- ^ if True, run in vmchecker mode
         -> Bool        -- ^ if True, show test points in the output and total points at the end
         -> Int         -- ^ when in non-vmchecker mode, the number of undefined exercises to display
         -> [TestData]  -- ^ the test data, as a list of 'TestCase' lists
         -> IO ()       -- ^ tests are run in sequence
runTests checkerMode showPoints showDefaults tests = do
  results <- mapM (mapM (runTestCase showPoints $ length tests) . getTestCases) tests
  let 
    res = foldl showResult ([], []) results 
    in mapM putStr (reverse (fst res) ++
      if checkerMode || null (snd res) then []
      else ["Exercitii care nu sunt implementate: " ++ (show $ reverse $ snd res) ++ "\n"])
  printFinalGrade results showPoints
  where
    showResult (out, notdone) r = 
        if not checkerMode && isUndefined && length notdone >= showDefaults
                  then (out, notdoneNew)
                  else (concatMap printRes r : out, notdoneNew)
      where
        isUndefined = (and $ map testExcepted r) && (or $ map testSubjectUndefined r)
        notdoneNew = if isUndefined then testExercise (head r) : notdone else notdone
        printRes r = (if checkerMode then "+" ++ showPt (testScore r) ++ " " else "")
             ++ testMessage r ++ "\n"
    printFinalGrade rr True = do
      putStr $ "total: "
      putStrLn $ showPt $ sum $ concat $ map (map testScore) rr
    printFinalGrade rr False = do
      putStrLn ""

{-|
    Use this for a laboratory situation.
    
    Prints only the first 2 of the undefined exercises. See 'runTests'.
    
    The test data is normally a list of 'tests', 'test', 'tests_', or 'test_' output.
 -}
quickCheck :: Bool      -- ^ Whether to show any information about points
     -> [TestData]      -- ^ The test data, as a list of 'TestCase' lists
     -> IO ()
quickCheck showPoints = runTests False showPoints 2

{-|
    Use this for the vmchecker.
    
    Prepends each test case with the points accumulated for the test. See 'runTests'.
    
    The test data is normally a list of 'tests', 'test', 'tests_', or 'test_' output.
 -}
vmCheck :: [TestData]   -- ^ the test data, as a list of 'TestCase' lists
             -> IO ()
vmCheck = runTests True True undefined

{-|
    Use this for the vmchecker, in the case when some tests need to be run inside the IO monad.
    
    The same with 'vmCheck', just that instead of a list of 'TestData', it works over a list
    of @IO TestData@.
    
    The test data is normally a list of 'tests', 'test', 'tests_', or 'test_' output.
 -}
vmCheckIO :: [IO TestData]   -- ^ The test data, as a list of 'TestCase' lists inside 'IO'
             -> IO ()
vmCheckIO tests = do
    td <- sequence tests
    runTests True True undefined td