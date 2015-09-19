import Excercises

main :: IO ()
main = do
  putStrLn ""
  putStrLn "- TESTS -----------------------------------"
  report "furryList" $ assertEqual [2,4,6] $ furry (*2) [1,2,3]
  report "furryEmptyList" $  assertNull $ furry (*2) []
  report "furryJust" $ assertEqual (Just 4) $ furry (*2) (Just 2)
  report "furryNothing" $ assertEqual Nothing $ furry (*2) Nothing
  putStrLn "-------------------------------------------"

data Test = OK | WahWah String

report :: String -> Test -> IO ()
report name OK = putStrLn $ "'" ++ name ++ "' is OK!"
report name (WahWah message) = putStrLn $ "'" ++ name ++
        "' failed (" ++ message ++ ")."

assertEqual :: (Eq a, Show a) => a -> a -> Test
assertEqual expected actual = if expected == actual
  then OK
  else WahWah $ "not equal: " ++ show (expected,actual)

assertNull :: (Show a) => [a] -> Test
assertNull xs = if null xs
  then OK
  else WahWah $ "not null:" ++ show xs
