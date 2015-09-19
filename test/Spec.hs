import Excercises

main :: IO ()
main = do
  putStrLn ""
  putStrLn "- TESTS -----------------------------------"

  report "furryList" $ assertEqual [2,4,6] $ furry (*2) [1,2,3]
  report "furryEmptyList" $  assertNull $ furry (*2) []
  report "furryJust" $ assertEqual (Just 4) $ furry (*2) (Just 2)
  report "furryNothing" $ assertEqual Nothing $ furry (*2) Nothing
  report "furryFun" $ assertEqual "6" $ furry (show . (*2)) (+1) 2

  let leftLeft2 = EitherLeft $ Left 2 :: EitherLeft String Int
  let leftLeft4 = EitherLeft $ Left 4 :: EitherLeft String Int
  let leftRight = EitherLeft $ Right "oops" :: EitherLeft String Int
  report "furryEitherLeftLeft" $ assertEqual leftLeft4 $ furry (*2) leftLeft2
  report "furryEitherLeftRight" $ assertEqual leftRight $ furry (*2) leftRight

  let rightRight2 = EitherRight $ Right 2 :: EitherRight String Int
  let rightRight4 = EitherRight $ Right 4 :: EitherRight String Int
  let rightLeft  = EitherRight $ Left "oops" :: EitherRight String Int
  report "furryEitherRightRight" $ assertEqual rightRight4 $
                                    furry (*2) rightRight2
  report "furryEitherRightLeft" $ assertEqual rightLeft $ furry (*2) rightLeft

  report "furry prime just" $ assertEqual (Just 4) $ furry' (*2) (Just 2)
  report "furry prime nothing" $ assertEqual Nothing$ furry' (*2) Nothing

  report "unicorn maybe" $ assertEqual (Just 3) $ unicorn 3
  report "banana just" $ assertEqual (Just 4) $
                           banana (\x -> Just $ 2 * x) $ Just 2
  report "banana nothing" $ assertEqual Nothing $
                           banana (\x -> Just $ 2 * x) Nothing

  report "unicorn list" $ assertEqual [3] $ unicorn 3
  report "banana nonempty list" $ assertEqual [2,4,6] $
                           banana (\x -> [x * 2]) [1,2,3]
  report "banana empty list" $ assertEqual [] $
                           banana (\x -> [x * 2]) []

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
