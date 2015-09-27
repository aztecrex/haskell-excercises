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
  report "unicorn fun" $ assertEqual 7 $ unicorn 7 "heynow"
  report "banana fun" $ assertEqual 14 $ banana (\a r -> 5 + a + r) (*2) 3

  report "unicorn either left" $ assertEqual leftLeft2 $ unicorn 2
  report "banana either left left" $ assertEqual leftLeft4 $
                                     banana (\x -> unicorn $ 2 * x) leftLeft2
  report "banana either left right" $ assertEqual leftRight $
                                     banana (\x -> unicorn $ 2 * x) leftRight

  report "unicorn either right" $ assertEqual rightRight2 $ unicorn 2
  report "banana either right right" $ assertEqual rightRight4 $
                                     banana (\x -> unicorn $ 2 * x) rightRight2
  report "banana either right left" $ assertEqual rightLeft $
                                     banana (\x -> unicorn $ 2 * x) rightLeft

  report "jellybean" $ assertEqual (Just 3) $ jellybean $ Just $ Just 3

  report "apple" $ assertEqual (Just 6) $ apple (Just 3) $ Just (*2)

  report "moppy" $ assertEqual (Just ["2","4","6"]) $
                              moppy [1,2,3] (Just . show . (*2))
  report "moppy empty" $ assertEqual (Just []) $
                              moppy [] (Just . show . (*2))
  report "moppy with a Nothing" $ assertEqual Nothing $
                              moppy [1,2,3]
                              (\x -> if x == 2
                                  then Nothing
                                  else (Just . show . (*2)) x)

  report "sausage" $ assertEqual (Just [1,2,3]) $
                                  sausage [Just 1, Just 2, Just 3]

  report "banana2 just" $ assertEqual (Just "ab") $
                                  banana2 (++) (Just "a") (Just "b")
  report "banana2 left nothing" $ assertEqual Nothing $
                                  banana2 (*) Nothing (Just 5)
  report "banana2 right nothing" $ assertEqual Nothing $
                                  banana2 (*) (Just 3) Nothing

  let f3 a b c = a ++ b ++ c
  report "banana3 just" $ assertEqual (Just"abc") $
                                 banana3 f3 (Just "a") (Just "b") (Just "c")
  report "banana3 nothing" $ assertEqual Nothing $
                                 banana3 f3 (Just [1,2,3]) Nothing (Just [5,6,7])

  let f4 a b c d = a ++ b ++ c ++ d
  report "banana4 just" $ assertEqual (Just"abcd") $
                                 banana4 f4 (Just "a") (Just "b") (Just "c")
                                            (Just "d")
  report "banana4 nothing" $ assertEqual Nothing $
                                 banana4 f4 (Just [1,2,3]) Nothing
                                 (Just [5,6,7]) (Just [8,9])


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
