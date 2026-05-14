import Lib

import Test.QuickCheck
import qualified Data.Text as T
import Data.Char


main :: IO ()
main = do
    putStrLn "Here we go!"
    putStrLn "Проверка коммутативности"
--    quickCheck prop_kommutative
    putStrLn "Проверка сложения по модулю"
    quickCheck (prop_add_module)
    putStrLn "Проверка нейтрального элемента"
--    quickCheckWith stdArgs{maxSuccess = 1000} prop_neutral_el

    putStrLn "========================================="

    putStrLn "Проверка пустой строки"
    quickCheck (prop_emptyStr)

    putStrLn "Проверка строки из одного слова"
    quickCheck (prop_oneSymStr2)
    

-- ============================== TESTS for addMod =================================

prop_kommutative :: Int -> Int -> Int -> Property
prop_kommutative x y m = (m > 0) ==> classify (x > 100) "big x" $ (addMod x y m) == (addMod y x m)
    where cond = x > 100

prop_add_module :: Int -> Int -> Int -> Property
prop_add_module x y m = (m > 0) ==> (addMod x y m) == (x + y) `mod` m

prop_neutral_el :: Int -> Int -> Property
prop_neutral_el x m = (m > 0) ==> (addMod x 0 m) == x `mod` m



-- =========================== TESTS for reverseWords ===================================


instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary
  shrink t = T.pack <$> shrink (T.unpack t)

prop_emptyStr :: Bool
prop_emptyStr = ("" == reverseWords "")

prop_oneSymStr :: T.Text -> Property
prop_oneSymStr sym = not (T.any isSpace sym) ==> (tsym == reverseWords2 tsym)
    where tsym = trim2 $ sym

prop_oneSymStr2 :: String -> Property
prop_oneSymStr2 sym = not (any isSpace sym) ==> (sym == reverseWords sym)

