
import Roman
import Test.Hspec
import Data.List
import Data.Maybe
import Data.Char (isAlpha)
import Control.Monad
import Control.Monad.Trans.List -- deprecated, but only just learning about Monad Transformers
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)

is :: Int -> String -> SpecWith ()
is n s = it ((show n) <> " = " <> s) $ toRoman2 n `shouldBe` s

main :: IO ()
main = hspec $ do

  describe "Single Characters:" $ do
    0 `is` "" 
    1 `is` "I"
    5 `is` "V"
    10 `is` "X"
    50 `is` "L"
    100 `is` "C"
    500 `is` "D"
    1000 `is` "M"

  describe "Compound Characters:" $ do
    2 `is` "II"
    6 `is` "VI"
    8 `is` "VIII"
    111 `is` "CXI"
    62 `is` "LXII"
    1803 `is` "MDCCCIII"
    3000 `is` "MMM"

  describe "Tricky ones! " $ do
    4 `is` "IV"
    9 `is` "IX"
    14 `is` "XIV"
    39 `is` "XXXIX"
    40 `is` "XL"
    44 `is` "XLIV"
    48 `is` "XLVIII"
    49 `is` "XLIX"
    99 `is` "XCIX"
    400 `is` "CD"
    448 `is` "CDXLVIII"
    499 `is` "CDXCIX"
    1903 `is` "MCMIII"
    3000 `is` "MMM"

  describe "Full acceptance test!" $ do
    cases <- (runIO . runListT) getTestData
    forM_ cases (uncurry is)

-- getTestData :: IO [(Int, String)]
getTestData :: ListT IO (Int, String)
getTestData = fmap splitIntoExpected (readFileLines "1-3000.txt")

readFileLines :: String -> ListT IO String
readFileLines filePath = do
  fileContents <- fmap lines $ liftIO (readFile filePath)
  ListT (return fileContents)
-- ^ This could just be one line: (but wanted to use liftIO)
-- readFileLines filePath = ListT $ readFile filePath >>= return . lines

splitIntoExpected :: String -> (Int, String)
splitIntoExpected s = (number, expectedNumeral)
  where (input, expected) = splitAt equalsPos s
        equalsPos         = fromJust $ elemIndex '=' s
        number            = read input :: Int
        expectedNumeral   = filter isAlpha $ tail expected