{-# Language OverloadedStrings #-}
import Prelude hiding (getContents, putStr, lines)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import Data.Text.IO (getContents, putStr)
import GHC.Exts (sortWith)
import Safe (headMay)

data AlignType = LJustify | RJustify deriving (Show)
data Delim = Delim { delim     :: T.Text
                   , alignment :: AlignType
                   , buffer    :: Int
                   } deriving (Show)

main = do
    contents <- getContents
    let lines    = T.lines contents
    let d        = chooseDelim delimSet lines
    let getIndex = fromMaybe 0 . maxIndex (delim d)
    let dIndex   = maximum $ fmap getIndex lines
    let newLines = fmap (format d dIndex) lines
    putStr $ T.unlines newLines

delimSet :: [Delim]
delimSet = [ 
        Delim ":" LJustify 0,
        Delim "=" RJustify 1, 
        Delim "::" RJustify 1,
        Delim "=>" RJustify 1,
        Delim "->" RJustify 1,
        Delim "<-" RJustify 1,
        Delim "==" RJustify 1
    ]

chooseDelim :: [Delim] -> [T.Text] -> Delim
chooseDelim ds bs = last $ sortWith countDelim ds
    where dElem d      = T.isInfixOf (delim d)
          countDelim d = length $ filter (dElem d) bs

maxIndex :: T.Text -> T.Text -> Maybe Int
maxIndex d t = do
    (f, _) <- breakOnMay d t
    return . T.length . T.stripEnd $ f

breakOnMay :: T.Text -> T.Text -> Maybe (T.Text, T.Text)
breakOnMay d = headMay . (T.breakOnAll d)

-- Take the delim and a line, producing a formatted line
format :: Delim -> Int -> T.Text -> T.Text
format (Delim d at buff) off line = fromMaybe line $ do
    (left, right)      <- breakOnMay d line
    let dLen = T.length d
    let (sLeft, sRight) = (T.stripEnd left, T.stripStart . T.drop dLen $ right)
    let (lPad, rPad)    = justify at off buff (sLeft, sRight)
    return $ T.concat [sLeft, lPad, d, rPad, sRight]

pad = flip T.replicate " "
        
-- Return the whitespace
justify :: AlignType -> Int -> Int -> (T.Text, T.Text) -> (T.Text, T.Text)
justify LJustify dLoc buff (lT, _) = (pad lWhitespace, pad rWhitespace)
    where lWhitespace = buff
          rWhitespace = dLoc - (T.length lT) + 1

justify RJustify dLoc buff (lT, _) = (pad lWhitespace, pad rWhitespace)
    where lWhitespace = dLoc - (T.length lT) + 1
          rWhitespace = buff

