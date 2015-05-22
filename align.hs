{-# Language OverloadedStrings #-}
import Prelude hiding (getContents, putStr, lines)
import qualified Data.Text as T
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
    let dIndex   = maximum $ fmap (maxIndex (delim d)) lines
    let newLines = fmap (format d dIndex) lines
    putStr $ T.unlines newLines

delimSet :: [Delim]
delimSet = [ 
        Delim "=" RJustify 1, 
        Delim ":" LJustify 0,
        Delim "::" RJustify 1,
        Delim "=>" RJustify 1,
        Delim "->" RJustify 1,
        Delim "<-" LJustify 1
    ]

chooseDelim :: [Delim] -> [T.Text] -> Delim
chooseDelim ds bs = last $ sortWith countDelim ds
    where dElem d      = T.isInfixOf (delim d)
          countDelim d = length $ filter (dElem d) bs

maxIndex :: T.Text -> T.Text -> Int
maxIndex d t = maybe 0 id $ do
    (f, _) <- headMay . (T.breakOnAll d) $ t
    return $ T.length f

-- Take the delim and a line, producing a formatted line
format :: Delim -> Int -> T.Text -> T.Text
format (Delim d at buff) off line = maybe line id $ do
    index <- headMay . fmap (T.length . fst) (T.breakOnAll d line)
    let dLen            = T.length d
    let (left, right)   = (T.take index line, T.drop (index + dLen) line)
    let (sLeft, sRight) = (T.stripEnd left, T.stripStart right)
    let (lPad, rPad)    = justify at off buff (sLeft, sRight)
    return $ T.concat [sLeft, lPad, d, rPad, sRight]

pad = flip T.replicate " "
        
-- Return the whitespace
justify :: AlignType -> Int -> Int -> (T.Text, T.Text) -> (T.Text, T.Text)
justify LJustify dLoc buff (lT, _) = (pad lWhitespace, pad rWhitespace)
    where lWhitespace = buff
          rWhitespace = dLoc - (T.length lT) + 1

justify RJustify dLoc buff (lT, _) = (pad lWhitespace, pad rWhitespace)
    where lWhitespace = dLoc - (T.length lT)
          rWhitespace = buff

