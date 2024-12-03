module VigenereDecrypt where

import qualified Data.Char as Char
import System.Environment (getArgs)

decrypt :: String -> String -> String
decrypt key text = undefined

-- >>> decrypt "giovan" "ZPSPNOXMOFAORMQDPUKZ"
-- "theunbreakablecipher"

-- >>> decrypt "battista" $ encrypt "battista" "a simple example"
-- "a simple example"

main :: IO ()
main = error "TODO"
