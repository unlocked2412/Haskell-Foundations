{-# language TemplateHaskellQuotes #-}
module TH where

import qualified Language.Haskell.Meta.Parse as P
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax (Exp, Q)
import qualified Data.Text as T
import Test.QuickCheck (counterexample)

-- | Automatically adds a 'counterexample' annotation to a QuickCheck
-- 'Test.QuickCheck.Property' indicating what expression produced the
-- 'Test.QuickCheck.Property'.
ann :: QuasiQuoter
ann = doubleExp_ "ann" [| counterexample |]

-- | Turns @[doubleExp| my expression |]@ into
-- @("my expression", my expression)@. This is more flexible
-- than `ann`, but requires slightly more optimization of test code.
doubleExp :: QuasiQuoter
doubleExp = doubleExp_ "doubleExp" [| (,) |]

doubleExp_ :: String -> Q Exp -> QuasiQuoter
doubleExp_ name fun = QuasiQuoter
  { quoteExp = \s ->
      case P.parseExp s of
        -- I don't understand what's wrapped in Left here; I feel like it's supposed
        -- to give some sort of source location, but it doesn't match up with anything
        -- super-obvious.
        Left _ -> fail $ "Could not parse expression:\n  " ++ strip s
        Right e -> [| $fun s $(pure e) |]
  , quotePat = \_ -> fail $ name ++ " is not for use in patterns."
  , quoteType = \_ -> fail $ name ++ " is not for use in types."
  , quoteDec = \_ -> fail $ name ++ " is not for creating declarations."
  }

strip :: String -> String
strip = T.unpack . T.strip . T.pack
{-# INLINE strip #-}
