module Misc ((+++), changeLeft, mapLeft) where

import qualified Data.Text as T

(+++) :: T.Text -> T.Text -> T.Text
(+++) = T.append

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x) = Left (f x)
mapLeft _ (Right x) = Right x

changeLeft :: a -> Either b c -> Either a c
changeLeft = mapLeft . const
