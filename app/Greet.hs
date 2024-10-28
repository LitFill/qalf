{-# LANGUAGE OverloadedStrings #-}
module Greet where

import Data.String qualified as S

greet :: (Semigroup a, S.IsString a) => a -> a
greet name = "Hello, " <> name <> "!"
