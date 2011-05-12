{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-|

Generic combinators for building splices.

-}

module Snap.Heist.Helpers where

import Text.Templating.Heist

conditionalSplice :: Monad m => Bool -> Splice m
conditionalSplice False = return []
conditionalSplice True = runChildren
