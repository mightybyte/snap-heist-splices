{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-|

Generic combinators for building splices.

-}

module Snap.Heist.Helpers where

conditionalSplice :: Monad m => Bool -> Splice m
conditionalSplice False = return []
conditionalSplice True = runChildren
