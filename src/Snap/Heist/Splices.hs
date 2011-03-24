{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Snap.Heist.Splices where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Text as T
import           Data.Text.Encoding
import Snap.Types
import Text.Templating.Heist
import Text.XmlHtml


------------------------------------------------------------------------------
paramSplice :: MonadSnap m => Splice m
paramSplice = do
    res <- runMaybeT $
        (MaybeT . lift . getParam . encodeUtf8) =<<
        MaybeT (liftM (getAttribute "name") getParamNode)
    return $ maybe [] ((:[]) . TextNode . decodeUtf8) res


------------------------------------------------------------------------------
bindParamsSplice :: MonadSnap m => Splice m
bindParamsSplice = do
    params <- lift getParams
    let f k v accum = modifyTS (bindString ("p:" `T.append` decodeUtf8 k)
                                           (decodeUtf8 $ B.concat v)) : accum
    sequence $ M.foldWithKey f [] params
    return []

snapHeistSplices :: MonadSnap m => [(T.Text, Splice m)]
snapHeistSplices =
    [ ("param", paramSplice)
    , ("bindParams", bindParamsSplice)
    ]
