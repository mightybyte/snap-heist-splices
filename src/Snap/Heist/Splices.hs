{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Snap.Heist.Splices where

import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Text as T
import           Data.Text.Encoding
import           Snap.Auth
import           Snap.Auth.Handlers
import           Snap.Extension.DB.MongoDB hiding (u)
import           Snap.Types
import           Text.Templating.Heist
import           Text.XmlHtml

import           Snap.Heist.Helpers

------------------------------------------------------------------------------
-- | Pre-made list of all the splices that we can build for you.
snapHeistSplices :: MonadSnap m => [(T.Text, Splice m)]
snapHeistSplices =
    [ ("rqparam", paramSplice)
    , ("bindParams", bindParamsSplice)
    , ("ifLoggedIn", ifLoggedIn)
    , ("ifGuest", ifGuest)
    , ("requireAuth", requireAuth)

    -- Example
    --, ("currentUser", userField username)
    ]

------------------------------------------------------------------------------
-- Splices for request parameters
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- | Gets the value of a request parameter.  Example use:
--
-- <rqparam name="username"/>
paramSplice :: MonadSnap m => Splice m
paramSplice = do
    res <- runMaybeT $
        (MaybeT . lift . getParam . encodeUtf8) =<<
        MaybeT (liftM (getAttribute "name") getParamNode)
    return $ maybe [] ((:[]) . TextNode . decodeUtf8) res


------------------------------------------------------------------------------
-- | Binds all request parameters to splices with the 'p' namespace.  The
-- splice itself renders as nothing, but on the rest of the page you'll be
-- able to use tags to get any of the request parameters.  Example:
--
-- <bindParams/><p:username/>
bindParamsSplice :: MonadSnap m => Splice m
bindParamsSplice = do
    params <- lift getParams
    let f k v accum = modifyTS (bindString ("p:" `T.append` decodeUtf8 k)
                                           (decodeUtf8 $ B.concat v)) : accum
    sequence $ M.foldrWithKey f [] params
    return []

------------------------------------------------------------------------------
-- Auth splices
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- | Renders the child nodes only if the request comes from an authenticated
-- user.
ifLoggedIn :: (MonadAuth m, MonadMongoDB m) => Splice m
ifLoggedIn = conditionalSplice =<< lift isLoggedIn


------------------------------------------------------------------------------
-- | Renders the child nodes only if the request comes from a user that is not
-- logged in.
ifGuest :: (MonadAuth m, MonadMongoDB m) => Splice m
ifGuest = conditionalSplice . not =<< lift isLoggedIn


------------------------------------------------------------------------------
-- | Returns any field of the user data structure as a text node.  This allows
-- you to easily implement a <currentUser/> splice.
userField :: (MonadAuth m, MonadMongoDB m, Val a)
          => (a -> T.Text) -> Splice m
userField field = do
    mu <- lift currentAuthUser
    return $ maybe [] (\u -> [TextNode $ field u]) $ (cast' . Doc . snd) =<< mu


------------------------------------------------------------------------------
-- | Prevents the whole page from rendering if the user isn't logged in.
requireAuth :: (MonadAuth m, MonadMongoDB m) => Splice m
requireAuth = do
    lift $ requireUser pass (return [])

