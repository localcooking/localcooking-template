{-# LANGUAGE
    OverloadedStrings
  #-}

{-|

Module: LocalCooking.Server.Dependencies
Copyright: (c) 2018 Local Cooking Inc.
License: Proprietary
Maintainer: athan.clark@localcooking.com
Portability: GHC

-}

module LocalCooking.Server.Dependencies where

import LocalCooking.Server.Dependencies.AuthToken (authTokenServer)
import LocalCooking.Server.Dependencies.Register (registerServer)
import LocalCooking.Server.Dependencies.UserEmail (userEmailServer)
import LocalCooking.Server.Dependencies.UserRoles (userRolesServer)
import LocalCooking.Server.Dependencies.Security (securityServer)
import LocalCooking.Server.Dependencies.PasswordVerify (passwordVerifyServer)
import LocalCooking.Types (AppM)

import Web.Routes.Nested (l_, o_, (</>))
import Web.Dependencies.Sparrow (Topic (..), unpackServer, SparrowServerT, match, matchGroup)
import Network.Wai.Trans (MiddlewareT)


-- | Enterprise-wide dependencies - auth tokens, registration, user details, etc.
dependencies :: SparrowServerT (MiddlewareT AppM) AppM () -- ^ Extra deps
             -> SparrowServerT (MiddlewareT AppM) AppM ()
dependencies deps = do
  matchGroup (l_ "template" </> o_) $ do
    match (l_ "authToken" </> o_)
      =<< unpackServer (Topic ["template", "authToken"]) authTokenServer
    match (l_ "register" </> o_)
      =<< unpackServer (Topic ["template", "register"]) registerServer
    match (l_ "userEmail" </> o_)
      =<< unpackServer (Topic ["template", "userEmail"]) userEmailServer
    match (l_ "userRoles" </> o_)
      =<< unpackServer (Topic ["template", "userRoles"]) userRolesServer
    match (l_ "security" </> o_)
      =<< unpackServer (Topic ["template", "security"]) securityServer
    match (l_ "passwordVerify" </> o_)
      =<< unpackServer (Topic ["template", "passwordVerify"]) passwordVerifyServer
  deps
