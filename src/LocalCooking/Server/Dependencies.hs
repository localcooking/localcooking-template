{-# LANGUAGE
    OverloadedStrings
  #-}

module LocalCooking.Server.Dependencies where

import LocalCooking.Server.Dependencies.AuthToken (authTokenServer)
import LocalCooking.Server.Dependencies.Register (registerServer)
import LocalCooking.Server.Dependencies.UserEmail (userEmailServer)
import LocalCooking.Server.Dependencies.Security (securityServer)
import LocalCooking.Server.Dependencies.PasswordVerify (passwordVerifyServer)
import LocalCooking.Types (AppM)

import Web.Routes.Nested (RouterT, l_, o_, (</>))
import Web.Dependencies.Sparrow (Topic (..), serveDependencies, unpackServer, SparrowServerT, match, matchGroup)
import Network.Wai.Trans (MiddlewareT)


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
    match (l_ "security" </> o_)
      =<< unpackServer (Topic ["template", "security"]) securityServer
    match (l_ "passwordVerify" </> o_)
      =<< unpackServer (Topic ["template", "passwordVerify"]) passwordVerifyServer
  deps


servedDependencies :: SparrowServerT (MiddlewareT AppM) AppM ()
                   -> AppM (RouterT (MiddlewareT AppM) sec AppM ())
servedDependencies = serveDependencies . dependencies
