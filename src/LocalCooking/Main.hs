{-# LANGUAGE
    OverloadedStrings
  #-}

module LocalCooking.Main where

import LocalCooking.Main.Options (args, mkEnv)
import LocalCooking.Server (LocalCookingArgs, server)
import LocalCooking.Types (runAppM)
import LocalCooking.Types.Env (Env, releaseEnv)
import LocalCooking.Links.Class (LocalCookingSiteLinks)

import Options.Applicative (execParser, info, helper, fullDesc, progDesc, header)
import Data.Monoid ((<>))
import Control.Exception.Safe (bracket)
-- import System.Posix.User (getLoginName)
import System.Environment (getEnv)
import Control.Logging (withStderrLogging)
import Path.Extended (ToLocation, FromLocation)


defaultMain :: LocalCookingSiteLinks siteLinks
            => FromLocation siteLinks
            => ToLocation siteLinks
            => String -- ^ Heading
            -> LocalCookingArgs siteLinks sec
            -> IO ()
defaultMain head' lcArgs = do
  username <- getEnv "USER"

  let allocate :: IO (Env, Int)
      allocate = do
        as <- execParser (opts username)
        mkEnv as

      release :: (Env, Int) -> IO ()
      release (e,_) =
        releaseEnv e

  withStderrLogging $
    bracket allocate release $ \(env, port) ->
      runAppM (server port lcArgs) env

  where
    opts u = info (helper <*> args u) $ fullDesc <> progDesc desc <> header head'
    desc = "Start the daemon"
