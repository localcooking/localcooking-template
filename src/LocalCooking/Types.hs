{-# LANGUAGE
    DeriveGeneric
  #-}

module LocalCooking.Types where

import LocalCooking.Types.Env (Env)
import qualified Data.ByteString.Lazy as LBS
import Control.Monad.Reader (ReaderT (..))
import Control.Exception.Safe (Exception)
import GHC.Generics


type AppM = ReaderT Env IO


runAppM :: AppM a -> Env -> IO a
runAppM = runReaderT



data HTTPException
  = JSONDecodingFailed LBS.ByteString
  | NoUsername
  | NoSalt
  | SaltParseError String
  deriving (Show, Eq, Ord, Generic)

instance Exception HTTPException


-- data LoginException
--   = ChallengeForWrongSessionID SessionID SessionID ChallengeID
--   | ChallengeDoesntExist ChallengeID
--   | ChallengeResponseInvalidSignature UserID SignedChallenge
--   | UserDoesntExist UserID
--   deriving (Show, Eq, Ord, Generic)

-- instance Exception LoginException


