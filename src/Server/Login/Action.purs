module Server.Login.Action where

import Prelude
import Server.Types
import Shared.Types

import Data.Maybe (Maybe(..))
import Data.String as DS
import Run.Except as RE
import Server.Database.User as SDU
import Effect.Console as EC
import Server.Token as ST
import Run as R
import Server.Response as SRR
import Debug.Trace(spy)

invalidUserEmailMessage :: String
invalidUserEmailMessage = "Invalid email or password"

invalidLogin :: String
invalidLogin = "Email not registered or incorrect password"

login :: RegisterLogin -> ServerEffect Token
login (RegisterLogin registerLogin) = do
        when (DS.null registerLogin.email || DS.null registerLogin.password) $ SRR.throwBadRequest invalidUserEmailMessage

        maybeUser <- SDU.userBy $ Email registerLogin.email
        case maybeUser of
                Nothing -> SRR.throwBadRequest invalidLogin
                Just (User {id: PrimaryKey userID, password}) -> do
                        hashed <- ST.hashPassword registerLogin.password

                        when (hashed /= password) $ SRR.throwBadRequest invalidLogin

                        ST.createToken userID