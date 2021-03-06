module Server.WebSocket.Events where

import Prelude
import Server.Types
import Shared.Types

import Data.Argonaut.Decode.Generic.Rep as DADGR
import Data.Either (Either(..))
import Data.Int53 (Int53)
import Data.Map (Map)
import Data.Map as DM
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff as EA
import Effect.Console as EC
import Effect.Exception (Error)
import Effect.Ref (Ref)
import Effect.Now as EN
import Effect.Ref as ER
import Node.HTTP (Request)
import Run as R
import Data.Tuple(Tuple(..))
import Run.Except as RE
import Run.Reader as RR
import Server.IM.Database as SID
import Debug.Trace(spy)
import Server.Token as ST
import Server.WebSocket (WebSocketConnection, WebSocketMessage(..), CloseCode, CloseReason)
import Server.WebSocket as SW
import Shared.JSON as SJ

handleError :: Error -> Effect Unit
handleError = EC.log <<< show

--here it seems like the only way is get the cookie token and transform into a post token
handleClose :: Configuration -> Ref (Map Int53 WebSocketConnection) -> Request -> CloseCode -> CloseReason -> Effect Unit
handleClose (Configuration configuration) allConnections request _ _ = pure unit

handleMessage :: WebSocketConnection -> WebSocketMessage -> WebSocketEffect
handleMessage connection (WebSocketMessage message) = do
        possiblePayload <- pure $ SJ.fromJSON message
        case possiblePayload of
                Right payload -> do
                        { allConnections } <- RR.ask
                        case payload of
                                Connect token -> withUser token $ \userID -> R.liftEffect $ ER.modify_ (DM.insert userID connection) allConnections
                                ServerMessage {id, user: recipient@(PrimaryKey recipientID), token, content} -> withUser token $ \userID -> do
                                        date <- map MDateTime $ R.liftEffect EN.nowDateTime
                                        Tuple messageID senderUser <- SID.insertMessage (PrimaryKey userID) recipient content
                                        sendMessage connection <<< SJ.toJSON $ Received {
                                                previousID: id,
                                                id: messageID
                                        }

                                        possibleRecipientConnection <- R.liftEffect (DM.lookup recipientID <$> ER.read allConnections)

                                        case possibleRecipientConnection of
                                                Nothing -> pure unit
                                                Just recipientConnection -> sendMessage recipientConnection <<< SJ.toJSON $ ClientMessage {
                                                        id : messageID,
                                                        user: senderUser,
                                                        content,
                                                        date
                                                }
                Left error -> do
                        log $ "received faulty payload " <> error

        where   log = R.liftEffect <<< EC.log
                sendMessage connection = R.liftEffect <<< SW.sendMessage connection <<< WebSocketMessage

                withUser token f = do
                        {configuration: Configuration configuration} <- RR.ask
                        user <- R.liftEffect $ ST.userIDFromToken configuration.tokenSecretPOST token
                        case user of
                                Nothing -> do
                                        R.liftEffect $ SW.close connection
                                        log "closed due to auth error"
                                Just userId -> f userId

handleConnection :: WebSocketReader -> WebSocketConnection -> Request -> Effect Unit
handleConnection reading connection request = do
        SW.onError connection handleError
        SW.onClose connection $ handleClose reading.configuration reading.allConnections request
        SW.onMessage connection
                (EA.launchAff_ <<<
                 R.runBaseAff' <<<
                 RE.catch (const (pure unit)) <<<
                 RR.runReader reading <<<
                 handleMessage connection)