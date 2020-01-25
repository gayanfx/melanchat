module Client.IM.Chat where

import Client.Common
import Client.Types
import Prelude
import Shared.Types

import Data.Argonaut.Core as DAC
import Data.Argonaut.Encode.Generic.Rep as DAEGR
import Data.Array ((!!))
import Data.Array as DA
import Data.Array.NonEmpty as DAN
import Data.Int53 as DI
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Effect.Aff (Aff)
import Effect.Aff as EA
import Effect.Class (liftEffect)
import Effect.Console as EC
import Flame (World)
import Partial.Unsafe as PU
import Shared.JSON as SJ
import Web.Socket.WebSocket (WebSocket)
import Web.Socket.WebSocket as WSW

webSocketHandler :: WebSocketHandler
webSocketHandler = { sendString: WSW.sendString }

update :: World IMModel IMMessage -> IMModel -> ChatMessage -> Aff IMModel
update _ model =
        case _ of
                SendMessage content -> do
                        model' <- startChat model
                        sendMessage webSocketHandler model' content
                ReceiveMessage payload -> receiveMessage model payload

startChat :: IMModel -> Aff IMModel
startChat immodel@(IMModel model@{chatting, contacts, suggesting, suggestions}) =
        pure <<< case chatting of
                        Nothing ->
                                let chatted = PU.unsafePartial (DM.fromJust $ DAN.index suggestions suggesting)
                                in IMModel $ model {
                                        chatting = Just 0,
                                        contacts = DA.cons chatted contacts
                                }
                        _ -> immodel

sendMessage :: WebSocketHandler -> IMModel -> String -> Aff IMModel
sendMessage webSocketHandler (IMModel model@{webSocket: Just (WS webSocket), token: Just token, temporaryID, chatting: Just chatting, contacts}) content = do
        let     (IMUser user) = PU.unsafePartial $ DM.fromJust (contacts !! chatting)
                newTemporaryID = temporaryID + 1
                updatedChatting = IMUser $ user {
                        message = "",
                        history = DA.snoc user.history $ History { content }
                }
        liftEffect <<< webSocketHandler.sendString webSocket <<< SJ.toJSON $ Message {
                id: PrimaryKey $ DI.fromInt newTemporaryID,
                user: user.id,
                token: token,
                content
        }
        pure <<< IMModel $ model {
                temporaryID = newTemporaryID,
                contacts = PU.unsafePartial (DM.fromJust $ DA.updateAt chatting updatedChatting contacts)
        }
sendMessage _ model _ = do
        liftEffect $ EC.log "Invalid sendMessage state"
        pure model

receiveMessage :: IMModel -> WebSocketPayload -> Aff IMModel
receiveMessage (IMModel model@{contacts}) payload = do
        case payload of
                Message { id, user, content } -> liftEffect $ EC.log content
                Received { previousID, id } -> liftEffect $ EC.log "received id"
                e -> liftEffect <<< EC.log $ "bogus payload " <> show e
        pure model