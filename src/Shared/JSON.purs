module Shared.JSON where

import Prelude

import Data.Argonaut.Core as DAC
import Data.Argonaut.Decode.Generic.Rep (class DecodeRep)
import Data.Argonaut.Decode.Generic.Rep as DADGR
import Data.Argonaut.Encode.Generic.Rep (class EncodeRep)
import Data.Argonaut.Encode.Generic.Rep as DAEGR
import Data.Argonaut.Parser as DAP
import Data.Either (Either)
import Data.Generic.Rep (class Generic)

fromJSON :: forall v value. Generic value v => DecodeRep v => String -> Either String value
fromJSON content = do
        json <- DAP.jsonParser content
        DADGR.genericDecodeJson json

toJSON :: forall v value. Generic value v => EncodeRep v => value -> String
toJSON = DAC.stringify <<< DAEGR.genericEncodeJson