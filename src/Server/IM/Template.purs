module Server.IM.Template where

import Prelude
import Shared.Types

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Flame (QuerySelector(..))
import Flame as F
import Flame.HTML.Attribute as HA
import Flame.HTML.Element as HE
import Server.Template (defaultParameters)
import Data.Array as DA
import Server.Template as ST
import Shared.IM.View as SIV

template :: Array IMUser -> Array IMUser -> IMUser -> Effect String
template contacts suggestions user = do
        parameters <- ST.extendParameters $ defaultParameters {
                javascript = javascript,
                css = css
        }
        F.preMount (QuerySelector ".im") {
                view: \model' -> ST.templateWith $ parameters { content = [SIV.view model'] },
                init: IMModel {
                        contacts,
                        suggestions,
                        user,
                        chatting: Nothing,
                        webSocket: Nothing,
                        token: Nothing,
                        temporaryID : 0,
                        suggesting: if DA.null suggestions then Nothing else Just 0
                }
        }
        where   javascript = [
                        HE.script' [HA.type' "text/javascript", HA.src "/client/javascript/im.bundle.js"]
                ]
                css = [
                        HE.link [HA.rel "stylesheet", HA.type' "text/css", HA.href "/client/css/im.css"],
                        HE.link [HA.rel "stylesheet", HA.href "https://cdn.jsdelivr.net/simplemde/latest/simplemde.min.css"]
                ]