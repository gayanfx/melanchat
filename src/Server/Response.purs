module Server.Response(
        html,
        json,
        serveDevelopmentFile,
        requestError,
        throwInternalError,
        throwBadRequest,
        redirect
) where

import Prelude
import Server.Types
import Shared.Types

import Data.Argonaut.Core as DAC
import Data.Argonaut.Decode.Generic.Rep (class DecodeRep)
import Data.Argonaut.Encode as DAE
import Data.Argonaut.Encode.Generic.Rep (class EncodeRep)
import Data.Argonaut.Encode.Generic.Rep as DAEGR
import Data.Argonaut.Parser as DAP
import Data.Either as DE
import Data.Generic.Rep (class Generic)
import Shared.Unsafe as SU
import Data.Maybe (Maybe(..))
import Shared.JSON as SJ
import Data.Maybe as DM
import Data.String as DS
import Data.String.Read (class Read)
import Data.String.Read as DSR
import Effect.Console as EC
import HTTPure (Headers, Response, ResponseM, Path)
import HTTPure as H
import HTTPure.Body (class Body)
import Node.FS.Aff as NFA
import Node.Path as NP
import Partial.Unsafe as PU
import Partial.Unsafe as UP
import Run (Run, AFF, EFFECT)
import Run as R
import Run.Except as RE
import Server.NotFound.Template as SNT

data ContentType = JSON | JS | GIF | JPEG | PNG | CSS | HTML | OctetStream

instance contentTypeShow :: Show ContentType where
        show JSON = "application/json"
        show JS = "application/javascript"
        show GIF = "image/gif"
        show JPEG = "image/jpeg"
        show PNG = "image/png"
        show CSS = "text/css"
        show HTML = "text/html"
        show _ = "application/octet-stream"

-- match both on file extension or content type
instance contentTypeRead :: Read ContentType where
        read v =
                Just $
                        if value == ".json" || value == show JSON then JSON
                         else if value == ".js" || value == show JS then JS
                         else if value == ".gif" || value == show GIF then GIF
                         else if value == ".jpeg" || value == ".jpg" || value == show JPEG then JPEG
                         else if value == ".png" || value == show PNG then PNG
                         else if value == ".css" || value == show CSS then CSS
                         else if value == ".html" || value == show HTML then HTML
                         else OctetStream
                where value = DS.trim $ DS.toLower v

ok' :: forall response. Body response => Headers -> response -> ResponseEffect
ok' headers = R.liftAff <<< H.ok' headers

html :: String -> ResponseEffect
html contents = ok' (headerContentType $ show HTML) contents

json :: forall response r. Generic response r => EncodeRep r => response -> ResponseEffect
json = ok' (headerContentType $ show JSON) <<< SJ.toJSON

serveDevelopmentFile :: String -> String -> ResponseEffect
serveDevelopmentFile folder fileName = do
              contents <- R.liftAff <<< NFA.readFile $ "src/Client/" <> folder <> "/" <> fileName
              ok' (contentTypeFromExtension fileName) contents

contentTypeFromExtension :: String -> Headers
contentTypeFromExtension = headerContentType <<< show <<< read' <<< NP.extname
        where   read' :: String -> ContentType
                read' = SU.unsafeFromJust "contentTypeFromExtension" <<< DSR.read

headerContentType :: String -> Headers
headerContentType = H.header "Content-Type"

requestError :: ResponseError -> Run (aff :: AFF, effect :: EFFECT) Response
requestError ohno = do
        R.liftEffect <<< EC.log $ "internal server error " <> show ohno
        case ohno of
                baddie@(BadRequest { reason }) -> liftedJSONResponse H.badRequest' reason
                err@(InternalError { reason }) -> liftedJSONResponse H.internalServerError' reason
                unfound@(NotFound { reason, isPost }) ->
                        if isPost then
                                liftedJSONResponse (const <<< H.notFound') reason
                         else do
                                contents <- R.liftEffect SNT.template
                                R.liftAff $ H.ok' (headerContentType $ show HTML) contents
        where liftedJSONResponse handler = R.liftAff <<< handler (headerContentType $ show JSON) <<< DAC.stringify <<< DAE.encodeJson

throwInternalError :: forall whatever. String -> ServerEffect whatever
throwInternalError reason = RE.throw $ InternalError { reason: reason }

throwBadRequest :: forall whatever. String -> ServerEffect whatever
throwBadRequest reason = RE.throw $ BadRequest { reason: reason }

redirect :: String -> ResponseEffect
redirect = R.liftAff <<< flip H.temporaryRedirect' "" <<< H.header "Location"