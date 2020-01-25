module Server.IM.Database where

import Prelude
import Server.Types
import Shared.Types

import Data.Int53 (Int53)
import Data.Tuple.Nested ((/\))
import Database.PostgreSQL (Query(..), Row1(..), Row2(..), Row3(..))
import Database.PostgreSQL as DP
import Server.Database as SD

presentationFields :: String
presentationFields = """u.id,
gender,
birthday,
name,
headline,
description,
(select name from countries where id = country) country,
(select string_agg(l.name, ','  order by name) from languages l join languagesUsers lu on l.id = lu.language and lu.speaker = u.id ) languages,
(select string_agg(name, '\n' order by name) from tags l join tagsUsers tu on l.id = tu.tag and tu.creator = u.id ) tags"""

presentUser :: Int53 -> ServerEffect IMUser
presentUser id =
        SD.single' (Query ("select " <> presentationFields <> " from users u where id = $1")) <<< Row1 $ PrimaryKey id

suggest :: Int53 -> ServerEffect (Array IMUser)
suggest id =
        SD.select (Query ("select " <> presentationFields <> " from users u where id not in (1, $1) and not exists(select 1 from histories where sender in ($1, u.id) and recipient in ($1, u.id))")) <<< Row1 $ PrimaryKey id

insertMessage sender recipient content = SD.withTransaction $ \connection -> do
        DP.execute connection (Query """select insertHistory($1, $2)""") (Row2 sender recipient)
        SD.insert' connection (Query """INSERT INTO messages(sender, recipient, content) VALUES ($1, $2, $3)""") (Row3 sender recipient content)