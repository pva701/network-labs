module Service.Common
       ( requestFile
       , toUrl
       ) where

import           Data.ByteString.Lazy (ByteString)
import qualified Network.Wreq         as Wr (get, responseBody, responseStatus,
                                             statusCode)
import           Universum            hiding (ByteString)

import           DNS.Types            (IPv4)

toUrl :: (IPv4, Word16) -> FilePath -> String
toUrl (ipv4, port) path =
    "http://" ++ show ipv4 ++ ":" ++ show port ++ "/" ++ path -- TODO vot eto kaef

requestFile :: (IPv4, Word16) -> FilePath -> IO (Maybe ByteString)
requestFile addr filename = do
    resp <- Wr.get $ toUrl addr filename
    pure $
        if | resp ^. Wr.responseStatus . Wr.statusCode == 200 ->
              Just $ resp ^. Wr.responseBody
            | otherwise ->
              Nothing
