module Service.Common
       ( requestFile
       ) where

import           Data.ByteString.Lazy (ByteString)
import qualified Network.Wreq         as Wr (get, responseBody, responseStatus,
                                             statusCode)
import           Universum            hiding (ByteString)

import           DNS.Types            (IPv4)

requestFile :: IPv4 -> FilePath -> IO (Maybe ByteString)
requestFile ipv4 filename = do
    resp <- Wr.get $ "http://" ++ show ipv4 ++ "/" ++ filename -- TODO vot eto kaef
    pure $
        if | resp ^. Wr.responseStatus . Wr.statusCode == 200 ->
              Just $ resp ^. Wr.responseBody
            | otherwise ->
              Nothing
