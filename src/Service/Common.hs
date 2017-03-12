module Service.Common
       ( requestFile
       , toUrl
       , requestTask
       , hasFile
       ) where

import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Encoding   as T
import qualified Network.Wreq         as Wr (get, responseBody, responseStatus,
                                             statusCode)
import           System.FilePath      ((</>))
import           Universum            hiding (ByteString)

import           DNS.Types            (IPv4)

toUrl :: (IPv4, Word16) -> FilePath -> String
toUrl (ipv4, port) path =
    "http://" ++ show ipv4 ++ ":" ++ show port </> path -- TODO vot eto kaef

requestFile :: (IPv4, Word16) -> FilePath -> IO (Maybe ByteString)
requestFile addr filename = do
    resp <- Wr.get $ toUrl addr ("download" </> filename)
    pure $
        if | resp ^. Wr.responseStatus . Wr.statusCode == 200 ->
              Just $ resp ^. Wr.responseBody
            | otherwise -> Nothing

hasFile :: (IPv4, Word16) -> FilePath -> IO Bool
hasFile addr filename = do
    resp <- Wr.get $ toUrl addr ("file" </> filename)
    pure $ resp ^. Wr.responseStatus . Wr.statusCode == 200

requestTask :: (IPv4, Word16) -> FilePath -> Int -> IO (Maybe Text)
requestTask addr task arg = do
    resp <- Wr.get $ toUrl addr (task </> show arg)
    pure $
        if | resp ^. Wr.responseStatus . Wr.statusCode == 200 ->
              Just $ T.decodeUtf8 $ BSL.toStrict $ resp ^. Wr.responseBody
           | otherwise -> Nothing
