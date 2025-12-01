module ZkFold.Prover.API.Handler.Delegation where

import Control.Lens ((^.))
import Crypto.Number.Generate
import Crypto.Random (MonadRandom)
import Data.ByteString hiding (foldr, length, (!?))
import Network.HTTP.Types (Header)
import Network.HTTP.Types.Header (hContentType)
import Network.Wreq hiding (options)
import Network.Wreq.Types (Options (..))
import ZkFold.Prover.API.Utils ((!?))

-- | Returns which prover to delegate the proof to. If Nothing, no delegation is needed.
delegationStrategy :: forall i m. (MonadRandom m) => [String] -> i -> m (Maybe String)
delegationStrategy urls _ = do
    i <- generateBetween 0 (fromIntegral $ length urls)
    pure $ urls !? fromIntegral i

customHeaders :: [Header]
customHeaders =
    [ (hContentType, "application/json")
    ]

{- | Sends a POST request to the specified URL with the body and headers.
Returns the response body content or an error.
-}
sendPostRequest ::
    String ->
    ByteString ->
    [Header] ->
    IO ByteString
sendPostRequest serverUrl body hs = do
    response <- postWith options serverUrl body
    pure $ toStrict $ response ^. responseBody
  where
    options = defaults{headers = hs}
