module ZkFold.Prover.API.Handler.Delegation where

import Control.Exception (SomeException, try)
import Data.ByteString hiding ((!?))
import Data.List ((!?))
import Network.HTTP.Client
import Network.HTTP.Types

-- | Returns which prover to delegate the proof to. If Nothing , no delegation is needed.
delegationStrategy :: forall i. [String] -> i -> Maybe String
delegationStrategy urls _ = urls !? 0

customHeaders :: [(HeaderName, ByteString)]
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
    IO (Either String ByteString)
sendPostRequest serverUrl body headers = do
    manager <- newManager defaultManagerSettings

    initialRequestResult <- try (parseRequest serverUrl) :: IO (Either SomeException Request)

    case initialRequestResult of
        Left err -> return $ Left $ "Parsing URL error: " ++ show err
        Right req -> do
            let modifiedRequest =
                    req
                        { method = methodPost
                        , requestBody = RequestBodyLBS (fromStrict body)
                        , requestHeaders = headers
                        }

            responseResult <- fmap (toStrict . responseBody) <$> try (httpLbs modifiedRequest manager) :: IO (Either SomeException ByteString)

            case responseResult of
                Left err -> return $ Left $ "Error executing request: " ++ show err
                Right response -> do
                    return $ Right response
