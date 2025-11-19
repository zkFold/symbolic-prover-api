module ZkFold.Prover.API.Robots (
  RobotsAPI,
  handleRobots,
) where

import Data.ByteString.Char8 qualified as BS
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType)
import Network.Wai (Application, responseFile)
import Servant (Handler, Raw, ServerT, (:>))
import Servant.Server (Tagged (..))

-- | API for serving the static robots.txt file
type RobotsAPI = "robots.txt" :> Raw

-- | Serve the compiled WAI application that returns robots.txt without touching FS per request
handleRobots :: ServerT RobotsAPI Handler
handleRobots = Tagged robotsApp
 where
  robotsApp :: Application
  robotsApp _ respond =
    respond $ responseFile status200 [(hContentType, BS.pack "text/plain; charset=utf-8")] "web/robots.txt" Nothing
