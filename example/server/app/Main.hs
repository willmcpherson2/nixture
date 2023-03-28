module Main (main) where

import Control.Applicative (Alternative ((<|>)))
import Data.Text (unpack)
import Network.HTTP.Types
  ( hContentType,
    status200,
    status404,
  )
import Network.Wai
  ( Application,
    Request (pathInfo, requestMethod),
    Response,
    responseFile,
  )
import Network.Wai.Handler.Warp (run)
import System.Environment (getArgs, getEnv)

main :: IO ()
main =
  getArgs >>= \case
    [dir] -> do
      port <- read <$> getEnv "PORT" <|> pure 8000
      putStrLn dir
      putStrLn $ "http://localhost:" <> show port
      run port (app dir)
    _ -> putStrLn "no directory provided"

app :: String -> Application
app dir request respond = do
  let method = requestMethod request
      path = map unpack (pathInfo request)
  print (method, path)
  route dir path >>= respond

route :: String -> [String] -> IO Response
route dir = \case
  [] ->
    pure $
      responseFile
        status200
        [(hContentType, "text/html")]
        (dir <> "/" <> "index.html")
        Nothing
  ["hs-script.js"] ->
    pure $
      responseFile
        status200
        [(hContentType, "text/javascript")]
        (dir <> "/" <> "hs-script.js")
        Nothing
  _ ->
    pure $
      responseFile
        status404
        [(hContentType, "text/html")]
        (dir <> "/" <> "not-found.html")
        Nothing
