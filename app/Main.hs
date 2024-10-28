{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logInfoN, runStdoutLoggingT)
import Data.Text qualified as T (pack, unpack)
import Data.Text.Lazy qualified as TL
import Flow
import Greet
import Network.Wai (Middleware, Request (pathInfo, requestMethod))
import Text.Printf (printf)
import Todo
import Web.Scotty (formParam, get, middleware, pathParam, post, scotty, text)

logReqs :: Middleware
logReqs app req sendResp = do
  putStrLn
    <| printf
      "Request: %s %s"
      (req |> pathInfo |> show)
      (req |> requestMethod |> show)
  putStrLn <| T.unpack <| pprintTodos exampleTodos
  app req sendResp

type Port = Int

port :: Port
port = 3003

main :: IO ()
main =
  runStdoutLoggingT <| do
    logInfoN
      <| T.pack
      <| printf "Starting the server on port %d..." port

    liftIO
      <| scotty port
      <| do
        middleware logReqs

        get "/" <| text "Welcome to Haskell Backend"

        get "/hello/:name" <| do
          name <- pathParam "name"
          text <| greet name

        post "/echo" <| do
          msg <- formParam "msg"
          text msg

        get "/todos"
          <| text
          <| TL.fromStrict
          <| pprintTodos exampleTodos
