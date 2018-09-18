{-
  Author: Kovacs Gyorgy
  Email: georgesmth202 at gmail dot com <- wrote it like to confuse bots

  This program was written using the Haskell language and was compiled with the
  Glasgow Haskell Compiler. I made this little program for fun, and did not care
  about proper standards at all.

  If you wish to use this code, or modify it, please do so.
-}
module Main where

import Network.Simple.TCP
import Data.ByteString.Char8 (pack, unpack)
import Control.Concurrent
import Control.Concurrent.MVar

type HTML = String
type Request = String
type Response = HTML
data State = State {
  count :: Int,
  people :: [String]
} deriving (Eq)

instance Show State where
  show state =
    "RSVPs: " ++
    addBreak (show (count state)) ++
    (addBreak =<< people state)

-- This needs to be set according to your setup.
-- Use ipconfig command in the command line to find it.
localIPAddress =  Host "192.168.0.157"
-- You will have to do port forwarding through your router or modem
-- for this to work. You will have to look that up yourself.
localPort = "80"

-- Program starts here:
main :: IO ()
main = do
  startData <- loadData "data.txt"
  dataMV <- newMVar startData
  putStrLn "Listening for connections."
  serve localIPAddress localPort $ \(socket, _) -> do
    request <- getRequest socket
    response <- updateAndProcess dataMV request
    sendResponse socket response

loadData :: String -> IO State
loadData path = do
  content <- readFile path
  return State {
    count = length . lines $ content,
    people = lines content
  }

getRequest :: Socket -> IO String
getRequest socket = maybe "" unpack <$> recv socket 1000

updateAndProcess :: MVar State -> Request -> IO Response
updateAndProcess global request = do
  state <- takeMVar global
  (response, newState) <- process request state
  putMVar global newState
  return response

sendResponse :: Socket -> HTML -> IO ()
sendResponse socket response = send socket $ pack response

process :: Request -> State -> IO (Response, State)
process request state  =
  let newState = updateState state name
  in if not . null $ name
     then do
       putStrLn $ "RSVP from " ++ name
       if newState /= state
       then appendFile "data.txt" $ name ++ "\n"
       else putStrLn "Name already taken."
       (\a -> (a, newState)) <$> response newState
     else do
       putStrLn "Ignoring Request"
       (\a -> (a, state)) <$> response state
  where
    updateState :: State -> String -> State
    updateState state name
      | elem name $ people state = state
      | otherwise = state {
        count = count state + 1,
        people = people state ++ [name]
      }

    name = replace '+' ' '
           . filter (`elem` allowedCharacters)
           . takeWhile (/= ' ')
           . dropWhile (/= '=')
           . dropWhile (/= '?')
           . safeHead ""
           . lines
           $ request

response :: State -> IO HTML
response count = do
  description <- flatMap addBreak . lines <$> readFile "description.txt"
  greeting <- flatMap addBreak . lines <$> readFile "greeting.txt"
  sourceCode <- flatMap addBreak . lines <$> readFile "Server.hs"
  return $
    tag "!DOCTYPE html"
    |> html (
      hed (
        title "Free Software Meetup"
      )
      |> body (
        h 2 "Greeting"
        |> addBreak greeting
        |> (h 1 $ show count)
        |> form (
          addBreak "Enter any name you want (there is no remove feature as of yet):"
          |> input "text" "Nickname" ""
          |> input "submit" "" "Submit"
        )
        |> hline
        |> h 2 "Description"
        |> description
        |> hline
        |> h 2 "Source Code"
        |> code sourceCode
      )
    )

-- HTML functions

input :: String -> String -> String -> HTML
input inputType inputName valueName = tag $
  "input type=" ++ quoted inputType ++
  " name=" ++ quoted inputName ++
  " value=" ++ quoted valueName

html  = addTags "html"
title = addTags "title"
hed = addTags "head" -- head is an existing function :(
body  = addTags "body"
form  = addTags "form"
h i = addTags $ "h" ++ show i
code = addTags "pre" . addTags "code"
hline = tag "hr"
tag s = "<" ++ s ++ ">"
addTags tagName s = mconcat [tag tagName, "\n", s, "\n", tag ('/':tagName)]
addBreak = (++ "<br>")
allowedCharacters = ['a'..'z'] ++ ['A'..'Z'] ++ "!~-,_ +"

-- Helper Functions and renames

replace :: Char -> Char -> String -> String
replace rc wc xs = map (\c -> if c == rc then wc else c) xs

safeTail :: [a] -> [a]
safeTail as
  | null as = []
  | otherwise = tail as

safeHead :: a -> [a] -> a
safeHead a as
  | null as = a
  | otherwise = head as

flatMap = (=<<)
a |> b = a ++ "\n" ++ b
quoted s = "\"" ++ s ++ "\""
