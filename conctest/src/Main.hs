module Main where


import qualified Data.List as L
import Control.Concurrent
import Control.Concurrent.Chan as Chan

import Control.Monad

import System.Environment ( getArgs
                          , getProgName)
import System.Exit
import System.IO
import System.Random



-- printChan :: (Show a) => Chan a -> IO ()
printChan :: Chan String -> IO ()
printChan c = do
  str <- Chan.readChan c
  putStrLn str


sendMsg :: Chan String -> String -> Int -> IO ()
sendMsg ch str d = do
  threadDelay $ d * 1000
  Chan.writeChan ch str


printer :: Chan String -> IO ()
printer = getChanContents >=> mapM_ putStrLn


mkChat :: Chan String
       -> String
       -> (Chan String -> String -> IO ())
       -> IO ()
mkChat ch name f = do
  ch' <- dupChan ch
  f ch' name


chat :: Chan String -> String -> IO ()
chat ch name = do
  msg <- getLine
  Chan.writeChan ch $ name ++ ":\t" ++ msg
  chat ch name



messages :: [String]
messages = ["asshole"
           ,"butt"
           ,"idiot"
           ,"hellfucker"
           ,"fuck"
           ]


genMessage :: [String] -> Int -> IO String
genMessage strs len = do
  let nStrs = length strs
  is <- replicateM len $ randomRIO (0, nStrs-1)
  return $ unwords $ map (strs !!) is


chatBot :: Chan String -> String -> IO ()
chatBot ch n = do
  delay <- randomRIO (100,300)
  len <- randomRIO (1,4)
  threadDelay $ delay * 10^4
  msg <- genMessage messages len
  writeChan ch $ n ++ ":\t" ++ msg
  chatBot ch n

genName :: Int -> IO String
genName len = do
  i <- randomRIO (0,len `div` 2)
  fh <- replicateM (len - i) $ randomRIO ('A', 'z')
  sh <- fmap concat $ replicateM i $ show <$> (randomRIO (0,9) :: IO Int)
  return $ fh ++ sh


mkChatBot :: Chan String -> Int -> IO ThreadId
mkChatBot ch d = do
  threadDelay d
  len <- randomRIO (4,8)
  n <- genName len
  writeChan ch $ n ++ " has connected"
  tid <- forkIO $ chatBot ch n
  life <- randomRIO (30, 300)
  _ <- forkIO $ killer tid (life * 10^6) n
  return tid

birther c = forever $ do
  d <- randomRIO (30, 90)
  threadDelay $ 10^6 * d
  _ <- mkChatBot c 0
  return ()


killer :: ThreadId -> Int -> String -> IO ()
killer tid d s = do
  threadDelay $ d * 10^6
  putStrLn s
  killThread tid

exitW :: String -> IO String
exitW str = do
  putStrLn str
  exitFailure

main :: IO ()
main = do
  progName <- getProgName
  args <- getArgs
  name <- case args of
    [n] -> return n
    _ -> putStrLn ("usage: " ++ progName ++ " username") >> exitFailure

  hSetBuffering stdout NoBuffering
  putStrLn $ "joined chatroom as " ++ name

  c <- Chan.newChan
  _ <- forkIO $ printer c

  nBots <- randomRIO (3,8)

  _ <- replicateM_ nBots $ do
    d <- randomRIO (50,250)
    mkChatBot c (d * 10^4)

  _ <- forkIO $ birther c

  mkChat c name chat
