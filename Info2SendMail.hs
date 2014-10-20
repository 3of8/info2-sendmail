{-# LANGUAGE OverloadedStrings #-}

module Main where

import Misc
import Config
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Lazy (fromStrict)
import qualified Data.Text.IO as TIO
import Data.ByteString.Lazy (toStrict)
import Network.Mail.Mime
import Network.HaskellNet.SMTP
import Network.HaskellNet.Auth
import Network.HaskellNet.SMTP.SSL
import Control.Monad
import Control.Exception
import System.IO
import System.Environment

-- User settings that may change on a weekly basis

data Config = Config
  {
    cfgMyName :: T.Text,
    cfgMyMailAddress :: T.Text,
    cfgSMTPHost :: T.Text,
    cfgSMTPUser :: T.Text,
    cfgSheet :: T.Text,
    cfgSubject :: T.Text,
    cfgGreeting :: T.Text,
    cfgIntro :: T.Text,
    cfgConclusion :: T.Text,
    cfgValediction :: T.Text,
    cfgSigName :: T.Text
  }
  
defaultConfigMap :: ConfigMap
defaultConfigMap =
  case readConfig ["Introduction: ", "Conclusion: ", "Sheet: "] of
    Left errs -> error (T.unpack $ T.intercalate "\n" errs)
    Right c -> c


-- Reading config

readNamedText :: T.Text -> [T.Text] -> (T.Text, [T.Text])
readNamedText s (t:ts)
    | not (null ls) && T.strip (head ls) == s = (T.strip $ T.unlines (tail ls), ts)
  where ls = dropWhile T.null $ T.lines t
readNamedText _ _ = ("", [])

parseConfig :: [T.Text] -> Either [T.Text] (Config, [T.Text])
parseConfig (t:ts) =
  do c' <- readConfig (T.lines t)
     let c = mergeConfigs c' defaultConfigMap
     let (intro, ts') = readNamedText "Introduction:" ts
     let (concl, ts'') = readNamedText "Conclusion:" ts'
     [myName, myMailAddress, smtpHost, smtpUser, sheet, subject, greeting, valediction, sigName] <-
         mapM (mapLeft return . lookupConfigString c) 
              ["My name", "My e-mail", "SMTP host", "SMTP user", "Sheet", "Subject", "Greeting", 
                   "Valediction", "Signature name"]
     return (Config myName myMailAddress smtpHost smtpUser sheet subject greeting intro concl 
                    valediction sigName, ts'')
parseConfig _ = Left ["No configuration given."]


-- Parsing blocks

parseBlock :: [T.Text] -> Maybe (T.Text, T.Text, T.Text, T.Text)
parseBlock (name' : mail' : rest) =
  do name <- liftM T.strip $ T.stripPrefix "Name:" name'
     i <- T.findIndex (== ' ') name
     let (lastName, firstName) = (T.take i name, T.drop (i + 1) name)
     mail <- liftM T.strip $ T.stripPrefix "E-Mail:" mail'
     let text = T.strip $ T.unlines rest
     return (firstName, lastName, mail, text)
parseBlock _ = Nothing

parseBlocks :: [T.Text] -> Either [Integer] [(T.Text, T.Text, T.Text, T.Text)]
parseBlocks bs = 
  let bs' = map (parseBlock . T.lines . T.strip) $ bs 
  in  case sequence bs' of
        Just rs -> Right rs
        Nothing -> Left $ map snd . filter (isNothing . fst) $ zip bs' [1..]


-- Formatting messages

sender :: Config -> Address
sender c = Address (Just (cfgMyName c)) (cfgMyMailAddress c)

replaceVar :: (T.Text, T.Text) -> T.Text -> T.Text
replaceVar (x,y) = T.replace ("@{" +++ x +++ "}") y

replaceVars :: [(T.Text, T.Text)] -> T.Text -> T.Text
replaceVars xs t = foldr replaceVar t xs

formatBlock :: Config -> (T.Text,T.Text,T.Text, T.Text) -> Mail
formatBlock c (firstName, lastName, mailAddress, text) =
    simpleMail' receiver (sender c) (subst cfgSubject) body
  where paragraph s = if T.null (T.strip s) then "" else s +++ "\n\n"
        body = fromStrict $ 
                 subst cfgGreeting +++ ",\n\n" +++ paragraph (subst cfgIntro) +++ 
                 subst' text +++ "\n\n" +++ paragraph (subst cfgConclusion) +++ 
                 subst cfgValediction +++ "\n" +++ subst cfgSigName
        subst' = replaceVars vars
        subst f = subst' (f c)
        receiver = Address (Just (firstName +++ " " +++ lastName)) mailAddress
        vars = [("first_name", firstName), ("last_name", lastName), ("sheet", cfgSheet c), 
                ("email", mailAddress), ("nl", "\n")]
        


-- Sending mails

formatAddress :: Address -> T.Text
formatAddress (Address name email) =
  case name of
    Nothing -> email
    Just name' -> name' +++ " <" +++ email +++ ">"

sendMails :: Config -> Password -> [Mail] -> IO ()
sendMails c smtpPassword mails =
  do con <- connectSMTPSTARTTLS (T.unpack $ cfgSMTPHost c)
     _ <- sendCommand con (EHLO (T.unpack $ cfgSMTPHost c))
     _ <- sendCommand con (AUTH LOGIN (T.unpack $ cfgSMTPUser c) smtpPassword)
     forM_ mails (\mail ->
       do TIO.putStrLn $ "Sending mail to ‘" +++ formatAddress (head (mailTo mail)) +++ "’..."
          renderedMail <- renderMail' mail
          let Address _ from = mailFrom mail
          let Address _ to = head (mailTo mail)
          sendMail (T.unpack from) [T.unpack to] (toStrict renderedMail) con
       )
     closeSMTP con


-- Main function

getPassword :: IO String
getPassword = do
  putStr "Password: "
  hFlush stdout
  pass <- withEcho False getLine
  putChar '\n'
  return pass

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

main :: IO ()
main =
  do args <- getArgs
     if null args then
       putStrLn "No file given."
     else do
       password <- getPassword
       text <- TIO.readFile (args !! 0)
       let blocks = T.splitOn "\n----\n" text
       case parseConfig blocks of
         Left errors -> mapM_ TIO.putStrLn errors
         Right (config, blocks') ->
           case parseBlocks blocks' of
             Left is -> TIO.putStrLn $ "Invalid blocks: " +++ T.intercalate ", " (map (T.pack . show) is)
             Right bs -> sendMails config password (map (formatBlock config) bs)


