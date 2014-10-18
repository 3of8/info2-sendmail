{-# LANGUAGE OverloadedStrings #-}

module Info2SendMail where

import Data.Either
import qualified Data.Text as T
import Data.Text.Lazy (fromStrict)
import qualified Data.Text.IO as TIO
import Data.ByteString.Lazy (toStrict)
import Text.Printf
import Network.Mail.Mime
import Network.HaskellNet.SMTP
import Network.HaskellNet.Auth
import Network.HaskellNet.SMTP.SSL
import Control.Monad
import Control.Exception
import System.IO
import System.Environment

-- User settings that may change on a weekly basis

sheetNumber    = 1
introduction   = "hier ist das Feedback für deine Hausaufgaben zu Übungsblatt " +++ T.pack (show sheetNumber) +++ ". " +++
                 "Die genaue Bepunktung wird automatisch auf Basis unserer QuickCheck-Tests erstellt. " +++
                 "Du kannst sie online im Übungssystem einsehen."
conclusion     = "Falls etwas an der Bewertung unklar ist, kannst du gerne per E-Mail nachfragen."


-- User settings that should remain mostly constant

myName         = "Manuel Eberl"
myMailAddress  = "eberlm@in.tum.de"
valediction    = "Viele Grüße"
signatureName  = "Manuel"
subjectPattern = "[Info2] Feedback zu Übungsblatt %d"


-- SMTP settings

smtpHost       = "mail.in.tum.de"
smtpUser       = "eberlm"
smtpAuthType   = LOGIN


-- Constants

usageDescription = "No file given."

-- Parsing blocks

parseBlock (name : mail : rest) =
  do name <- liftM T.strip $ T.stripPrefix "Name:" name
     i <- T.findIndex (== ' ') name
     let (lastName, firstName) = (T.take i name, T.drop (i + 1) name)
     mail <- liftM T.strip $ T.stripPrefix "E-Mail:" mail
     let text = T.strip . T.unlines $ rest
     return (firstName, lastName, mail, text)
parseBlock b = Nothing

parseBlocks :: [T.Text] -> Either [Integer] [(T.Text, T.Text, T.Text, T.Text)]
parseBlocks bs = 
  let bs' = map (parseBlock . T.lines . T.strip) $ bs 
  in  case sequence bs' of
        Just rs -> Right rs
        Nothing -> Left $ map snd . filter (maybe True (const False) . fst) $ zip bs' [1..]


-- Formatting messages

(+++) = T.append

subject = T.pack $ printf subjectPattern (sheetNumber :: Integer)
sender = Address (Just myName) myMailAddress

formatBlock :: (T.Text,T.Text,T.Text, T.Text) -> Mail
formatBlock (firstName, lastName, mailAddress, text) =
    simpleMail' receiver sender subject body
  where paragraph s = if T.null s then "" else s +++ "\n\n"
        body = fromStrict $ 
                 "Hallo " +++ firstName +++ " " +++ lastName +++ ",\n\n" +++ paragraph introduction +++ 
                 text +++ "\n\n" +++ paragraph conclusion +++ valediction +++ "\n" +++ myName
        receiver = Address (Just (firstName +++ " " +++ lastName)) mailAddress     


-- Sending mails

formatAddress (Address name email) =
  case name of
    Nothing -> email
    Just name' -> name' +++ " <" +++ email +++ ">"

sendMails smtpPassword mails =
  do con <- connectSMTPSTARTTLS smtpHost
     sendCommand con (EHLO smtpHost)
     sendCommand con (AUTH smtpAuthType smtpUser smtpPassword)
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

main =
  do args <- getArgs
     if null args then
       putStrLn usageDescription
     else do
       password <- getPassword
       text <- TIO.readFile (args !! 0)
       case parseBlocks $ T.splitOn "\n----\n" text of
         Left is -> TIO.putStrLn $ "Invalid blocks: " +++ T.intercalate ", " (map (T.pack . show) is)
         Right bs -> sendMails password (map formatBlock bs)


