-- |
-- Module      : Network.Mail.Account
-- Copyright   : (c) Justus Sagemüller 2017
-- License     : GPL v3
-- 
-- Maintainer  : (@) sagemueller $ geo.uni-koeln.de
-- Stability   : experimental
-- Portability : requires GHC>6 extensions

{-# LANGUAGE DeriveGeneric     #-}

module Network.Mail.Account where

import Network.Mail.Mime
import Network.HaskellNet.SMTP.SSL as SMTP

import Data.Aeson
import qualified Data.Text as Txt
import Data.Text (Text)

import GHC.Generics

import Data.Monoid

sendsMail :: MailAccount -> (Mail -> Mail) -> IO ()
sendsMail (MailAccount name address Nothing) f
       = renderSendMail . f . emptyMail $ Address name address
sendsMail (MailAccount name address (Just (MailingHost host SSL _ _ port', passwd))) f
         = doSMTPSSL ("smtp."<>Txt.unpack host) $ \c -> do
             True <- SMTP.authenticate LOGIN (Txt.unpack address) (Txt.unpack passwd) c
             sendMimeMail2 (f . emptyMail $ Address name address) c
 where port = maybe 465 id port'

type Password = Text

data MailAccount = MailAccount {
        userName :: Maybe Text
      , userMail :: Text
      , mailingHost :: Maybe (MailingHost, Password)
      } deriving (Generic)
instance ToJSON MailAccount
instance FromJSON MailAccount

type Port = Int
        
data MailingHost = MailingHost {
        hostname :: Text
      , hostSecurity :: ConnectionSecurity
      , pop3PortNumber, imapPortNumber, smtpPortNumber :: Maybe Port
      } deriving (Generic)
instance ToJSON MailingHost
instance FromJSON MailingHost

data ConnectionSecurity
       = Unsecured
       | SSL
       | STARTTLS
     deriving (Generic)
instance ToJSON ConnectionSecurity
instance FromJSON ConnectionSecurity
