{-# LANGUAGE OverloadedStrings #-}

module Config (ConfigMap, emptyConfig, insertConfigEntry, mergeConfigs, readConfig, lookupConfigString, 
               lookupConfigString', lookupConfigInt, hasConfig) where

import Misc
import Data.Either
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Text.Read (readEither)

newtype ConfigMap = ConfigMap (M.Map T.Text T.Text)

emptyConfig :: ConfigMap
emptyConfig = ConfigMap M.empty

mergeConfigs :: ConfigMap -> ConfigMap -> ConfigMap
mergeConfigs (ConfigMap m1) (ConfigMap m2) = ConfigMap $ M.union m1 m2

readEntry :: T.Text -> Either T.Text (T.Text, T.Text)
readEntry s =
  case T.findIndex (== ':') s of
    Nothing -> Left $ "Invalid entry: " +++ s
    Just i -> Right (T.toLower . T.strip $ T.take i s, T.strip $ T.drop (i + 1) s)

readConfig :: [T.Text] -> Either [T.Text] ConfigMap
readConfig ts =
  case lefts entries of
    [] -> Right . ConfigMap . M.fromList . rights $ entries
    ls -> Left ls
  where entries = map readEntry . filter (not . T.null) . map T.strip $ ts
  
insertConfigEntry :: ConfigMap -> T.Text -> T.Text -> ConfigMap
insertConfigEntry (ConfigMap c) k v = ConfigMap $ M.insert k v c
  
lookupConfigString :: ConfigMap -> T.Text -> Either T.Text T.Text
lookupConfigString (ConfigMap m) k = 
  case M.lookup (T.toLower k) m of
    Just v -> Right v
    Nothing -> Left $ "Missing config entry: " +++ k
    
lookupConfigString' :: ConfigMap -> T.Text -> T.Text
lookupConfigString' (ConfigMap m) k = M.findWithDefault "" (T.toLower k) m

lookupConfigInt :: ConfigMap -> T.Text -> Either T.Text Integer
lookupConfigInt c k = 
  do v <- lookupConfigString c k
     changeLeft ("Not an integer value: " +++ v) (readEither (T.unpack v))

hasConfig :: ConfigMap -> T.Text -> Bool
hasConfig (ConfigMap m) k = M.member (T.toLower k) m

