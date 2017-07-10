{-# LANGUAGE AutoDeriveTypeable    #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

import           Servant.Subscriber.Request
import           Servant.Subscriber.Response
import           Servant.Subscriber.Types

import           Control.Applicative
import           Data.Monoid
import           Data.Proxy
import           Language.PureScript.Bridge
import           Language.PureScript.Bridge.PSTypes
import           Servant.API
import           Servant.PureScript
import           Servant.Subscriber.Subscribable

myTypes = [
       mkSumType (Proxy :: Proxy Request)
     , mkSumType (Proxy :: Proxy HttpRequest)
     , mkSumType (Proxy :: Proxy Response)
     , mkSumType (Proxy :: Proxy HttpResponse)
     , mkSumType (Proxy :: Proxy Status)
     , mkSumType (Proxy :: Proxy Path)
     ]

bridgeResponseBody :: BridgePart
bridgeResponseBody = do
  typeName ^== "ResponseBody"
  t <- view haskType
  TypeInfo (_typePackage t) "Servant.Subscriber.Response" (_typeName t) <$> psTypeParameters

bridgeRequestBody :: BridgePart
bridgeRequestBody = do
  typeName ^== "RequestBody"
  t <- view haskType
  TypeInfo (_typePackage t) "Servant.Subscriber.Request" (_typeName t) <$> psTypeParameters

myBridge :: BridgePart
myBridge = bridgeResponseBody <|> bridgeRequestBody <|> defaultBridge

main :: IO ()
main = writePSTypes "../purescript-subscriber/src" (buildBridge myBridge PSv011) myTypes
