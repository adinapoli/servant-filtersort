{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Data.Text
import Data.Time (Day, fromGregorian, parseTimeM, defaultTimeLocale)
import Servant.API
import Data.Aeson
import Data.Aeson.TH
import Data.Proxy
import Network.Wai
import Network.Wai.Handler.Warp

import Servant

import Servant.API.FilterSort.Indices
import Servant.API.FilterSort.Request.Filter
import Servant.API.FilterSort.Request.Sort

-- Let's pick a backend to serve our filter & sort machinery, in this
-- case, ixset-typed.
import Servant.API.FilterSort.Indices.IxSet
import qualified Data.IxSet.Typed as IxSetTyped
import qualified Servant.API.FilterSort.Indices.IxSet as IxSet
import Servant.API.FilterSort.Response.Filter.IxSet
import Servant.API.FilterSort.Response.Sort.IxSet

newtype Name = Name { getName :: String }
    deriving (Ord, Eq, ToHttpApiData, FromHttpApiData)

deriveToJSON defaultOptions { unwrapUnaryRecords = True } ''Name

newtype Age  = Age  { getAge :: Int } 
    deriving (Ord, Eq, ToHttpApiData, FromHttpApiData)

deriveToJSON defaultOptions { unwrapUnaryRecords = True } ''Age

data User = User 
    { uid   :: Int
    , name  :: Name
    , age   :: Age
    , email :: String
    , registration_date :: Day
    }

deriveToJSON defaultOptions ''User

-- library-specific boilerplate

type instance IndexToQueryParam User Name = "name"
type instance IndexToQueryParam User Age  = "age"

-- The following below are OK as in this example there is no ambiguity.
type instance IndexToQueryParam User Day  = "registration_date"
type instance IndexToQueryParam User Int  = "id"

instance ToIndex User Int where
    -- Error handling disregarded in this simple example.
    toIndex _ = Just . read . unpack
    accessIx User{..} = uid

instance ToIndex User Age where
    -- Error handling disregarded in this simple example.
    toIndex _ = Just . Age . read . unpack
    accessIx User{..} = age

instance ToIndex User Name where
    toIndex _ = Just . Name . unpack
    accessIx User{..} = name

instance ToIndex User Day where
    -- Error handling disregarded in this simple example.
    toIndex _ x = parseTimeM True defaultTimeLocale "%F" (unpack x)
    accessIx User{..} = registration_date

-- backend-specific boilerplate, these typeclasses comes from the IxSet
-- backend.

instance HasPrimKey User where
  type PrimKey User = Int
  primKey = uid

-- | The secondary indices for this resource.
type SecondaryUserIxs = '[Name, Age, Day]
type instance IndicesOf User = SecondaryUserIxs

instance IxSetTyped.Indexable (Int ': SecondaryUserIxs)
                         (OrdByPrimKey User) where
    indices = ixList (ixFun ((:[]) . name))
                     (ixFun ((:[]) . age))
                     (ixFun ((:[]) . registration_date))


type UserAPI
  = "users" :> SortBy '[Day] User
            :> FilterBy '[Int, Name, Age] User
            :> Get '[JSON] [User]


users :: IxSet User
users = IxSet.fromList $
  [ User 1 (Name "Isaac Newton")    (Age 372) "isaac@newton.co.uk" (fromGregorian 1683  3 1)
  , User 2 (Name "Albert Einstein") (Age 136) "ae@mc2.org"         (fromGregorian 1905 12 1)
  ]

usersHandler :: SortOperations User 
             -> FilterOperations '[Int,Name, Age] User 
             -> Handler [User]
usersHandler sops fops = 
    return $ sortData sops $ applyFilters fops users

server :: Server UserAPI
server = usersHandler

userAPI :: Proxy UserAPI
userAPI = Proxy

main :: IO ()
main = do
    print "Starting the server..."
    run 8080 $ serve userAPI server
