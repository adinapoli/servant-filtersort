{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Servant.API.FilterSort.Indices where

import Data.Proxy
import Data.Text (Text)
import GHC.TypeLits

-- | 'ToIndex' represents the witness that we can build an index 'ix' for a resource 'a'
-- from an input 'Text'.
-- An example instance could look like this:
--
-- instance ToIndex Inventory InventoryId where
--     toIndex _ x = Just (InventoryId x)
--     accessIx Inventory{..} = inventoryId
--
class ToIndex a ix where

  -- | How to build this index from the input 'Text'.
  toIndex :: Proxy a -> Text -> Maybe ix

  -- | How to access this index from the input data.
  accessIx :: (a -> ix)

--
-- Primary and secondary indices
--
--instance HasPrimKey Wallet where
--
--  type PrimKey Wallet = WalletId
--
--  primKey = walId

-- | The secondary indices for each major resource.
--type SecondaryWalletIxs = '[Core.Coin, WalletTimestamp]

--type instance IndicesOf Wallet = SecondaryWalletIxs

-- | Extract the parameter names from a type leve list with the shape
type family ParamNames res xs where
  ParamNames res '[] = '[]
  ParamNames res (ty ': xs) =
    IndexToQueryParam res ty ': ParamNames res xs

-- | This type family allows you to recover the query parameter if you know
-- the resource and index into that resource.
type family IndexToQueryParam resource ix :: Symbol

--  IndexToQueryParam Wallet Core.Coin = "balance"
--  IndexToQueryParam Wallet WalletId = "id"
--  IndexToQueryParam Wallet WalletTimestamp = "created_at"

-- | Type-level composition of 'KnownSymbol' and 'IndexToQueryParam'
--
-- Example:
--
-- instance KnownQueryParam Inventory InventoryIndex
class
  KnownSymbol (IndexToQueryParam resource ix)
  => KnownQueryParam resource ix
