{-# LANGUAGE
    RecordWildCards
  #-}

module LocalCooking.Server.Dependencies.Pagination where

import LocalCooking.Types (AppM)

import Data.HashSet (HashSet)
import Database.Persist.Class (PersistEntity (Key, EntityField))
import qualified Database.Persist.Types as Persist
import Web.Dependencies.Sparrow (Server)


data SortOrdering = Asc | Dsc



data PaginationArgs fieldLabel = PaginationArgs
  { paginationArgsField         :: fieldLabel
  , paginationArgsFieldOrdering :: SortOrdering
  , paginationArgsPageSize      :: Int
  , paginationArgsPageIndex     :: Int
  }


paginationArgsToQuery :: PaginationArgs (EntityField record typ) -> [Persist.SelectOpt record]
paginationArgsToQuery PaginationArgs{..} =
  [ case paginationArgsFieldOrdering of
      Asc -> Persist.Asc paginationArgsField
      Dsc -> Persist.Desc paginationArgsField
  , Persist.LimitTo paginationArgsPageSize
  , Persist.OffsetBy (paginationArgsPageIndex * paginationArgsPageSize)
  ]


newtype PaginationInitIn fieldLabel = PaginationInitIn (PaginationArgs fieldLabel)

newtype PaginationInitOut a = PaginationInitOut [a]

data PaginationDeltaIn fieldLabel
  = PaginationChangeSize Int
  | PaginationChangeIndex Int
  | PaginationResort fieldLabel SortOrdering

data PaginationDeltaOut a
  = PaginationUpdate a
  | PaginationFlush [a]


type ObservationScope record =
  HashSet (Key record)


paginationServer :: Server AppM
                      (PaginationInitIn (EntityField record typ))
                      (PaginationInitOut record)
                      (PaginationDeltaIn (EntityField record typ))
                      (PaginationDeltaOut record)
paginationServer (PaginationInitIn pageArgs) = do
  Env{envDatabase} <- ask
  ents <- runSqlPool envDatabase (selectList [] (paginationArgsToQuery pageArgs))
  
