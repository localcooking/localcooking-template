module LocalCooking.Server.Dependencies.Pagination where


data SortOrdering = Asc | Dsc



data PaginationArgs fieldLabel = PaginationArgs
  { paginationArgsField         :: fieldLabel
  , paginationArgsFieldOrdering :: SortOrdering
  , paginationArgsPageSize      :: Int
  , paginationArgsPageIndex     :: Int
  }


newtype PaginationInitIn fieldLabel = PaginationInitIn (PaginationArgs fieldLabel)

newtype PaginationInitOut a = PaginationInitOut [a]

data PaginationDeltaIn fieldLabel
  = PaginationChangeSize Int
  | PaginationChangeIndex Int
  | PaginationResort fieldLabel SortOrdering

data PaginationDeltaOut a
  = PaginationUpdate a
  | PaginationFlush [a]
