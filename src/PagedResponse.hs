{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module PagedResponse (Paged, nextPage) where
import Data.Text (Text, unpack)
import Data.ByteString.UTF8 (fromString)
import Servant.API
import Network.HTTP.Link
import Network.HTTP.Types.URI

type Paged a = Headers '[Header "Link" Text] a

extractNextURI :: [Link] -> Maybe URI
extractNextURI [] = Nothing
extractNextURI (link:rest) =
  case link of
    Link uri params ->
      if hasNext params then
        Just uri
      else extractNextURI rest
      where hasNext = foldr ((||) . isNext) False
            isNext (Rel, "next") = True
            isNext _ = False

extractPageParam :: URI -> Maybe Text
extractPageParam uri =
  do
    item <- extractPageQueryItem queryText
    case item of (_, value) -> value
  where queryText = parseQueryText $ fromString $ uriQuery uri
        extractPageQueryItem [] = Nothing
        extractPageQueryItem (x:xs) =
          if isPageItem x then Just x else extractPageQueryItem xs
        isPageItem item =
          case item of
            ("page", _) -> True
            _ -> False

nextPage :: Paged a -> Maybe String
nextPage pagedResult = let hdrs = getHeadersHList pagedResult in
  case hdrs of
    HCons (Header val) _ ->
        do
          links <- parseLinkHeader val
          uri <- extractNextURI links
          pageText <- extractPageParam uri
          return $ unpack pageText
    _ -> Nothing
