{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Google.Mail.Filters where

import Prelude
#if MIN_VERSION_xml_conduit(1,0,0)
import qualified Data.Map as Map
#endif
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as B
import Data.Time.Clock
import Data.Time.Format
#if !MIN_VERSION_time(1,5,0)
import System.Locale (defaultTimeLocale)
#endif
import Text.XML

import Language.Google.Search.Simple as Search
import Language.Google.Search.Mail as Search

data Action
    = Archive
    | Categorise Category
    | Delete
    | ForwardTo Text
    | LabelAs Text
    | MarkAsImportant Bool
    | MarkAsRead
    | NeverSpam
    | Star
    deriving (Show)

data Filter = Filter
    { actions :: [Action]
    , hasTheWord :: Search.Mail -- ^ subsumes other search operators
    } deriving (Show)

-- | (Name, Email) of author.
type Author = (Text, Text)

toXML :: UTCTime -> Author -> [Filter] -> Document
toXML now (author, account) filters = Document prologue root [] where
    prologue = Prologue [] Nothing []
    root = Element "feed" namespaces (preamble ++ zipWith entry [0 ..] filters)
    namespaces = toAttrs
        [ ("xmlns", "http://www.w3.org/2005/Atom")
        , ("xmlns:apps", "http://schemas.google.com/apps/2006") ]
    updated = noel "updated" [] [NodeContent $ utcText now]
    preamble =
        [ noel "title" [] [NodeContent "Mail Filters"]
        , noel "id" [] [ NodeContent . T.append tagFilters . T.intercalate "," $
            zipWith (const . showIdent) [0 ..] filters ]
        , updated
        , noel "author" []
            [ noel "name"  [] [NodeContent author]
            , noel "email" [] [NodeContent account]
            ]
        ]

    tagFilter, tagFilters :: Text
    tagFilter  = "tag:mail.google.com,2008:filter:"
    tagFilters = "tag:mail.google.com,2008:filters:"

    noel :: Name -> [(Name, Text)] -> [Node] -> Node
    noel name attrs nodes = NodeElement $ Element name (toAttrs attrs) nodes

    utcText :: UTCTime -> Text
    utcText = T.pack . formatTime defaultTimeLocale "%FT%TZ"

    showIdent :: Int -> Text
    showIdent = T.pack . show

    entry :: Int -> Filter -> Node
    entry ident Filter {..} = noel "entry" [] $
        [ noel "category" [("term", "filter")] []
        , noel "title" [] [NodeContent "Mail Filter"]
        , noel "id" [] [NodeContent $ tagFilter <> showIdent ident]
        , updated
        , noel "content" [] []
        , prop "hasTheWord" (search hasTheWord)
        ] ++ map pact actions where

        prop name value = noel "apps:property"
            [("name", name), ("value", value)] []
        search s = TL.toStrict (B.toLazyText b) where
            PrecBuilder _prec b = searchBuilder s
        pact act = case act of
            Archive             -> prop "shouldArchive" "true"
            Categorise cat      -> prop "smartLabelToApply" $ case cat of
                Forums      -> "^smartlabel_group"
                Personal    -> "^smartlabel_personal"
                Promotions  -> "^smartlabel_promo"
                Social      -> "^smartlabel_social"
                Updates     -> "^smartlabel_notification"
            Delete              -> prop "shouldTrash" "true"
            ForwardTo t         -> prop "forwardTo" t
            LabelAs t           -> prop "label" t
            MarkAsImportant yes -> case yes of
                True -> prop "shouldAlwaysMarkAsImportant" "true"
                False -> prop "shouldNeverMarkAsImportant" "true"
            MarkAsRead          -> prop "shouldMarkAsRead" "true"
            NeverSpam           -> prop "shouldNeverSpam" "true"
            Star                -> prop "shouldStar" "true"

#if MIN_VERSION_xml_conduit(1,0,0)
    toAttrs = Map.fromList
#else
    toAttrs = id
#endif

