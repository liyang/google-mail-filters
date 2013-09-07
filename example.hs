{-# LANGUAGE OverloadedStrings #-}

import Prelude
import Control.Applicative
import qualified Data.Text.Lazy.IO as TL
import Data.Time.Clock
import Text.XML as XML

import Language.Google.Search.Simple as Search
import Language.Google.Search.Mail as Search
import Data.Google.Mail.Filters as Filters

main :: IO ()
main = do
    now <- getCurrentTime
    TL.putStrLn . XML.renderText def {rsPretty = True} $
        Filters.toXML now ("Your Name", "you@example.com") filters

filters :: [Filter]
filters =

    [ Filter [Archive, LabelAs "shopping"] $ orB
        [ pure (From $ ("auto-confirm" \/ "noreply") /\ "amazon")
        , pure (From "no-reply@kickstarter.com")
        , pure (From $ "service" /\ "paypal")
        ]

    , Filter [Archive, LabelAs "haskell"]
        $  pure (List "haskell-cafe.haskell.org")
        /\ notB haskellWeeklyNews -- keep in inbox
    , Filter [LabelAs "haskell"] haskellWeeklyNews

    , Filter [LabelAs "notification", MarkAsImportant False, NeverSpam] $ orB
        [ pure $ From "plus.google.com"
        , pure $ From "facebookmail.com"
        , pure $ From "postmaster.twitter.com"
        , pure $ From "noreply@foursquare.com"
        ]

    ] where
    haskellWeeklyNews = pure . Subject $ Exact <$> "Haskell Weekly News"

