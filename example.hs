{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

import Prelude
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.IO as TL
import Data.Time.Clock
import System.IO
import Text.XML as XML

import Language.Google.Search.Simple as Search
import Language.Google.Search.Mail as Search
import Data.Google.Mail.Filters as Filters

main :: IO ()
main = do
    now <- getCurrentTime
    TL.putStrLn . XML.renderText def {rsPretty = True} $
        Filters.toXML now ("Your Name", "you@example.com") filters

    -- Search Terms for GMail Labs' ‘Quick Links’ by Dan P
    TL.hPutStrLn stderr . TL.append "old:\t" . searchText $ orB
        [ pure (Label "notification") /\ pure (Older 1 Months)
        , pure (Label "dev") /\ pure (Older 6 Months)
        :: Search.Mail ]

searchText :: (SearchBuilder e) => e -> TL.Text
searchText s = B.toLazyText b where PrecBuilder _prec b = searchBuilder s

filters :: [Filter]
filters =
    [ Filter [Archive, LabelAs "shopping"]
        $ (pure . From . orB)
            [ "googleplay-noreply@google.com"
            , "noreply@indiegogo.com" ]
        \/ ( pure . From $ "amazon" /\ orB
            ["confirm", "digital", "marketplace", "no-reply", "update"] )
        \/ andB [ pure $ From "no-reply@kickstarter.com"
            , notB . pure $ Subject "reminder" ]

    , Filter [Archive, LabelAs "dev"] $ orB
        [ pure (From "ghc-devs@haskell.org")
            /\ pure (Cc "ghc-tickets@haskell.org")
        , pure . Subject $ Exact <$> "Haskell Weekly News"
        , (pure . From . orB)
            [ "notifications@travis-ci.org"
            , "notifications@github.com"
            , "googlecode.com"
            ]
        ]

    , Filter [LabelAs "notification", MarkAsImportant False, NeverSpam]
        $ (pure . From . orB)
            [ "MAILER-DAEMON"
            , "facebookmail.com"
            , "no-reply@mail.instagram.com"
            , "noreply@foursquare.com"
            , "noreply@youtube.com"
            , "notifications.pinterest.com"
            , "notify@twitter.com"
            , "plus.google.com"
            ]
        \/ pure (List "meetup.com")

    ]

