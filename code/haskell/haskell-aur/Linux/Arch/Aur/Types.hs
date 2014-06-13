{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module    : Linux.Arch.Aur.Types
-- Copyright : (c) Colin Woodbury, 2014
-- License   : GPL3
-- Maintainer: Colin Woodbury <colingw@gmail.com>

module Linux.Arch.Aur.Types
    ( AurInfo(..) ) where

import Control.Applicative
import Control.Monad (mzero)
import Data.Aeson
import Data.Text

---

data AurInfo = AurInfo { aurIdOf          :: Int
                       , aurNameOf        :: Text
                       , pkgBaseIdOf      :: Int
                       , pkgBaseOf        :: Text
                       , aurVersionOf     :: Text
                       , aurCategoryOf    :: Int
                       , aurDescriptionOf :: Text
                       , licenseOf        :: [Text]
                       , urlOf            :: Text
                       , aurVotesOf       :: Int
                       , isOutOfDate      :: Bool
                       , aurMaintainerOf  :: Text
                       , submissionDatOf  :: Int
                       , modifiedDateOf   :: Int
                       , aurTarballUrlOf  :: Text
                       , dependsOf        :: [Text]
                       , makeDepsOf       :: [Text]
                       , optDepsOf        :: [Text]
                       , conflictsOf      :: [Text]
                       , providesOf       :: [Text] } deriving (Eq,Show)

instance FromJSON AurInfo where
    parseJSON (Object v) = AurInfo                    <$>
                           v .:  "ID"                 <*>
                           v .:  "Name"               <*>
                           v .:  "PackageBaseID"      <*>
                           v .:  "PackageBase"        <*>
                           v .:  "Version"            <*>
                           v .:  "CategoryID"         <*>
                           v .:  "Description"        <*>
                           v .:? "License"     .!= [] <*>
                           v .:  "URL"                <*>
                           v .:  "NumVotes"           <*>
                           (f <$> (v .:  "OutOfDate")) <*>
                           v .:  "Maintainer"         <*>
                           v .:  "FirstSubmitted"     <*>
                           v .:  "LastModified"       <*>
                           v .:  "URLPath"            <*>
                           v .:? "Depends"     .!= [] <*>
                           v .:? "MakeDepends" .!= [] <*>
                           v .:? "OptDepends"  .!= [] <*>
                           v .:? "Conflicts"   .!= [] <*>
                           v .:? "Provides"    .!= []
                               where f :: Int -> Bool
                                     f = (/= 0)
    parseJSON _          = mzero
