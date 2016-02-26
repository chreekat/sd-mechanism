{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
module Persist where

import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateMech"] [persistLowerCase|
MechPatron
    wallet Int
    externalKey Int
    ExternalPatron externalKey
MechProject
    dropbox Int
    externalKey Int
    ExternalProject externalKey
Pledge
    project MechProjectId
    patron MechPatronId
    UniquePledge project patron
|]
