{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Persist where

import Database.Persist.TH

mkPersist sqlSettings [persistLowerCase|
MechPatron
    wallet WalletId
MechProject
    dropbox DropboxId
Pledge
    patron MechPatronId
    project MechProjectId
    UniquePledge patron project
Wallet
    owner MechPatronId
    balance Int
Dropbox
    owner MechProjectId
    balance Int
|]
