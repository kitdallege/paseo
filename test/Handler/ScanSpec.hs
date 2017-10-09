{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.ScanSpec (spec) where

import TestImport

{-
/scans ScansR:
  /             ScanListR       GET
  /#Text        ScanDetailR     GET
  /#Text/#Int   ScanPageDetailR GET

-}
spec :: Spec
spec = withApp $ do

    describe "Scan Index" $ do
        it "loads the index and check that it looks right" $ do
            get (ScansR ScanListR)
            statusIs 200
