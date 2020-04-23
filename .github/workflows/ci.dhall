let haskellCi = ./haskell-ci.dhall

in    haskellCi.generalCi
        haskellCi.matrixSteps
        ( Some
            { ghc = [ haskellCi.GHC.GHC883, haskellCi.GHC.GHC865
                    , haskellCi.GHC.GHC844, haskellCi.GHC.GHC822
                    ]
            , cabal = [ haskellCi.Cabal.Cabal30 ]
            }
        )
    : haskellCi.CI.Type

