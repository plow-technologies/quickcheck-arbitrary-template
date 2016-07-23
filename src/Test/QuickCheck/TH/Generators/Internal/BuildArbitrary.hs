{-# LANGUAGE TemplateHaskell #-}

module Test.QuickCheck.TH.Generators.Internal.BuildArbitrary where

import           Language.Haskell.TH

import           Safe


-- | automatically build functions named `buildArbn` where n is an integer
-- greater than 0. $(buildArbAny 3) creates a function buildArb3 which takes
-- a constructor that takes 3 parameters, and returns an arbitrary instances
-- of that constructor. It assumes that the constructors type is an instance
-- of Arbitrary.
--
-- buildArb1 :: Arbitrary a
--           => (a -> b)
--           -> Gen b
-- buildArb1 f = f <$> arbitrary
--
-- buildArb2 :: (Arbitrary a, Arbitrary a1)
--           => (a1 -> a -> b)
--           -> Gen b
-- buildArb2 f = f <$> arbitrary <*> arbitrary

buildArbAny :: Int -> Q [Dec]
buildArbAny l
  | l < 1     = return []
  | otherwise = do
    -- name of the function to be returned
    let buildArbName = mkName ("buildArb" ++ show l)

    -- names of parameters used in the buildArbn function
    arbParameterNames <- mapM newName $ (("a" ++) . show) <$> [0..(l-1)] -- one for each parameter of f
    let mFirstArbParameterName = headMay arbParameterNames
        mRestArbParameterNames = tailMay arbParameterNames
    bName <- newName "b"  -- the return type of buildArbn
    fName <- newName "f"  -- the sum type constructor

    -- types used in the buildArbn function
    mArbitraryTypeName <- lookupTypeName "Arbitrary"
    mGenTypeName       <- lookupTypeName "Gen"

    -- functions used in the buildArbn function
    mFmapName        <- lookupValueName "<$>"
    mApName          <- lookupValueName "<*>"
    mArbitraryValue  <- lookupValueName "arbitrary"

    -- check if all the types and functions were found
    case (,,,,,,) <$> mFirstArbParameterName <*> mRestArbParameterNames <*> mArbitraryTypeName <*> mGenTypeName <*> mFmapName <*> mApName <*> mArbitraryValue of
      Nothing -> return []
      Just (firstArbParameterName,restArbParameterNames,arbitraryTypeName,genTypeName,fmapName,apName,arbitraryValue) -> do
        -- all of the variables in the function to be created, input and output
        let plainTVs = PlainTV <$> (arbParameterNames ++ [bName])
            -- Arbitrary type instance required for all vars in arbParameterNames
            typeClassRequirements = (AppT (ConT arbitraryTypeName) ) <$> (VarT <$> arbParameterNames)
            genB     = AppT (ConT genTypeName) (VarT bName)

            -- last parameter and return type
            -- (a -> Gen b)
            aToGenB = (AppT
                        (AppT
                          ArrowT
                          (VarT firstArbParameterName))
                        (VarT bName))
            -- fold function
            buildFunctionArgument old new = (AppT
                                              (AppT
                                                ArrowT
                                                (VarT new))
                                              old)
            -- build the rest of the parameters
            preFunctionArgument = AppT ArrowT $ foldl buildFunctionArgument
                                                      aToGenB
                                                      restArbParameterNames
            -- TH encoding for function arguments
            functionArgument = AppT preFunctionArgument genB

            -- build the function body
            -- f <$> arbitrary
            fFmapArbitrary = (InfixE (Just (VarE fName)) (VarE fmapName) (Just (VarE arbitraryValue)))
            arbRs = replicate (length arbParameterNames - 1) arbitraryValue

            -- build rest of body by folding <*> arbitrary
            preFunctionBody = foldl (\old new -> InfixE (Just old) (VarE apName) (Just (VarE new)))
                                    fFmapArbitrary
                                    arbRs

            -- TH encoding for function body
            functionBody = FunD buildArbName [Clause [VarP fName] (NormalB preFunctionBody) []]

        return
          [SigD
            buildArbName
            (ForallT
              plainTVs
              typeClassRequirements
              functionArgument
            )
          , functionBody
          ]
