{-# LANGUAGE TemplateHaskell #-}

module Test.QuickCheck.TH.Generators.Internal.BuildArbitrary where

import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax

import           Test.QuickCheck


{-
buildArb2 :: (Arbitrary a, Arbitrary a1)
          => (a1 -> a -> b)
          -> Gen b
buildArb2 f = ((f <$> arbitrary) <*> arbitrary)

-}

-- check that l greater than 0
buildArbAny :: Int -> Q [Dec]
buildArbAny l = do
  let buildArbName = mkName ("buildArb" ++ show l)

  arbNames      <- mapM newName $ (("a_" ++) . show) <$> [0..(l-1)]
  bName         <- newName "b"
  fName         <- newName "f"

  mArbitraryTypeName <- lookupTypeName "Arbitrary"
  mGenTypeName       <- lookupTypeName "Gen"

  mApName            <- lookupValueName "<$>"
  mFmapName          <- lookupValueName "<*>"
  mArbitraryValue    <- lookupValueName "arbitrary"

  case (,,,,) <$> mArbitraryTypeName <*> mGenTypeName <*> mFmapName <*> mArbitraryValue <*> mApName of
    Nothing -> return []
    Just (arbitraryTypeName,genTypeName,fmapName,arbitraryValue,apName) -> do
      let plainTVs = PlainTV <$> (arbNames ++ [bName])
      let typeClassRequirements = (AppT (ConT arbitraryTypeName) ) <$> (VarT <$> arbNames)

      -- (AppT (ConT Test.QuickCheck.Gen.Gen) (VarT b_3))
      let genB     = AppT (ConT genTypeName) (VarT bName)


      --let arbBackwards = reverse arbNames
      -- return argument
      {-
      (AppT
        (AppT
          ArrowT
          (AppT
            (AppT
              ArrowT
              (VarT a1_1))
            (AppT
              (AppT
                ArrowT
                (VarT a_2))
              (VarT b_3))))
        (AppT (ConT Test.QuickCheck.Gen.Gen) (VarT b_3))
      )
      -}

      {-
      (AppT
        (AppT
          ArrowT
          (AppT
            (AppT
              ArrowT
              (VarT a2_1))
            (AppT
              (AppT
                ArrowT
                (VarT a1_2))
              (AppT
                (AppT
                  ArrowT
                  (VarT a_3))
                (VarT b_4)))))
        (AppT (ConT Test.QuickCheck.Gen.Gen) (VarT b_4)))

      -}
      -- very bottom, VarT bName
      -- AppT (AppT ArrowT new) old
      let fs = (AppT
                 (AppT
                   ArrowT
                   (VarT $ head arbNames))
                 (VarT bName))
      let foldFunc old new = (AppT
                               (AppT
                                 ArrowT
                                 (VarT new))
                               old)
      let preFunctionArgument = AppT ArrowT $ foldl foldFunc -- (\old new -> AppT ArrowT (AppT (AppT ArrowT (VarT new)) old))
                                                    fs
                                                    (tail arbNames)

      {-
      let fs = (AppT (AppT ArrowT (VarT $ head arbNames)) (VarT bName))
      let preFunctionArgument = foldl (\old new -> AppT ArrowT (AppT (AppT ArrowT (VarT new)) old))
                                   fs
                                   (tail arbNames)
      -}
      let functionArgument = AppT preFunctionArgument genB

      let infi = (InfixE (Just (VarE fName)) (VarE apName) (Just (VarE arbitraryValue)))
      let arbRs = replicate (length arbNames - 1) arbitraryValue
      let preFunctionBody = foldl (\old new -> InfixE (Just old) (VarE fmapName) (Just (VarE new)))
                               infi
                               arbRs
      let functionBody = FunD buildArbName [Clause [VarP fName] (NormalB preFunctionBody) []]

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

{- -- very bottom, VarT bName
   -- AppT (AppT ArrowT new) old
-- 3
(AppT
  (AppT
    ArrowT
    (AppT
      (AppT ArrowT (VarT a2_1))
      (AppT
        (AppT ArrowT (VarT a1_2))
        (AppT
          (AppT ArrowT (VarT a_3))
          (VarT b_4)))))
  (AppT (ConT Test.QuickCheck.Gen.Gen) (VarT b_4)))

-- 2
(AppT
  (AppT
    ArrowT
    (AppT
      (AppT ArrowT (VarT a1_1))
      (AppT
        (AppT ArrowT (VarT a_2))
        (VarT b_3))))
  (AppT (ConT Test.QuickCheck.Gen.Gen) (VarT b_3)))

[SigD buildArb3_0
  (ForallT
    [PlainTV a2_1,PlainTV a1_2,PlainTV a_3,PlainTV b_4]
    [AppT (ConT Test.QuickCheck.Arbitrary.Arbitrary) (VarT a_3)
    ,AppT (ConT Test.QuickCheck.Arbitrary.Arbitrary) (VarT a1_2)
    ,AppT (ConT Test.QuickCheck.Arbitrary.Arbitrary) (VarT a2_1)
    ]

    (AppT
      (AppT ArrowT
        (AppT
          (AppT ArrowT (VarT a2_1))
          (AppT
            (AppT ArrowT (VarT a1_2))
            (AppT (AppT ArrowT (VarT a_3)) (VarT b_4)))))
      (AppT (ConT Test.QuickCheck.Gen.Gen) (VarT b_4))))
  ,FunD buildArb3_0 [Clause [VarP f_5] (NormalB (InfixE (Just (InfixE (Just (InfixE (Just (VarE f_5)) (VarE Data.Functor.<$>) (Just (VarE Test.QuickCheck.Arbitrary.arbitrary)))) (VarE GHC.Base.<*>) (Just (VarE Test.QuickCheck.Arbitrary.arbitrary)))) (VarE GHC.Base.<*>) (Just (VarE Test.QuickCheck.Arbitrary.arbitrary)))) []]]



[SigD buildArb2_0
  (ForallT
    [PlainTV a1_1,PlainTV a_2,PlainTV b_3]
    [ AppT  (ConT Test.QuickCheck.Arbitrary.Arbitrary) (VarT a_2)
    , AppT (ConT Test.QuickCheck.Arbitrary.Arbitrary) (VarT a1_1)
    ]
    (AppT
      (AppT ArrowT
        (AppT (AppT ArrowT (VarT a1_1))
          (AppT (AppT ArrowT (VarT a_2)) (VarT b_3))))
      (AppT (ConT Test.QuickCheck.Gen.Gen) (VarT b_3))
    )
  )
  ,FunD
    buildArb2_0 [Clause [VarP f_4]
    (NormalB (InfixE (Just (InfixE (Just (VarE f_4))
      (VarE Data.Functor.<$>)
      (Just (VarE Test.QuickCheck.Arbitrary.arbitrary))))
      (VarE GHC.Base.<*>)
      (Just (VarE Test.QuickCheck.Arbitrary.arbitrary)))) []]]

[SigD buildArb3_0 (ForallT [PlainTV a2_1,PlainTV a1_2,PlainTV a_3,PlainTV b_4] [AppT (ConT Test.QuickCheck.Arbitrary.Arbitrary) (VarT a_3),AppT (ConT Test.QuickCheck.Arbitrary.Arbitrary) (VarT a1_2),AppT (ConT Test.QuickCheck.Arbitrary.Arbitrary) (VarT a2_1)] (AppT (AppT ArrowT (AppT (AppT ArrowT (VarT a2_1)) (AppT (AppT ArrowT (VarT a1_2)) (AppT (AppT ArrowT (VarT a_3)) (VarT b_4))))) (AppT (ConT Test.QuickCheck.Gen.Gen) (VarT b_4)))),FunD buildArb3_0 [Clause [VarP f_5] (NormalB (InfixE (Just (InfixE (Just (InfixE (Just (VarE f_5)) (VarE Data.Functor.<$>) (Just (VarE Test.QuickCheck.Arbitrary.arbitrary)))) (VarE GHC.Base.<*>) (Just (VarE Test.QuickCheck.Arbitrary.arbitrary)))) (VarE GHC.Base.<*>) (Just (VarE Test.QuickCheck.Arbitrary.arbitrary)))) []]]
[SigD buildArb2_0 (ForallT [PlainTV a1_1,PlainTV a_2,PlainTV b_3] [AppT (ConT Test.QuickCheck.Arbitrary.Arbitrary) (VarT a_2),AppT (ConT Test.QuickCheck.Arbitrary.Arbitrary) (VarT a1_1)] (AppT (AppT ArrowT (AppT (AppT ArrowT (VarT a1_1)) (AppT (AppT ArrowT (VarT a_2)) (VarT b_3)))) (AppT (ConT Test.QuickCheck.Gen.Gen) (VarT b_3)))),FunD buildArb2_0 [Clause [VarP f_4] (NormalB (InfixE (Just (InfixE (Just (VarE f_4)) (VarE Data.Functor.<$>) (Just (VarE Test.QuickCheck.Arbitrary.arbitrary)))) (VarE GHC.Base.<*>) (Just (VarE Test.QuickCheck.Arbitrary.arbitrary)))) []]]


build_p1 :: Q [Dec]
build_p1 = do
  let p1 = mkName "p1" -- global name
  z <- lookupValueName "p1"
  a <- newName "a"     -- local name
  b <- newName "b"     -- local name
  return
    [ FunD p1
      [ Clause [TupP [VarP a, VarP b]] (NormalB (VarE a)) []
      ]
    ]

buildArb_2 :: Q [Dec]
buildArb_2 = do
  let p1 = mkName "p1" -- global name
  z <- lookupValueName "p1"
  a <- newName "a"     -- local name
  b <- newName "b"     -- local name
  return
    [ FunD p1
      [ --Clause [TupP [VarP a, VarP b]] (NormalB (VarE a)) []
        Clause [] () []
      ]
    ]
-- FunD [Clause]
-- Clause [Pat] Body [Dec]
-- f { p1 p2 = body where decs}

-- runQ [d| p1 (a,b) = a |]

runQ [d| f1 f = fmap f |]
[FunD f1_3 [Clause [VarP f_4] (NormalB (AppE (VarE GHC.Base.fmap) (VarE f_4))) []]]


runQ [d|
p1 :: (a,b) -> a
p1 (a,b) = a
|]


[SigD p1_5 (ForallT [PlainTV a_6,PlainTV b_7] [] (AppT (AppT ArrowT (AppT (AppT (TupleT 2) (VarT a_6)) (VarT b_7))) (VarT a_6))),FunD p1_5 [Clause [TupP [VarP a_8,VarP b_9]] (NormalB (VarE a_8)) []]]


foldr (\x y -> "(" ++ x ++ y ++ ")") "" ["hello","world", "goodbye"]
"(hello(world(goodbye)))"

foldr (\x y -> "(" ++ y ++ x ++ ")") "" ["hello","world", "goodbye"]
"(((goodbye)world)hello)"

-}


-- typeClassRequirements
-- functionArguments
-- functionBody
{-
[ SigD buildArbName
(ForallT
  plainTVs
  appTs

)
]
-}
{-
,FunD
buildArb2_0
[Clause
[VarP f_4] -- [Pat]
(NormalB   -- Body
(InfixE
  (Just
    (InfixE
      (Just (VarE f_4))
      (VarE Data.Functor.<$>)
      (Just (VarE Test.QuickCheck.Arbitrary.arbitrary)))
  )
  (VarE GHC.Base.<*>)
  (Just (VarE Test.QuickCheck.Arbitrary.arbitrary))
)
)
[] -- Dec
]
]

-}
