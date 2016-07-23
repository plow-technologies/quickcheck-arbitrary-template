{-# LANGUAGE TemplateHaskell #-}

module Test.QuickCheck.TH.Generators.Internal
    (makeArbitrary
    ) where


import           Data.Monoid ((<>))

import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax

import           Test.QuickCheck
import           Test.QuickCheck.TH.Generators.Internal.BuildArbitrary

import           Control.Monad

$(buildArbAny 2)
$(buildArbAny 3)
$(buildArbAny 4)
$(buildArbAny 5)
$(buildArbAny 6)
$(buildArbAny 7)
$(buildArbAny 8)
$(buildArbAny 9)
$(buildArbAny 10)

-- | Boilerplate for top level splices.
--
-- The given 'Name' must be from a type constructor. Furthermore, the
-- type constructor must be either a data type or a newtype. Any other
-- value will result in an exception.
withType :: Name
         -> ([TyVarBndr] -> [Con] -> Q a)
         -- ^ Function that generates the actual code. Will be applied
         -- to the type variable binders and constructors extracted
         -- from the given 'Name'.
         -> Q a
         -- ^ Resulting value in the 'Q'uasi monad.
withType name f = do
    info <- reify name
    case info of
      TyConI dec ->
        case dec of
          DataD    _ _ tvbs cons' _ -> f tvbs cons'
          NewtypeD _ _ tvbs con  _ -> f tvbs [con]
          other -> error $ "Example.TH.withType: Unsupported type: "
                          ++ show other
      _ -> error "Example.TH.withType: I need the name of a type."


-- | Extracts the name from a constructor.

-- | Make a ('Gen' a) for type 'a'
-- Currently support arbitrary Sum types up to 7 params
-- per constructor.
--
-- Record Types not currently supported
makeArbitrary :: Name -> Q [Dec]
makeArbitrary n = withType n runConstructionApp
  where
    runConstructionApp _  con = do
                         dec <- applyCon n con
                         return [dec]

-- | build the function taht applys the type constructor
applyCon :: Name -> [Con] -> DecQ
applyCon n cons' = valD (varP $ mkName ("arbitrary" <>nameBase n))
                    (normalB (makeArbList cons')) []


-- | select one of the list of generators
-- Q Exp == oneOf [Gen *]
makeArbList :: [Con] -> Q Exp
makeArbList cons' = appE (varE 'oneof)
                        (listE $ asNormalOrRecC applyConExp cons'  )

-- | Normal Constructors are the only ones we are considering
asNormalOrRecC  :: ((Name, [StrictType]) -> a) -> [Con] -> [a]
asNormalOrRecC  f cons' = foldr decodeC [] cons'
  where
   decodeC (RecC n l)   lst  = (f (n, varStrictToStrict <$>  l)) : lst
   decodeC (NormalC n l) lst = (f (n, l)) : lst
   decodeC _ lst = lst
   varStrictToStrict (_ , s,t) = (s,t)

-- | This is where we run the sum type thing
applyConExp :: (Name, [StrictType]) -> ExpQ
applyConExp deconstructedConstructor = runMapAndApp argCount
  where
    conName = fst deconstructedConstructor
    argCount = fromIntegral . length . snd $ deconstructedConstructor :: Integer

    runMapAndApp :: Integer -> ExpQ
    runMapAndApp 0 = appE (varE 'arbReturn ) (conE conName)
    runMapAndApp 1 = appE (varE 'buildArb1 ) (conE conName)
    runMapAndApp 2 = appE (varE 'buildArb2 ) (conE conName)
    runMapAndApp 3 = appE (varE 'buildArb3 ) (conE conName)
    runMapAndApp 4 = appE (varE 'buildArb4 ) (conE conName)
    runMapAndApp 5 = appE (varE 'buildArb5 ) (conE conName)
    runMapAndApp 6 = appE (varE 'buildArb6 ) (conE conName)
    runMapAndApp 7 = appE (varE 'buildArb7 ) (conE conName)
    runMapAndApp 8 = appE (varE 'buildArb8 ) (conE conName)
    runMapAndApp 9 = appE (varE 'buildArb9 ) (conE conName)
    runMapAndApp 10 = appE (varE 'buildArb10 ) (conE conName)
    runMapAndApp _ = error "Arbitrary TypeConstructors only defined for 8 or fewer"


arbReturn :: a -> Gen a
arbReturn = return


buildArb1 :: Arbitrary a
          => (a -> b)
          -> Gen b
buildArb1 f = f <$> arbitrary

{-
buildArb2 :: (Arbitrary a, Arbitrary a1)
          => (a1 -> a -> b)
          -> Gen b
buildArb2 f = ((f <$> arbitrary) <*> arbitrary)

buildArb3 :: (Arbitrary a, Arbitrary a1, Arbitrary a2)
          =>  (a2 -> a1 -> a -> b)
          -> Gen b
buildArb3 f = f <$> arbitrary
                <*> arbitrary
                <*> arbitrary


buildArb4 :: (Arbitrary a, Arbitrary a1, Arbitrary a2, Arbitrary a3)
           => (a3 -> a2 -> a1 -> a -> b)
           -> Gen b
buildArb4 f = f <$> arbitrary
                <*> arbitrary
                <*> arbitrary
                <*> arbitrary


buildArb5 :: (Arbitrary a, Arbitrary a1, Arbitrary a2, Arbitrary a3, Arbitrary a4)
          => (a4 -> a3 -> a2 -> a1 -> a -> b)
          -> Gen b
buildArb5 f = f <$> arbitrary
                <*> arbitrary
                <*> arbitrary
                <*> arbitrary
                <*> arbitrary

buildArb6 :: (Arbitrary a, Arbitrary a1, Arbitrary a2, Arbitrary a3, Arbitrary a4, Arbitrary a5)
          => (a5 -> a4 -> a3 -> a2 -> a1 -> a -> b)
          -> Gen b
buildArb6 f = f <$> arbitrary
                <*> arbitrary
                <*> arbitrary
                <*> arbitrary
                <*> arbitrary
                <*> arbitrary


buildArb7 :: (Arbitrary a, Arbitrary a1, Arbitrary a2, Arbitrary a3, Arbitrary a4, Arbitrary a5, Arbitrary a6)
          => (a6 -> a5 -> a4 -> a3 -> a2 -> a1 -> a -> b)
          -> Gen b
buildArb7 f = f <$> arbitrary
                <*> arbitrary
                <*> arbitrary
                <*> arbitrary
                <*> arbitrary
                <*> arbitrary
                <*> arbitrary

buildArb8 :: (Arbitrary a, Arbitrary a1, Arbitrary a2, Arbitrary a3, Arbitrary a4, Arbitrary a5, Arbitrary a6, Arbitrary a7)
          => (a7 -> a6 -> a5 -> a4 -> a3 -> a2 -> a1 -> a -> b)
          -> Gen b
buildArb8 f = f <$> arbitrary
                <*> arbitrary
                <*> arbitrary
                <*> arbitrary
                <*> arbitrary
                <*> arbitrary
                <*> arbitrary
                <*> arbitrary
-}
