{-# LANGUAGE TemplateHaskell #-}

module Test.QuickCheck.TH.Generators.Internal (makeArbitrary) where


import           Data.Monoid ((<>))

import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax

import           Test.QuickCheck
import           Test.QuickCheck.TH.Generators.Internal.BuildArbitrary


-- | create buildArb1 through buildArb20 automatically
$(buildArbAny 1)
$(buildArbAny 2)
$(buildArbAny 3)
$(buildArbAny 4)
$(buildArbAny 5)
$(buildArbAny 6)
$(buildArbAny 7)
$(buildArbAny 8)
$(buildArbAny 9)
$(buildArbAny 10)
$(buildArbAny 11)
$(buildArbAny 12)
$(buildArbAny 13)
$(buildArbAny 14)
$(buildArbAny 15)
$(buildArbAny 16)
$(buildArbAny 17)
$(buildArbAny 18)
$(buildArbAny 19)
$(buildArbAny 20)

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
                         return dec

-- | build the function taht applys the type constructor
applyCon :: Name -> [Con] -> DecsQ
applyCon n cons' = sequence [signature,value]
  where
    signature = sigD finalFunctionName (appT (conT ''Gen) (conT n))
    value =   valD (varP finalFunctionName) (normalB (makeArbList cons')) []
    finalFunctionName = mkName ("arbitrary" <> nameBase n)


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
-- Q Exp
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
    runMapAndApp 11 = appE (varE 'buildArb11 ) (conE conName)
    runMapAndApp 12 = appE (varE 'buildArb12 ) (conE conName)
    runMapAndApp 13 = appE (varE 'buildArb13 ) (conE conName)
    runMapAndApp 14 = appE (varE 'buildArb14 ) (conE conName)
    runMapAndApp 15 = appE (varE 'buildArb15 ) (conE conName)
    runMapAndApp 16 = appE (varE 'buildArb16 ) (conE conName)
    runMapAndApp 17 = appE (varE 'buildArb17 ) (conE conName)
    runMapAndApp 18 = appE (varE 'buildArb18 ) (conE conName)
    runMapAndApp 19 = appE (varE 'buildArb19 ) (conE conName)
    runMapAndApp 20 = appE (varE 'buildArb20 ) (conE conName)

    runMapAndApp _ = error "Arbitrary TypeConstructors only defined for 0 to 20 parameters"

{- attempting to automate it further
applyConExp :: (Name, [StrictType]) -> ExpQ
applyConExp deconstructedConstructor = -- runMapAndApp argCount
  case (argCount >= 0) && (argCount <= 20) of
    True -> do
      mBuildArbn <- lookupValueName buildArb
      case mBuildArbn of
        Nothing -> error "Could not find buildArbn function, TH error"
        Just buildArbn -> appE (varE buildArbn) (conE conName)

    False -> error "Arbitrary TypeConstructors only defined for 0 to 20 parameters"
  where
    conName = fst deconstructedConstructor
    argCount = fromIntegral . length . snd $ deconstructedConstructor :: Int
    buildArb = "buildArb" ++ show argCount
-}

arbReturn :: a -> Gen a
arbReturn = return
