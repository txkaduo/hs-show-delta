module Data.ShowDelta.TH
  ( deriveShowDelta, deriveShowDeltaOpts
  , Options(..)
  , lowerHead
  ) where

import Control.Monad
import Data.Char (toLower)
import Data.Default (Default(..))
import Data.Maybe
import Data.String
import Data.Text (intercalate)
import qualified Data.Text as T
import Text.Show.Unicode (ushow)

import Language.Haskell.TH

import Data.ShowDelta.Class


data Options = Options
  { optFieldLabelModifier :: String -> String
  , optConstructorTagModifier :: String -> String
  }

instance Default Options where
  def = Options id id


deriveShowDelta :: Name -> Q [Dec]
deriveShowDelta = deriveShowDeltaOpts def

deriveShowDeltaOpts :: Options -> Name -> Q [Dec]
deriveShowDeltaOpts opts ty =
  do (TyConI tyCon) <- reify ty
     (tyConName, tyVars, cs) <- case tyCon of
         DataD _ nm tyVars Nothing cs _   -> return (nm, tyVars, cs)
         NewtypeD _ nm tyVars Nothing c _ -> return (nm, tyVars, [c])
         _ -> fail "deriveShowDelta: tyCon may not be a type synonym."

#if MIN_VERSION_template_haskell(2, 19, 0)
     let (KindedTV _tyVar _ StarT) = last tyVars
#else
     let (KindedTV _tyVar StarT) = last tyVars
#endif
     let instanceType           = conT ''ShowDelta `appT` (foldl apply (conT tyConName) tyVars)

     sequence [instanceD (return []) instanceType [genShowDelta opts cs]]
  where
#if MIN_VERSION_template_haskell(2, 19, 0)
    apply t (PlainTV name _)    = appT t (varT name)
    apply t (KindedTV name _ _) = appT t (varT name)
#else
    apply t (PlainTV name)    = appT t (varT name)
    apply t (KindedTV name _) = appT t (varT name)
#endif

genShowDelta :: Options -> [Con] -> Q Dec
genShowDelta opts cs
  = do funD 'showDelta (map (uncurry $ genShowDeltaClause opts) two_cs)
       where
         two_cs = [ (x1, x2) | x1 <- cs, x2 <- cs ]


genShowDeltaClause :: Options -> Con -> Con -> Q Clause
genShowDeltaClause opts cX@(NormalC nameX fieldTypesX) cY@(NormalC nameY fieldTypesY)
  = do x <- newName "x"
       y <- newName "y"
       fieldNamesX <- replicateM (length fieldTypesX) (newName "xField")
       fieldNamesY <- replicateM (length fieldTypesY) (newName "yField")
       if cX == cY
          then do let pats = [conP nameX (map varP fieldNamesX), conP nameY (map varP fieldNamesY)]
                      twoFieldNames = zip fieldNamesX fieldNamesY
                      body = normalB $ do
                              let fieldDeltas = flip map twoFieldNames $ \ (f1, f2) -> do
                                              [e| showDelta $(varE f1) $(varE f2) |]
                              [e| let comb_delta = intercalate ", " (catMaybes $(listE fieldDeltas))
                                    in if T.null comb_delta
                                          then Nothing
                                          else Just $ fromString sNameX <> " { " <> comb_delta <> " }"
                                |]

                  clause pats body []

          else do let pats = [asP x (conP nameX (map varP fieldNamesX)), asP y (conP nameY (map varP fieldNamesY))]
                      body = normalB $ do
                              [e| Just $ fromString (ushow $(varE x)) <> " => " <> fromString (ushow $(varE y))
                                |]
                  clause pats body []

    where sNameX = optConstructorTagModifier opts $ nameBase nameX

genShowDeltaClause opts cX@(RecC nameX nameBangTypesX) cY@(RecC nameY nameBangTypesY)
  = do x <- newName "x"
       y <- newName "y"
       fieldNamesX <- replicateM (length nameBangTypesX) (newName "xField")
       fieldNamesY <- replicateM (length nameBangTypesY) (newName "yField")
       if cX == cY
          then do let pats = [conP nameX (map varP fieldNamesX), conP nameY (map varP fieldNamesY)]
                      twoFieldNames = zip3 nameNamesX fieldNamesX fieldNamesY
                      body = normalB $ do
                               let fieldDeltas = flip map twoFieldNames $ \ (n, f1, f2) -> do
                                               [e| ((n <> ": ") <>) <$> showDelta $(varE f1) $(varE f2) |]
                               [e| let comb_delta = intercalate ", " (catMaybes $(listE fieldDeltas))
                                     in if T.null comb_delta
                                           then Nothing
                                           else Just $ fromString sNameX <> " { " <> comb_delta <> " }"
                                 |]

                  clause pats body []

          else do let pats = [asP x (conP nameX (map varP fieldNamesX)), asP y (conP nameY (map varP fieldNamesY))]
                      body = normalB $ do
                              [e| Just $ fromString (ushow $(varE x)) <> " => " <> fromString (ushow $(varE y))
                                |]
                  clause pats body []

  where
    sNameX = optConstructorTagModifier opts $ nameBase nameX
    nameNamesX = map (\ (x1, _, _) -> optFieldLabelModifier opts $ nameBase x1) nameBangTypesX

genShowDeltaClause _ x y = fail $ "show delta does not support these types: " <> show x <> " and " <> show y

lowerHead :: String -> String
lowerHead [] = []
lowerHead (x:xs) = toLower x : xs
