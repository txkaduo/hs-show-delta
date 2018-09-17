module Data.ShowDelta.Class
  ( ShowDelta(..)
  , defaultShowDelta, defaultShowDelta', defaultShowListDelta
  ) where

import Control.Monad
import Data.Function (on)
import Data.Monoid
import Data.Maybe
import Data.Int
import Data.String
import Data.Text (Text, intercalate)
import Data.Time
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base16.Lazy as LB16
import Text.Show.Unicode (ushow)


class ShowDelta a where
  showDelta :: a -> a -> Maybe Text
  default showDelta :: (Eq a, Show a) => a -> a -> Maybe Text
  showDelta = defaultShowDelta


defaultShowDelta :: (Eq a, Show a) => a -> a -> Maybe Text
defaultShowDelta = defaultShowDelta' (fromString . ushow)

defaultShowDelta' :: (Eq a) => (a -> Text) -> a -> a -> Maybe Text
defaultShowDelta' show_func x y = do
  guard $ x /= y
  return $ show_func x <> " => " <> show_func y

defaultShowListDelta :: (ShowDelta a, Show a) => [a] -> [a] -> Maybe Text
defaultShowListDelta xs0 ys0 = do
  guard $ not $ null deltas
  pure $ "[ " <> intercalate ", " deltas <> " ]"
  where go idx (x:xs) (y:ys) = fmap ((tshow idx <> ": ") <> ) (showDelta x y) : go (idx + 1) xs ys
        go idx []     (y:ys) = pure (tshow idx <> ": _ => " <> tshow y) : go (idx + 1) [] ys
        go idx (x:xs) []     = pure (tshow idx <> ": " <> tshow x <> " => _") : go (idx + 1) xs []
        go _   []     []     = []

        deltas = catMaybes $ go (0 :: Int) xs0 ys0


instance (ShowDelta a, Show a) => ShowDelta (Maybe a) where
  showDelta Nothing Nothing = Nothing
  showDelta Nothing (Just y) = pure $ fromString $ "Nothing => " <> ushow y
  showDelta (Just x) Nothing = pure $ fromString $ ushow x <> " => Nothing"
  showDelta (Just x) (Just y) = showDelta x y

instance {-# OVERLAPPABLE #-} (ShowDelta a, Show a) => ShowDelta [a] where
  showDelta = defaultShowListDelta


instance (ShowDelta a, ShowDelta b) => ShowDelta (a, b) where
  showDelta (x1, x2) (y1, y2) = do
    guard $ not $ null mdeltas
    pure $ "(" <> intercalate ", " (map (fromMaybe "_") mdeltas) <> ")"
    where mdeltas = [ showDelta x1 y1, showDelta x2 y2 ]


instance (ShowDelta a, ShowDelta b, ShowDelta c) => ShowDelta (a, b, c) where
  showDelta (x1, x2, x3) (y1, y2, y3) = do
    guard $ not $ null mdeltas
    pure $ "(" <> intercalate ", " (map (fromMaybe "_") mdeltas) <> ")"
    where mdeltas = [ showDelta x1 y1, showDelta x2 y2, showDelta x3 y3 ]


instance ShowDelta Bool
instance ShowDelta Char
instance {-# OVERLAPPING #-} ShowDelta String where showDelta = defaultShowDelta
instance ShowDelta Text
instance ShowDelta LT.Text
instance ShowDelta Integer
instance ShowDelta Int
instance ShowDelta Int8
instance ShowDelta Int16
instance ShowDelta Int32
instance ShowDelta Int64
instance ShowDelta Float
instance ShowDelta Double
instance ShowDelta Rational
instance ShowDelta UTCTime
instance ShowDelta LocalTime
instance ShowDelta NominalDiffTime

instance ShowDelta B.ByteString where
  showDelta = showDelta `on` (C8.unpack . B16.encode)

instance ShowDelta LB.ByteString where
  showDelta = showDelta `on` (LC8.unpack . LB16.encode)

tshow :: Show a => a -> Text
tshow = fromString . ushow
