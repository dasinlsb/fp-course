{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.FastAnagrams where

import Course.Core
import Course.List
import Course.Functor
import qualified Data.Set as S

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
fastAnagrams ::
  Chars
  -> FilePath
  -> IO (List Chars)
fastAnagrams ana file =
  (filter ((==) (qsort ana) . qsort) . lines) <$> readFile file
    where qsort Nil = Nil
          qsort (x :. xs) = qsort l' ++ (x :. Nil) ++ qsort r'
            where l' = filter (< x) xs
                  r' = filter (>= x) xs

newtype NoCaseString =
  NoCaseString {
    ncString :: Chars
  }

instance Eq NoCaseString where
  (==) = (==) `on` map toLower . ncString

instance Show NoCaseString where
  show = show . ncString
