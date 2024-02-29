-- | This module allows you to zip lists with any `Traversable` data structure.
--
-- It's intended that you import it qualified:
--
-- > import qualified Data.Traversable.HeteroZip as Hetero
-- > x = Hetero.zipWith ...

module Data.Traversable.HeteroZip
  ( -- * Zipping #zipping#
    -- $zipping
    zip
  , zipErr
  , zipInf
  , zipMay
  , zipNote
  , zipWith
  , zipWithErr
  , zipWithInf
  , zipWithMay
  , zipWithNote

    -- * Enumeration #enumeration#
    -- $enumeration
  , enumerate
  , enumerateWith

    -- * Holes #holes#
    -- $holes
  , setHoles
  , unsetHoles

    -- * A note on `Traversable` #traversable#
    -- $traversable
  ) where

import Prelude hiding (zip, zipWith)

import Data.Functor.Compose (Compose(..))
import Data.List.Infinite (Infinite(..))
import Data.Maybe ( fromMaybe )
import Data.Traversable ( mapAccumL )
import GHC.Stack ( HasCallStack, withFrozenCallStack )

-- $setup
-- >>> :set -Wno-type-defaults -Wno-name-shadowing
-- >>> import Data.Maybe (isJust)

-- $zipping
--
-- A `Traversable`'s elements can be visited one at a time, and updated
-- in-place. That means we can visit them at the same time as we walk along a
-- list, and use the values in the list to update the values in the
-- `Traversable`. These functions do just that.
--
-- @zip*@ functions simply pair elements off, while @zipWith*@ functions use an
-- explicit combining function, like regular `Prelude.zip` and
-- `Prelude.zipWith`. They each have five variants to deal with the possibility
-- that the input list is shorter than the `Traversable`:
--
-- * `zip` and `zipWith` pass a `Maybe` to the combining function. `zipWith` is
--   the most general function here; all the others can be implemented as simple
--   wrappers around that.
--
-- * `zipMay` and `zipWithMay` put `Maybe`s in the result `Traversable`.
--
-- * `zipErr` and `zipWithErr` are partial, throwing errors if the list is too
--    short.
--
-- * `zipNote` and `zipWithNote` are also partial, with a custom error message.
--
-- * `zipInf` and `zipWithInf` use an `Infinite` list that will never be too
--   short.
--
-- All these functions are lazy in both the list and `Traversable` arguments,
-- and in the function application, to the extent that the `Traversable`
-- instance allows. For example:
--
-- >>> take 3 $ zip ([1, 2, 3] ++ undefined) ([1, 2, 3] ++ undefined)
-- [(Just 1,1),(Just 2,2),(Just 3,3)]
-- >>> fst <$> zip [1, 2, 3] [1, 2, undefined]
-- [Just 1,Just 2,Just 3]
-- >>> isJust . fst <$> zip [1, 2, undefined] [1, 2, undefined]
-- [True,True,True]
-- >>> snd <$> zip [1, 2, undefined] [1, 2, 3]
-- [1,2,3]

-- $more-laziness-examples
--
-- >>> take 3 $ zipMay ([1, 2, 3] ++ undefined) ([1, 2, 3] ++ undefined)
-- [Just (1,1),Just (2,2),Just (3,3)]
-- >>> take 3 $ zipMay [1, 2] [1, 2, undefined]
-- [Just (1,1),Just (2,2),Nothing]
-- >>> fmap fst <$> zipMay [1, 2] [1, 2, undefined]
-- [Just 1,Just 2,Nothing]
-- >>> fmap fst <$> zipMay [1, 2, 3] [1, 2, undefined]
-- [Just 1,Just 2,Just 3]
-- >>> isJust <$> zipMay [1, 2, undefined] [1, 2, undefined]
-- [True,True,True]
-- >>> fmap snd <$> zipMay [1, 2, undefined] [1, 2, 3]
-- [Just 1,Just 2,Just 3]
--
-- >>> take 3 $ zipErr ([1, 2, 3] ++ undefined) ([1, 2, 3] ++ undefined)
-- [(1,1),(2,2),(3,3)]
-- >>> fst <$> zipErr [1, 2, 3] [1, 2, undefined]
-- [1,2,3]
-- >>> snd <$> zipErr [1, 2, undefined] [1, 2, 3]
-- [1,2,3]
--
-- >>> take 3 $ zipNote undefined ([1, 2, 3] ++ undefined) ([1, 2, 3] ++ undefined)
-- [(1,1),(2,2),(3,3)]
-- >>> fst <$> zipNote undefined [1, 2, 3] [1, 2, undefined]
-- [1,2,3]
-- >>> snd <$> zipNote undefined [1, 2, undefined] [1, 2, 3]
-- [1,2,3]
--
-- >>> take 3 $ zipInf (1 :< 2 :< 3 :< undefined) ([1, 2, 3] ++ undefined)
-- [(1,1),(2,2),(3,3)]
-- >>> fst <$> zipInf (1 :< 2 :< 3 :< undefined) [1, 2, 3]
-- [1,2,3]
-- >>> snd <$> zipInf (1 :< 2 :< undefined :< undefined) [1, 2, 3]
-- [1,2,3]

-- | Zip a list with any `Traversable`, maintaining the shape of the latter.
--
-- If the list is too short, pair with `Just` values until it runs out, and then
-- `Nothing`s.
--
-- >>> zip [1, 2] (Just ())
-- Just (Just 1,())
--
-- >>> zip [] (Just ())
-- Just (Nothing,())
--
-- >>> zip [] Nothing
-- Nothing
zip :: Traversable t => [a] -> t b -> t (Maybe a, b)
zip = zipWith (,)

-- | Zip a list with any `Traversable`, maintaining the shape of the latter.
--
-- If the list is too short, throw an error.
--
-- >>> zipErr [1, 2] (Just ())
-- Just (1,())
--
-- >>> zipErr [] (Just ())
-- Just (*** Exception: zipErr: list too short
-- CallStack (from HasCallStack):
--   zipErr, called at ...
--
-- >>> zipErr [] Nothing
-- Nothing
zipErr :: (HasCallStack, Traversable t) => [a] -> t b -> t (a, b)
zipErr = withFrozenCallStack $ zipNote "zipErr: list too short"

-- | Zip an `Infinite` list with any `Traversable`, maintaining the shape of the
-- latter.
--
-- >>> :set -XPostfixOperators
-- >>> import Data.List.Infinite ((...))
--
-- >>> zipInf (1...) (Just ())
-- Just (1,())
--
-- >>> zipInf (1...) Nothing
-- Nothing
zipInf :: Traversable t => Infinite a -> t b -> t (a, b)
zipInf = zipWithInf (,)
-- When infinite-list 0.1.2 is released, this is heteroZip.

-- | Zip a list with any `Traversable`, maintaining the shape of the latter.
--
-- If the list is too short, start producing `Nothing`s. You can use `sequence`
-- to get a @`Maybe` (t (a, b))@; but note that this must walk the whole
-- `Traversable` before it can produce a `Just`.
--
-- >>> zipMay [1, 2] (Just ())
-- Just (Just (1,()))
--
-- >>> zipMay [] (Just ())
-- Just Nothing
--
-- >>> zipMay [] Nothing
-- Nothing
zipMay :: Traversable t => [a] -> t b -> t (Maybe (a, b))
zipMay = zipWithMay (,)

-- | Zip a list with any `Traversable`, maintaining the shape of the latter.
--
-- If the list is too short, throw an error with a custom error string.
--
-- >>> zipNote "oops" [1, 2] (Just ())
-- Just (1,())
--
-- >>> zipNote "oops" [] (Just ())
-- Just (*** Exception: oops
-- CallStack (from HasCallStack):
--   zipNote, called at ...
--
-- >>> zipNote "oops" [] Nothing
-- Nothing
zipNote
  :: (HasCallStack, Traversable t) => String -> [a] -> t b -> t (a, b)
zipNote errStr = withFrozenCallStack $ zipWithNote errStr (,)

-- | Use a given function to zip a list with any `Traversable`, maintaining the
-- shape of the latter.
--
-- If the list is too short, pass `Just` values until it runs out, and then
-- `Nothing`s.
--
-- >>> zipWith (maybe id (+)) [1, 2] (Just 10)
-- Just 11
--
-- >>> zipWith (maybe id (+)) [] (Just 10)
-- Just 10
--
-- >>> zipWith (maybe id (+)) [] Nothing
-- Nothing
zipWith :: Traversable t => (Maybe a -> b -> c) -> [a] -> t b -> t c
zipWith f = snd .: mapAccumL
  (\as b -> case as of
    []        -> ([], f Nothing b)
    (a : as') -> (as', f (Just a) b)
  )

-- | Use a given function to zip a list with any `Traversable`, maintaining the
-- shape of the latter.
--
-- If the list is too short, throw an error.
--
-- >>> zipWithErr (+) [1, 2] (Just 10)
-- Just 11
--
-- >>> zipWithErr (+) [] (Just 10)
-- Just *** Exception: zipWithErr: list too short
-- CallStack (from HasCallStack):
--   zipWithErr, called at ...
--
-- >>> zipWithErr (+) [] Nothing
-- Nothing
zipWithErr
  :: (HasCallStack, Traversable t) => (a -> b -> c) -> [a] -> t b -> t c
zipWithErr = withFrozenCallStack $ zipWithNote "zipWithErr: list too short"


-- | Use a given function to zip an `Infinite` list with any `Traversable`,
-- maintaining the shape of the latter.
--
-- >>> :set -XPostfixOperators
-- >>> import Data.List.Infinite ((...))
--
-- >>> zipWithInf (+) (1...) (Just 10)
-- Just 11
--
-- >>> zipWithInf (+) (1...) Nothing
-- Nothing
zipWithInf
  :: Traversable t => (a -> b -> c) -> Infinite a -> t b -> t c
zipWithInf f = snd .: mapAccumL (\(a :<  as) b -> (as, f a b))
-- When infinite-list 0.1.2 is released, this is heteroZipWith.

-- | Use a given function to zip a list with any `Traversable`, maintaining the
-- shape of the latter.
--
-- If the list is too short, start producing `Nothing`s. You can use `sequence`
-- to get a @`Maybe` (t c)@; but note that this must walk the whole
-- `Traversable` before it can produce a `Just`.
--
-- >>> zipWithMay (+) [1, 2] (Just 10)
-- Just (Just 11)
--
-- >>> zipWithMay (+) [] (Just 10)
-- Just Nothing
--
-- >>> zipWithMay (+) [] Nothing
-- Nothing
zipWithMay :: Traversable t => (a -> b -> c) -> [a] -> t b -> t (Maybe c)
zipWithMay f = zipWith (\ma b -> f <$> ma <*> pure b)

-- | Use a given function to zip a list with any `Traversable`, maintaining the
-- shape of the latter.
--
-- If the list is too short, throw an error with a custom error string.
--
-- >>> zipWithNote "oops" (+) [1, 2] (Just 10)
-- Just 11
--
-- >>> zipWithNote "oops" (+) [] (Just 10)
-- Just *** Exception: oops
-- CallStack (from HasCallStack):
--   zipWithNote, called at ...
--
-- >>> zipWithNote "oops" (+) [] Nothing
-- Nothing
zipWithNote
  :: (HasCallStack, Traversable t)
  => String -> (a -> b -> c) -> [a] -> t b -> t c
zipWithNote errStr f =
  withFrozenCallStack $ zipWith $ \ma b -> f (fromMaybe (error errStr) ma) b

-- $enumeration
--
-- If all you want is to number off elements starting from @0@, these functions
-- are convenient.

-- | Pair any `Traversable` with the `Int`s starting from @0@.
--
-- >>> enumerate (Just ())
-- Just (0,())
--
-- >>> enumerate "abc"
-- [(0,'a'),(1,'b'),(2,'c')]
enumerate :: Traversable t => t a -> t (Int, a)
enumerate = zipErr [0..]

-- | Use a given function to pair any `Traversable` with the `Int`s starting
-- from @0@.
--
-- >>> enumerateWith (+) (Just 3)
-- Just 3
--
-- >>> enumerateWith replicate "abc"
-- ["","b","cc"]
enumerateWith :: Traversable t => (Int -> a -> b) -> t a -> t b
enumerateWith f = zipWithErr f [0..]

-- $holes
--
-- The "holes" in a `Traversable` structure are the positions that hold
-- traversable elements. For example, if you call `Foldable.toList`, the result
-- contains all the values that were in holes; if you `fmap`, the values in
-- holes get updated, while everything else stays fixed.
--
-- We can use `Compose` to combine the holes of two `Traversable`s. If we have
-- @xs :: f (g a)@ where @f@ and @g@ are both `Traversable`, then the holes of
-- @xs@ are given by the @`Traversable` f@ instance, and have type @g a@. The
-- holes of @v`Compose` xs :: `Compose` f g a@ are the holes of the @g@s that
-- are themselves in the holes of the @f@, and have type @a@.
--
-- For example, the holes in these data structures are bolded:
--
-- @
-- -- Compose [] with `Maybe`:
--         [__Just 1__, __Nothing__, __Just 2__, __Nothing__] :: [Maybe Int]
-- Compose [Just __1__, Nothing, Just __2__, Nothing] :: Compose [] Maybe Int
--
-- -- Compose ((,) a) with []:
--         (True, __[1, 2, 3]__) :: (Bool, [Int])
-- Compose (True, [__1__, __2__, __3__]) :: Compose ((,) Bool) [] Int
--
-- -- Compose (`Either` a) with []:
--          Left  [1, 2]  :: Either [Int] [Int]
--          Right __[1, 2]__  :: Either [Int] [Int]
-- Compose (Left  [1, 2]) :: Compose (Either [Int]) [] Int
-- Compose (Right [__1__, __2__]) :: Compose (Either [Int]) [] Int
--
-- -- Nested `Compose`:
--          Compose [Just __(Just 1)__, Just __Nothing__, Nothing]  :: Compose [] Maybe (Maybe Int)
-- Compose (Compose [Just (Just __1__), Just Nothing, Nothing]) :: Compose (Compose [] Maybe) Maybe Int
-- @
--
-- When zipping, holes are relevant because every hole gets paired with exactly
-- one list element. You can use `setHoles` and `unsetHoles` to control the
-- pairing-off.
--
-- With @`setHoles` f x@, @f@ gets called once for every hole in @x@; and every
-- hole returned by every call to @f@ will be a hole. So if @f@ returns...
--
-- * ... @`Maybe` a@, then only the `Just` values will be holes.
-- * ... @`Either` a b@, then only the `Right` values will be holes.
-- * ... @(a, b)@, then only the `snd` values will be holes.
-- * ... @[a]@, then every list element will be a hole.
--
-- And @`unsetHoles` g x@ acts as an inverse; @g@ gets called on parts of @x@
-- that may have any number of holes, and whatever @g@ returns will become a
-- single hole in its own right.
--
-- >>> let xs = [Just 10, Nothing, Just 20, Nothing]
-- >>> enumerate xs
-- [(0,Just 10),(1,Nothing),(2,Just 20),(3,Nothing)]
-- >>> -- Only enumerate the `Just` values:
-- >>> unsetHoles id $ enumerate $ setHoles id xs
-- [Just (0,10),Nothing,Just (1,20),Nothing]
--
-- >>> let xs = [11,22..66]
-- >>> zipWithErr (+) [1..] xs
-- [12,24,36,48,60,72]
-- >>> -- Only add to the even numbers:
-- >>> :{
--   unsetHoles (either id id)
--     $ zipWithErr (+) [1..]
--     $ setHoles (\x -> if even x then Right x else Left x)
--     $ xs
-- :}
-- [11,23,33,46,55,69]

-- | Set the holes in a `Traversable` by wrapping in `Compose`.
--
-- @
-- setHoles f x = `Compose` (`fmap` f x)
-- @
setHoles :: Functor f => (a -> g b) -> f a -> Compose f g b
setHoles f = Compose . fmap f

-- | Simplify the holes in a `Traversable` by unwrapping from `Compose`.
--
-- @
-- unsetHoles f x = `fmap` f (`getCompose` x)
-- @
unsetHoles :: Functor f => (g a -> b) -> Compose f g a -> f b
unsetHoles f = fmap f . getCompose

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

-- == __Boring list of more examples__
--

-- $traversable
--
-- A `Traversable` structure @x :: t a@ can be decomposed into
--
-- * its __spine:__ @`const` () `<$>` x :: t ()@
-- * its __element list:__ @`Data.Foldable.toList` x :: [a]@
--
-- It can then be reconstructed from these. Or, given another list @[b]@ of the
-- same length, we can use the original spine to create a @t b@ with these new
-- elements, of the same shape as the original.
--
-- @`zipWithErr` `const`@ is the function that reconstructs it. That is,
--
-- @
-- `zipWithErr` `const` (elementList x) (spine x) === x
-- @
--
-- allows you to exactly recreate the original structure, or to replace its
-- element list by providing a different one.
