{-# LANGUAGE QuantifiedConstraints #-}

module TypedWasm.Definition.List where

import Data.Kind (Constraint, Type)
import Data.Monoid (Sum (..))
import Data.Proxy
import TypedWasm.Definition.Types

-- | A heterogenous list.
data HList (f :: k -> Type) (xs :: [k]) where
    HEmpty :: HList f '[]
    HCons :: f x -> HList f xs -> HList f (x ': xs)

deriving instance (forall x. Show (f x)) => Show (HList f xs)
deriving instance (forall x. Eq (f x)) => Eq (HList f xs)

{-# COMPLETE (:*:) #-}

-- | An infix definition of `HCons`
pattern (:*:) :: f x -> HList f xs -> HList f (x ': xs)
pattern (:*:) x xs = HCons x xs

infixr 5 :*:

{-# COMPLETE HSingle #-}

-- | A pattern to match a `HList` with a single element
pattern HSingle :: f x -> HList f '[x]
pattern HSingle x = HCons x HEmpty

{-# COMPLETE (:*) #-}

{- | Match an `HList` with two elements.

This can be combined with `:*:` to write terse matches of `HList`s
-}
pattern (:*) :: f x -> f y -> HList f '[x, y]
pattern (:*) x y = x :*: HSingle y

infixr 6 :*

-- | Concat two type level lists
type family ConcatList xs ys where
    ConcatList '[] ys = ys
    ConcatList (x ': xs) ys = x ': ConcatList xs ys

-- | Concat two `HList`s
concatHList :: HList f xs -> HList f ys -> HList f (ConcatList xs ys)
concatHList HEmpty other = other
concatHList (HCons x xs) ys = HCons x $ concatHList xs ys

-- | Check that all members of `xs` satsify `c`
class SingList (c :: k -> Constraint) (xs :: [k]) where
    -- | Generate an `HList` by calling the given polymorphic term for each element in `xs`.
    --
    -- GHC cannot infer @c@ correctly, so we must provide in a `Proxy`
    singList :: Proxy c -> (forall x. (c x) => f x) -> HList f xs

instance SingList c '[] where
    singList _ _ = HEmpty

instance (c x, SingList c xs) => SingList c (x ': xs) where
    singList prox x = HCons x $ singList prox x

singValueTypeList :: (SingList SingValueType xs) => HList SValueType xs
singValueTypeList = singList (Proxy @SingValueType) singValueType

{- | Map over the elements of a `HList`. The `Int` in the callback is the
position of this element in the list
-}
indexMapHList ::
    forall f g xs.
    (forall x. Int -> f x -> g x) ->
    HList f xs ->
    HList g xs
indexMapHList = helper 0
  where
    helper :: Int -> (forall x. Int -> f x -> g x) -> HList f xs' -> HList g xs'
    helper _ _ HEmpty = HEmpty
    helper n f (HCons x xs) = HCons (f n x) (helper (succ n) f xs)

-- | Fold over a `HList` by mapping each element to a `Monoid`
foldHList :: (Monoid m) => (forall x. f x -> m) -> HList f xs -> m
foldHList _ HEmpty = mempty
foldHList f (HCons x xs) = f x <> foldHList f xs

-- | Calculate the number of items in a `HList`
countHList :: HList f xs -> Int
countHList = getSum . foldHList (const $ Sum 1)

class DifferenceList (big :: [k]) (small :: [k]) where
    type RemainingList big small :: [k]

instance DifferenceList big '[] where
    type RemainingList big '[] = big

instance (b ~ s, DifferenceList big small) => DifferenceList (b ': big) (s ': small) where
    type RemainingList (b ': big) (s ': small) = RemainingList big small
