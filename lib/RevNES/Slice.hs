{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module RevNES.Slice
	( Slice, start, end
	-- | Conveniences
	, Word16, Slice16, SliceU, BV
	-- | Creation
	, full
	, fromStartEnd
	, fromStartSize, fromStartSizeB, fromStartSizeBU
	, fromStartSizem1, fromStartSizem1B, fromStartSizem1U
	, fromEndSize, fromEndSizeB, fromEndSizeBU
	, fromEndSizem1, fromEndSizem1B, fromEndSizem1U
	, singleton
	, u, b, bB
	-- | Size
	, size, sizeU, sizem1, sizem116, sizem1U
	-- | There are three statistics: the 'start', the 'end', and the 'size' (or
	-- 'sizem1'). Below we give modifiers that set one, keep one, and compute
	-- one. The naming scheme is @xFromY@, where @x@ is the computed one, @y@
	-- is given as a lone input, and the remaining one is kept from the slice
	-- input.
	--
	-- Suffix @B@ (for \"bounded\") avoids returning 'Maybe's by saturating the
	-- computed one in case of overflow. Suffix @U@ incurs fewer checks
	-- (sometimes even avoiding returning 'Maybe') by using types that can't
	-- overflow.
	, startFromEnd, startFromEndB, startFromEndU
	, startFromSize, startFromSizeU, startFromSizem1, startFromSizem1B, startFromSizem1U
	, startFromMaxSize, startFromMaxSizeB
	, endFromStart, endFromStartB, endFromStartU
	, endFromSize, endFromSizeB, endFromSizeU, endFromSizem1, endFromSizem1B, endFromSizem1U
	, endFromMaxSize, endFromMaxSizeB
	, sizeFromStart, sizeFromStartB, sizeFromStartU
	, sizeFromEnd, sizeFromEndB, sizeFromEndU
	-- | Containment
	, index
	) where

import Control.Monad
import Data.Int -- for haddock
import Data.IntervalMap.Generic.Interval
import Data.Word
import Numeric.Natural

-- Invariant: _start <= _end.
-- Both _start and _end are included in the range described by the slice.
data Slice a = Slice { _start, _end :: a }
	deriving (Eq, Ord, Read, Show)

valid :: Ord a => Slice a -> Bool
valid s = _end s >= _start s

{-# INLINE start #-}
{-# INLINE end #-}
-- So that users don't get the record update functions.
-- | Same as 'lowerBound', though very slightly less constrained.
start :: Slice a -> a
-- | Same as 'upperBound', though very slightly less constrained.
end :: Slice a -> a
start = _start
end = _end

instance Ord a => Interval (Slice a) a where
	{-# INLINE lowerBound #-}
	lowerBound = _start
	{-# INLINE upperBound #-}
	upperBound = _end
	{-# INLINE leftClosed #-}
	leftClosed _ = True
	{-# INLINE rightClosed #-}
	rightClosed _ = True
	{-# INLINE before #-}
	before a b = _start a < _end b
	{-# INLINE subsumes #-}
	subsumes a b = _start a <= _start b && _end a >= _end b
	{-# INLINE overlaps #-}
	overlaps a b = _start a <= _end b && _start b <= _end b
	{-# INLINE below #-}
	below p s = p < _start s
	{-# INLINE above #-}
	above p s = p > _end s
	{-# INLINE inside #-}
	inside p s = _start s <= p && p <= _end s
	{-# INLINE isEmpty #-}
	isEmpty _ = False
	{-# INLINE compareUpperBounds #-}
	compareUpperBounds a b = compare (_end a) (_end b)

type Slice16 = Slice Word16
-- | \"U\" for \"unbounded\".
type SliceU  = Slice Integer
-- | Bitvector-like types ('Word16', 'Int64', etc.)
type BV a = (Bounded a, Num a, Ord a, Integral a {- TODO: remove once the *From* update functions below are made more efficient -})

ensure :: (a -> Bool) -> a -> Maybe a
ensure p x = x <$ guard (p x)

full :: Bounded a => Slice a
full = Slice minBound maxBound

fromStartEnd :: (Num a, Ord a) => a -> a -> Maybe (Slice a)
fromStartEnd start' end' = ensure valid $ Slice start' end'

fromStartSize :: (Num a, Ord a) => a -> a -> Maybe (Slice a)
fromStartSize start' size' = guard (size' > 0) >> fromStartEnd start' (start' + size' - 1)

fromStartSizeB :: BV a => a -> a -> Slice a
fromStartSizeB start' size'
	| size' < 1 = singleton start'
	| end' < start' = Slice start' maxBound
	| otherwise = Slice start' end'
	where end' = start' + size' - 1

fromStartSizeBU :: Integer -> Integer -> SliceU
fromStartSizeBU start' size'
	| size' < 1 = singleton start'
	| otherwise = Slice start' (start' + size' - 1)

fromStartSizem1 :: (Num a, Ord a) => a -> a -> Maybe (Slice a)
fromStartSizem1 start' sizem1' = guard (sizem1' >= 0) >> fromStartEnd start' (start' + sizem1')

fromStartSizem1B :: BV a => a -> a -> Slice a
fromStartSizem1B start' sizem1'
	| sizem1' < 0 = singleton start'
	| end' < start' = Slice start' maxBound
	| otherwise = Slice start' end'
	where end' = start' + sizem1'

fromStartSizem1U :: Integer -> Natural -> SliceU
fromStartSizem1U start' sizem1' = Slice start' (start' + toInteger sizem1')

fromEndSize :: (Num a, Ord a) => a -> a -> Maybe (Slice a)
fromEndSize end' size' = guard (size' > 0) >> fromStartEnd (end' - size' + 1) end'

fromEndSizeB :: BV a => a -> a -> Slice a
fromEndSizeB end' size'
	| size' < 1 = singleton end'
	| end' < start' = Slice minBound end'
	| otherwise = Slice start' end'
	where start' = end' - size' + 1

fromEndSizeBU :: Integer -> Integer -> SliceU
fromEndSizeBU end' size'
	| size' < 1 = singleton end'
	| otherwise = Slice (end' - size' + 1) end'

fromEndSizem1 :: (Num a, Ord a) => a -> a -> Maybe (Slice a)
fromEndSizem1 end' sizem1' = guard (sizem1' >= 0) >> fromStartEnd (end' - sizem1') end'

fromEndSizem1B end' sizem1'
	| sizem1' < 0 = singleton end'
	| end' < start' = Slice minBound end'
	| otherwise = Slice start' end'
	where start' = end' - sizem1'

fromEndSizem1U :: Integer -> Natural -> SliceU
fromEndSizem1U end' sizem1' = Slice (end' - toInteger sizem1') end'

singleton :: a -> Slice a
singleton v = Slice v v

u :: Integral a => Slice a -> SliceU
u s = Slice (toInteger (_start s)) (toInteger (_end s))

b :: forall a. (BV a, Integral a) => SliceU -> Maybe (Slice a)
b s = do
	guard (_start s >= toInteger (minBound :: a) && _end s <= toInteger (maxBound :: a))
	Just (Slice (fromInteger (_start s)) (fromInteger (_end s)))

bB :: forall a. (BV a, Integral a) => SliceU -> Slice a
bB s = Slice (clip (_start s)) (clip (_end s)) where
	clip v | v < toInteger (minBound :: a) = minBound
	       | v > toInteger (maxBound :: a) = maxBound
	       | otherwise = fromInteger v

unsafeSizem1 :: Num a => Slice a -> a
unsafeSizem1 s = _end s - _start s

size :: (Ord a, Num a) => Slice a -> Maybe a
size = ensure (>0) . (1+) . unsafeSizem1

sizeU :: SliceU -> Integer
sizeU = (1+) . unsafeSizem1

-- | Size minus 1. (Very) slightly cheaper than 'size'.
sizem1 :: (Ord a, Num a) => Slice a -> Maybe a
sizem1 = ensure (>=0) . unsafeSizem1

sizem116 :: Slice16 -> Word16
sizem116 = unsafeSizem1

sizem1U :: SliceU -> Natural
sizem1U = fromInteger . unsafeSizem1

startFromEnd      :: BV a => a -> Slice a -> Maybe (Slice a)
startFromEndB     :: BV a => a -> Slice a -> Slice a
startFromEndU     :: Integer -> SliceU -> SliceU
startFromSize     :: BV a => a -> Slice a -> Maybe (Slice a)
startFromSizeB    :: BV a => a -> Slice a -> Slice a
startFromSizeU    :: Integer -> SliceU -> Maybe SliceU
startFromSizem1   :: BV a => a -> Slice a -> Maybe (Slice a)
startFromSizem1B  :: BV a => a -> Slice a -> Slice a
startFromSizem1U  :: Natural -> SliceU -> SliceU
startFromMaxSize  :: (Ord a, Num a) => a -> Slice a -> Maybe (Slice a)
startFromMaxSizeB :: (Ord a, Num a) => a -> Slice a -> Slice a
endFromStart      :: BV a => a -> Slice a -> Maybe (Slice a)
endFromStartB     :: BV a => a -> Slice a -> Slice a
endFromStartU     :: Integer -> SliceU -> SliceU
endFromSize       :: BV a => a -> Slice a -> Maybe (Slice a)
endFromSizeB      :: BV a => a -> Slice a -> Slice a
endFromSizeU      :: Integer -> SliceU -> Maybe SliceU
endFromSizem1     :: BV a => a -> Slice a -> Maybe (Slice a)
endFromSizem1B    :: BV a => a -> Slice a -> Slice a
endFromSizem1U    :: Natural -> SliceU -> SliceU
endFromMaxSize    :: (Ord a, Num a) => a -> Slice a -> Maybe (Slice a)
endFromMaxSizeB   :: (Ord a, Num a) => a -> Slice a -> Slice a
sizeFromStart     :: Ord a => a -> Slice a -> Maybe (Slice a)
sizeFromStartB    :: Ord a => a -> Slice a -> Slice a
sizeFromStartU    :: Integer -> SliceU -> Maybe SliceU
sizeFromEnd       :: Ord a => a -> Slice a -> Maybe (Slice a)
sizeFromEndB      :: Ord a => a -> Slice a -> Slice a
sizeFromEndU      :: Integer -> SliceU -> Maybe SliceU

-- TODO: These can probably be made more efficient by eliding
-- true-by-construction bound checks and avoiding casts to Integer. The
-- implementations below are obviously correct and very quick to write, and
-- that's the only reason they are what they are.
--
-- When this change is done, don't forget to remove Integral from BV.
startFromEnd  end' = b  . startFromEndU (toInteger end') . u
startFromEndB end' = bB . startFromEndU (toInteger end') . u
startFromEndU end' s = Slice (_start s + end' - _end s) end'

startFromSize  size' = b <=< startFromSizeU (toInteger size') . u
startFromSizeB size' | size' < 1 = \s -> s { _start = _end s }
                     | otherwise = bB . startFromSizem1U (fromIntegral size' - 1) . u
startFromSizeU size' s = ensure valid s { _start = _end s - size' + 1 }

startFromSizem1  sizem1' | sizem1' < 0 = const Nothing
                         | otherwise = b  . startFromSizem1U (fromIntegral sizem1') . u
startFromSizem1B sizem1' | sizem1' < 0 = \s -> s { _start = _end s }
                         | otherwise = bB . startFromSizem1U (fromIntegral sizem1') . u
startFromSizem1U sizem1' s = s { _start = _end s - toInteger sizem1' }

startFromMaxSize size' s = do
	guard (size' > 0)
	Just $ if start' <= _end s && start' > _start s
	then s { _start = start' }
	else s
	where start' = _end s - size' + 1

startFromMaxSizeB size' s | size' <= 0 || start' > _end s = s
                          | otherwise = s { _start = start' }
                          where start' = _end s - size' + 1

endFromStart  start' = b  . endFromStartU (toInteger start') . u
endFromStartB start' = bB . endFromStartU (toInteger start') . u
endFromStartU start' s = Slice start' (_end s - _start s + start')

endFromSize  size' = b <=< endFromSizeU (toInteger size') . u
endFromSizeB size' | size' < 1 = \s -> s { _end = _start s }
                   | otherwise = bB . endFromSizem1U (fromIntegral size' - 1) . u
endFromSizeU size' s = ensure valid s { _end = _start s + size' - 1 }

endFromSizem1  sizem1' | sizem1' < 0 = const Nothing
                       | otherwise = b  . endFromSizem1U (fromIntegral sizem1') . u
endFromSizem1B sizem1' | sizem1' < 0 = \s -> s { _end = _start s }
                       | otherwise = bB . endFromSizem1U (fromIntegral sizem1') . u
endFromSizem1U sizem1' s = s { _end = _start s + toInteger sizem1' }

endFromMaxSize size' s = do
	guard (size' > 0)
	Just $ if end' >= _start s && end' < _end s
	then s { _end = end' }
	else s
	where end' = _start s + size' - 1

endFromMaxSizeB size' s | size' <= 0 || end' < _start s = s
                        | otherwise = s { _end = end' }
                        where end' = _start s + size' - 1

sizeFromStart  start' s = ensure valid s { _start = start' }
sizeFromStartB start' s = s { _start = min start' (_end s) }
sizeFromStartU start' s = ensure valid s { _start = start' }
sizeFromEnd  end' s = ensure valid s { _end = end' }
sizeFromEndB end' s = s { _end = max end' (_start s) }
sizeFromEndU end' s = ensure valid s { _end = end' }

index :: (Num a, Ord a) => a -> Slice a -> Maybe a
index v s = do
	guard (inside v s)
	ensure (0<=) (v - _start s)
