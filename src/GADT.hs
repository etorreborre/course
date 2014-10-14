{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}

module GADT where

import Prelude

{-
  Lets start ^^ by enabling some language extensions. GADTs to encode our data types and
  Rank2Types for the existental we will need to hide our types when we don't care about
  the invariant.
 -}


{-
  Lets start with our key data type, we want to be able to represent
  keys that point to a String, an Int or a Bool.
 -}
data Key a where
	KeyS :: Key String
	KeyI :: Key Int
	KeyB :: Key Bool



{-
  Lets than create an "entry in a bag" that holds any well typed
  key-value pair. Note well that we explicitly want to hide the
  invariants that our keys maintain at this point.
 -}
data Entry =
  forall a. Entry (Key a) a

{-
  A simple representation of a Bag as a list of entries.
 -}
type Bag =
  [Entry]

flag :: Key Bool
flag =
  KeyB

counter :: Key Int
counter =
  KeyI

name :: Key String
name =
  KeyS

stuff :: Bag
stuff =
  [Entry flag True, Entry counter 10, Entry name "name"]

get :: Key a -> Bag -> Maybe a
get _ [] = Nothing
get k1 (Entry k2 v : rest) = case (k1, k2) of
	(KeyB, KeyB) -> Just v
	(KeyI, KeyI) -> Just v
	(KeyS, KeyS) -> Just v
	_            -> get k1 rest
