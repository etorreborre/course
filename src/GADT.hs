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
	KeyS :: String -> Key String
	KeyI :: String -> Key Int
	KeyB :: String -> Key Bool



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

flag :: String -> Key Bool
flag =
  KeyB

counter :: String -> Key Int
counter =
  KeyI

name :: String -> Key String
name =
  KeyS

stuff :: Bag
stuff =
  [Entry (flag "f")  True, Entry (counter "C") 10, Entry (name "n") "name"]

get :: Key a -> Bag -> Maybe a
get _ [] = Nothing
get k1 (Entry k2 v : rest) = case (k1, k2) of
	(KeyB n1, KeyB n2) -> if n1 == n2 then Just v else get k1 rest
	(KeyI n1, KeyI n2) -> if n1 == n2 then Just v else get k1 rest
	(KeyS n1, KeyS n2) -> if n1 == n2 then Just v else get k1 rest
	_            -> get k1 rest
