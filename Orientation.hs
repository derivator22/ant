{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeFamilies, GADTs #-}

module Orientation where

data Command :: * -> *

data CardinalPoints :: * -> *

data L = L
data R
data N
data U

data North = North
data South
data West
data East

data Z
data S i

type family Orientation a b = r | a -> b   
type instance Orientation L North = West
type instance Orientation L South = East
type instance Orientation L West  = South
type instance Orientation L East  = North
type instance Orientation R North = East
type instance Orientation R South = West
type instance Orientation R West  = North
type instance Orientation R East  = South

{-
type instance Orientation N a     = a
type instance Orientation U North = South
type instance Orientation U South = North
type instance Orientation U West  = East
type instance Orientation U East  = West
-}


type family G a b c = foo | foo -> a b 
type instance G Int  Char Bool = Bool
type instance G Int  Char Int  = Bool
type instance G Bool Int  Int  = Int


  
