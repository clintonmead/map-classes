module Control.Class.Impl.Test where

data T f t

f1 :: T f t
f1 = f2

f2 :: f t
f2 = undefined
