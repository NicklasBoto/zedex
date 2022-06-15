# zedex

```hs
-- define a datatype
Bool := F | T

-- define boolean negation
not : Bool -> Bool
  | F => T
  | T => F

-- one could also define this with rotations
not : Bool -> Bool
  := X(Pi) b => b

-- or both
X(Pi),
not : Bool -> Bool
  | F => T
  | T => F

-- using let
foo := let x = 2
         in x + 1

-- using where
foo := x
  where x = 2

-- scope injection
bar := x=3 in foo

-- inductive datatypes
List a := Nil | Cons a (List a) 

-- typeclasses
typeclass Functor f implements
  map : (a -> b) -> f a -> f b

proof Functor List where
  map f | Nil       => Nil
        | Cons x xs => Cons (f x) (map f xs)

l : List Bool
  := Cons T (Cons F Nil)

l' := map (X(Pi) b => b) l


-- define a "flipped" map function
for : Functor f |- f a -> (a -> b) -> f b
  xs f := map f xs
```
