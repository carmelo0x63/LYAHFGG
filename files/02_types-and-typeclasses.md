Chapter **Types and Typeclasses** introduces us to Haskell's type system.<br/>
Source: http://learnyouahaskell.com/types-and-typeclasses<br/>

Functions defined in this chapter can be found in [02_types.hs](files/02_types.hs)

#### Believe the type
```
ghci> :t 'a'
'a' :: Char
ghci> :t True
True :: Bool
ghci> :t "HELLO!"
"HELLO!" :: [Char]
ghci> :t (True, 'a')
(True, 'a') :: (Bool, Char)
ghci> :t 4 == 5
4 == 5 :: Bool
```
```
removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']] 
```
```
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z
```

##### Int/Integer
```
factorial :: Integer -> Integer
factorial n = product [1..n]
```
```
ghci> factorial 50
30414093201713378043612608166064768844377641568960512000000000000
```

##### Float
```
circumference :: Float -> Float
circumference r = 2 * pi * r
```
```
ghci> circumference 4.0
25.132742
```

##### Double
```
circumference' :: Double -> Double
circumference' r = 2 * pi * r
```
```
ghci> circumference' 4.0
25.132741228718345
```

##### Bool
```
gchi> :t True
True :: Bool
```

##### Char
```
gchi> :t 'a'
'a' :: Char

gchi> :t "char"
"char" :: [Char]
```

##### Tuples
Tuples' type depends on both the length and the types of their components. One special case is the `Unit` which is the empty tuple.
```
gchi> :t "char"
"char" :: [Char]

gchi> :t (1,2)
(1,2) :: (Num t1, Num t) => (t, t1)

gchi> :t (1,'a')
(1,'a') :: Num t => (t, Char)

gchi> :t ()
() :: ()
```

#### Type variables
```
ghci> :t head
head :: [a] -> a
```
```
ghci> :t fst
fst :: (a, b) -> a
```

#### Typeclasses 101
```
ghci> :t (==)
(==) :: (Eq a) => a -> a -> Bool

gchi> :t elem
elem :: (Foldable t, Eq a) => a -> t a -> Bool
```

##### Eq typeclass
```
ghci> 5 == 5
True
ghci> 5 /= 5
False
ghci> 'a' == 'a'
True
ghci> "Ho Ho" == "Ho Ho"
True
ghci> 3.432 == 3.432
True
```

##### Ord typeclass
```
ghci> :t (>)
(>) :: (Ord a) => a -> a -> Bool
```
```
ghci> "Abrakadabra" < "Zebra"
True
ghci> "Abrakadabra" `compare` "Zebra"
LT
ghci> 5 >= 2
True
ghci> 5 `compare` 3
GT
```

##### Show typeclass
```
ghci> show 3
"3"
ghci> show 5.334
"5.334"
ghci> show True
"True"
```

##### Read typeclass
```
ghci> read "True" || False
True
ghci> read "8.2" + 3.8
12.0
ghci> read "5" - 2
3
ghci> read "[1,2,3,4]" ++ [3]
[1,2,3,4,3]
```
```
ghci> read "4"
<interactive>:1:0:
    Ambiguous type variable `a' in the constraint:
      `Read a' arising from a use of `read' at <interactive>:1:0-7
    Probable fix: add a type signature that fixes these type variable(s)
```
```
ghci> :t read
read :: (Read a) => String -> a
```
```
ghci> read "5" :: Int
5
ghci> read "5" :: Float
5.0
ghci> (read "5" :: Float) * 4
20.0
ghci> read "[1,2,3,4]" :: [Int]
[1,2,3,4]
ghci> read "(3, 'a')" :: (Int, Char)
(3, 'a')
```

##### Enum typeclass
```
ghci> ['a'..'e']
"abcde"
ghci> [LT .. GT]
[LT,EQ,GT]
ghci> [3 .. 5]
[3,4,5]
ghci> succ 'B'
'C'
```

##### Bounded typeclass
```
ghci> minBound :: Int
-2147483648
ghci> maxBound :: Char
'\1114111'
ghci> maxBound :: Bool
True
ghci> minBound :: Bool
False
```
```
ghci> maxBound :: (Bool, Int, Char)
(True,2147483647,'\1114111')
```

##### Num typeclass
```
ghci> :t 20
20 :: (Num t) => t
```
```
ghci> 20 :: Int
20
ghci> 20 :: Integer
20
ghci> 20 :: Float
20.0
ghci> 20 :: Double
20.0
```
```
ghci> :t (*)
(*) :: (Num a) => a -> a -> a
```

##### Integral typeclass

##### Floating typeclass
```
gchi> :t fromIntegral
fromIntegral :: (Num b, Integral a) => a -> b
```
