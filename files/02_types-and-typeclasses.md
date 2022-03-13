Chapter **Types and Typeclasses** introduces us to Haskell's type system.<br/>
Source: http://learnyouahaskell.com/types-and-typeclasses<br/>

Functions defined in this chapter can be found in [02_types.hs](02_types.hs)

## Summary
### Types
- `Int` stands for integer. It's used for whole numbers. 7 can be an `Int` but 7.2 cannot. `Int` is bounded, which means that it has a minimum and a maximum value. Usually on 32-bit machines the maximum possible `Int` is 2147483647 and the minimum is -2147483648.
- `Integer` stands for, er … also integer. The main difference is that it's not bounded so it can be used to represent really really big numbers. I mean like really big. `Int`, however, is more efficient.
- `Float` is a real floating point with single precision.
- `Double` is a real floating point with double the precision!
- `Bool` is a boolean type. It can have only two values: True and False.
- `Char` represents a character. It's denoted by single quotes. A list of characters is a string.
- `Tuples` are types but they are dependent on their length as well as the types of their components, so there is theoretically an infinite number of tuple types, which is too many to cover in this tutorial.</br>
</br>
**NOTE**: the empty tuple () is also a type which can only have a single value: ()
```
    gchi> :t ()
    () :: ()
```

### Typeclasses
- `Eq` is used for types that support equality testing.
- `Ord` is for types that have an ordering.
- `Show` typeclass' members can be presented as strings.
- `Read` is sort of the opposite typeclass of `Show`.
- `Enum` members are sequentially ordered types — they can be enumerated.
- `Bounded` members have an upper and a lower bound.
- `Num` is a numeric typeclass. Its members have the property of being able to act like numbers.
- `Integral` is also a numeric typeclass. `Num` includes all numbers, including real numbers and integral numbers, `Integral` includes only integral (whole) numbers. In this typeclass are `Int` and `Integer`.
- `Floating` includes only floating point numbers, so `Float` and `Double`.

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

##### Floating typeclass
Includes `Float` and `Double` types

##### Integral typeclass
```
gchi> :t fromIntegral
fromIntegral :: (Integral a, Num b) => a -> b
gchi> :t length
length :: Foldable t => t a -> Int
ghci> fromIntegral (length [1,2,3,4]) + 3.2
7.2
```

[Home](README.md)

