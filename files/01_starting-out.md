Chapter **Starting Out**, shows several examples along with their results. Here all the inputs can be found ready to be typed in.<br/>
If you please, they're also safe to copy and paste into GHCi, results will be printed out right away.<br/>
Source: http://learnyouahaskell.com/starting-out<br/>

Functions defined in this chapter can be found in [01_starting.hs](01_starting.hs)

#### Ready, set, go!
```
$ ghci
GHCi, version 8.4.4: http://www.haskell.org/ghc/  :? for help
Prelude>
```

Prompt is set to `Prelude>` by default. If one whishes to permanently change it they can do so by saving a file named `.ghci` in their home directory with the following contents:</br>
```
:set prompt "gchi> "
```

#### Simple arithmetics
```
ghci> 2 + 15
17
ghci> 49 * 100
4900
ghci> 1892 - 1472
420
ghci> 5 / 2
2.5
ghci>
```

#### Parentheses and precedence
```
ghci> (50 * 100) - 4999
1
ghci> 50 * 100 - 4999
1
ghci> 50 * (100 - 4999)
-244950
```

#### Boolean logic
```
ghci> True && False
False
ghci> True && True
True
ghci> False || True
True 
ghci> not False
True
ghci> not (True && True)
False
```

#### (in)Equalities
```
ghci> 5 == 5
True
ghci> 1 == 0
False
ghci> 5 /= 5
False
ghci> 5 /= 4
True
ghci> "hello" == "hello"
True
```

#### Type error
```
gchi> 5 + "llama"

<interactive>:1:1: error:
    • No instance for (Num [Char]) arising from a use of ‘+’
    • In the expression: 5 + "llama"
      In an equation for ‘it’: it = 5 + "llama"
```

#### Orderable types
```
ghci> succ 8
9
ghci> min 9 10
9
ghci> min 3.4 3.2
3.2
ghci> max 100 101
101
ghci> succ 9 + max 5 4 + 1
16
ghci> (succ 9) + (max 5 4) + 1
16
gchi> succ 9 * 10
100
gchi> succ (9 * 10)
91
```

#### Prefix or infix functions
```
gchi> div 92 10
9
gchi> 92 `div` 10
9
```

#### Baby's first function
See [01_starting.hs](01_starting.hs)

```
doubleMe x = x + x
```

```
ghci> :l files/01_starting.hs
[1 of 1] Compiling Main             ( files/01_starting.hs, interpreted )
Ok, modules loaded: Main.
ghci> doubleMe 9
18
ghci> doubleMe 8.3
16.6
```

```
doubleUs x y = x*2 + y*2
```

```
ghci> doubleUs 4 9
26
ghci> doubleUs 2.3 34.2
73.0
ghci> doubleUs 28 88 + doubleMe 123
478
```

```
doubleUs x y = doubleMe x + doubleMe y
```

```
doubleSmallNumber x = if x > 100
                        then x
                        else x*2
```

```
doubleSmallNumber' x = (if x > 100 then x else x*2) + 1
```

```
conanO'Brien = "It's a-me, Conan O'Brien!"
```

**NOTE**: it's possible to display info about loaded modules and their contents like so...
```
gchi> :show modules
Main             ( files/01_starting.hs, interpreted )

gchi> :browse Main
doubleMe :: Num a => a -> a
doubleUs :: Num a => a -> a -> a
doubleSmallNumber :: (Ord a, Num a) => a -> a
doubleSmallNumber' :: (Num a, Ord a) => a -> a
conanO'Brien :: [Char]
```

#### An intro to lists
```
ghci> let lostNumbers = [4,8,15,16,23,42]
ghci> lostNumbers
[4,8,15,16,23,42]
```

```
ghci> [1,2,3,4] ++ [9,10,11,12]
[1,2,3,4,9,10,11,12]
ghci> "hello" ++ " " ++ "world"
"hello world"
ghci> ['w','o'] ++ ['o','t']
"woot"
```

```
ghci> 'A':" SMALL CAT"
"A SMALL CAT"
ghci> 5:[1,2,3,4,5]
[5,1,2,3,4,5]
```

```
ghci> "Steve Buscemi" !! 6
'B'
ghci> [9.4,33.2,96.2,11.2,23.25] !! 1
33.2
```

```
ghci> let b = [[1,2,3,4],[5,3,3,3],[1,2,2,3,4],[1,2,3]]
ghci> b
[[1,2,3,4],[5,3,3,3],[1,2,2,3,4],[1,2,3]]
ghci> b ++ [[1,1,1,1]]
[[1,2,3,4],[5,3,3,3],[1,2,2,3,4],[1,2,3],[1,1,1,1]]
ghci> [6,6,6]:b
[[6,6,6],[1,2,3,4],[5,3,3,3],[1,2,2,3,4],[1,2,3]]
ghci> b !! 2
[1,2,2,3,4]
```

```
ghci> [3,2,1] > [2,1,0]
True
ghci> [3,2,1] > [2,10,100]
True
ghci> [3,4,2] > [3,4]
True
ghci> [3,4,2] > [2,4]
True
ghci> [3,4,2] == [3,4,2]
True
```

```
ghci> head [5,4,3,2,1]
5
```

```
ghci> tail [5,4,3,2,1]
[4,3,2,1]
```

```
ghci> last [5,4,3,2,1]
1
```

```
ghci> init [5,4,3,2,1]
[5,4,3,2]
```

```
ghci> head []
*** Exception: Prelude.head: empty list
```

```
ghci> length [5,4,3,2,1]
5
```

```
ghci> null [1,2,3]
False
ghci> null []
True
```

```
ghci> reverse [5,4,3,2,1]
[1,2,3,4,5]
```

```
ghci> take 3 [5,4,3,2,1]
[5,4,3]
ghci> take 1 [3,9,3]
[3]
ghci> take 5 [1,2]
[1,2]
ghci> take 0 [6,6,6]
[]
```

```
ghci> drop 3 [8,4,2,1,5,6]
[1,5,6]
ghci> drop 0 [1,2,3,4]
[1,2,3,4]
ghci> drop 100 [1,2,3,4]
[]
```

```
ghci> minimum [8,4,2,1,5,6]
1
ghci> maximum [1,9,2,3,4]
9
```

```
ghci> sum [5,2,1,6,3,2,5,7]
31
ghci> product [6,2,1,2]
24
ghci> product [1,2,5,6,7,9,2,0]
0
```

```
ghci> 4 `elem` [3,4,5,6]
True
ghci> 10 `elem` [3,4,5,6]
False
```

#### Texas ranges
```
ghci> [1..20]
[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
ghci> ['a'..'z']
"abcdefghijklmnopqrstuvwxyz"
ghci> ['K'..'Z']
"KLMNOPQRSTUVWXYZ"
```

```
ghci> [2,4..20]
[2,4,6,8,10,12,14,16,18,20]
ghci> [3,6..20]
[3,6,9,12,15,18]
```

```
ghci> [0.1, 0.3 .. 1]
[0.1,0.3,0.5,0.7,0.8999999999999999,1.0999999999999999]
```

```
ghci> take 10 (cycle [1,2,3])
[1,2,3,1,2,3,1,2,3,1]
ghci> take 12 (cycle "LOL ")
"LOL LOL LOL "
```

```
ghci> take 10 (repeat 5)
[5,5,5,5,5,5,5,5,5,5]
```

```
gchi> replicate 3 10
[10,10,10]
```

#### I'm a list comprehension
Doubles of the first 10 natural numbers
```
ghci> [x*2 | x <- [1..10]]
[2,4,6,8,10,12,14,16,18,20]
```

Same as above but with the additional condition, also predicate, that the list elements are equal or greater than 12
```
ghci> [x*2 | x <- [1..10], x*2 >= 12]
[12,14,16,18,20]
```

All numbers from 50 to 100 whose remainder when divided with the number 7 is 3
```
ghci> [ x | x <- [50..100], x `mod` 7 == 3]
[52,59,66,73,80,87,94]
```

Replace each odd number greater than 10 with "BANG!" and each odd number that's less than 10 with "BOOM!", if a number isn't odd, we throw it out of our list
```
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]
```

```
ghci> boomBangs [7..13]
["BOOM!","BOOM!","BANG!","BANG!"]
```

We can include several predicates, if we wanted all numbers from 10 to 20 that are not 13, 15 or 19, we'd do:
```
ghci> [ x | x <- [10..20], x /= 13, x /= 15, x /= 19]
[10,11,12,14,16,17,18,20]
```

Draw elements from several lists
```
ghci> [ x*y | x <- [2,5,10], y <- [8,10,11]]
[16,20,22,40,50,55,80,100,110]
```

```
ghci> [ x*y | x <- [2,5,10], y <- [8,10,11], x*y > 50]
[55,80,100,110]
```

```
ghci> let nouns = ["hobo","frog","pope"]
ghci> let adjectives = ["lazy","grouchy","scheming"]
ghci> [adjective ++ " " ++ noun | adjective <- adjectives, noun <- nouns]
["lazy hobo","lazy frog","lazy pope","grouchy hobo","grouchy frog",
"grouchy pope","scheming hobo","scheming frog","scheming pope"]
```

```
length' xs = sum [1 | _ <- xs]
```

```
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]
```

```
ghci> removeNonUppercase "Hahaha! Ahahaha!"
"HA"
ghci> removeNonUppercase "IdontLIKEFROGS"
"ILIKEFROGS"
```

```
ghci> let xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]
ghci> [ [ x | x <- xs, even x ] | xs <- xxs]
[[2,2,4],[2,4,6,8],[2,4,2,6,2,6]]
```

#### Tuples
```
Couldn't match expected type `(t, t1)'
against inferred type `(t2, t3, t4)'
In the expression: (8, 11, 5)
In the expression: [(1, 2), (8, 11, 5), (4, 5)]
In the definition of `it': it = [(1, 2), (8, 11, 5), (4, 5)]
```

```
ghci> fst (8,11)
8
ghci> fst ("Wow", False)
"Wow"
```

```
ghci> snd (8,11)
11
ghci> snd ("Wow", False)
False
```

```
ghci> zip [1,2,3,4,5] [5,5,5,5,5]
[(1,5),(2,5),(3,5),(4,5),(5,5)]
ghci> zip [1 .. 5] ["one", "two", "three", "four", "five"]
[(1,"one"),(2,"two"),(3,"three"),(4,"four"),(5,"five")]
```

```
ghci> zip [5,3,2,6,2,7,2,5,4,6,6] ["im","a","turtle"]
[(5,"im"),(3,"a"),(2,"turtle")]
```

```
ghci> zip [1..] ["apple", "orange", "cherry", "mango"]
[(1,"apple"),(2,"orange"),(3,"cherry"),(4,"mango")]
```

```
ghci> let triangles = [ (a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10] ]
```

```
ghci> let rightTriangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]
```

```
ghci> let rightTriangles' = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24]
ghci> rightTriangles'
[(6,8,10)]
```

[Home](../README.md)

