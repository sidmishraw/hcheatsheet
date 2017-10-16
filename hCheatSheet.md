# Haskell cheat sheet

`Prelude` is the default module that is always loaded.

`type String = [Char]`

`The **do** keyword introduces a block of actions that can cause effects in the real world, such as reading or writing a file.`

`To define or apply a function or value constructor using infix notation, we enclose its name in backtick characters (sometimes known as backquotes).`
```haskell
3 `div` 4 -- 0
```

## Library functions for making life easy

* `System.Environment.getArgs` : Reads command line arguments as a `[[Char]] or [String]`
  This is a dirty function as it requires `IO` Monad. Should be used in a `do` block.

Basing upon the example from the book [Real World Haskell (RWH)](http://book.realworldhaskell.org/read/functional-programming.html)
```haskell
import System.Environment (getArgs)

-- | Read arguments from the command line
-- if the number of arguments is not 2, print error msg
readFromCommandLine :: IO()
readFromCommandLine = do
  args <- getArgs
  case args of
    [input, output] -> print $ input ++ " :: " ++ output
    _ -> print "error:: needed 2 command line arguments"

main :: IO()
main = readFromCommandLine
```

* `id with signature (id :: a -> a    -- Defined in ‘GHC.Base’)` is the identity function. Common usage is for protecting information across functions.

* `lines with signature (lines :: String -> [String] -- Defined in ‘base-4.9.1.0:Data.OldList’` lets us split a text string on line boundaries. It returns a list of strings with line termination characters omitted.

  `lines` relies on us reading a file in “text mode” in order to work.

  `When we read a file in text mode, the file I/O library translates the line ending sequence "\r\n" (carriage return followed by newline) to "\n" (newline alone), and it does the reverse when we write a file. On Unix-like systems, text mode does not perform any translation. As a result of this difference, if we read a file on one platform that was written on the other, the line endings are likely to become a mess. (Both readFile and writeFile operate in text mode.)` ~ [RWH](http://book.realworldhaskell.org/read/functional-programming.html)


* `break with signature (break :: (a -> Bool) -> [a] -> ([a], [a])       -- Defined in ‘GHC.List’` takes a function as its first parameter. That function must examine an element of the list, and return a Bool to indicate whether to break the list at that point. The `break` function returns a pair, which consists of `the sublist consumed before the predicate returned True (the prefix`), and `the rest of the list (the suffix`).

```haskell
break odd [2,4,5,6,8] -- o/p ([2,4],[5,6,8])
```

* `(:) or the List constructor` has signature `(:) :: a -> [a] -> [a]` and is defined as:
  ```
  data [] a = ... | a : [a]       -- Defined in ‘GHC.Types’
  infixr 5 :
  ```

  `infixr` above specifies that `(:)` has a precedence level of 5

* `unlines` with signature `unlines :: [String] -> String -- Defined in ‘base-4.9.1.0:Data.OldList’` concatenates a list of strings, adding a newline to the end of each.
