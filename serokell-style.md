Serokell Haskell Style Guide
============================
> This style guide is mostly a copy of [Johan Tibell's guide](https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md)
> with some restructurization, elaboration on some topics and some additions.
> This style guide's aims are code beauty, readability and understandability.

You can find our other formatting utilites and guidelines which expand the code style:
* [.stylish-haskell.yaml](https://github.com/serokell/serokell-core/blob/master/.stylish-haskell.yaml) config
* [Universum](https://github.com/serokell/universum)
* TODO: custom _HLint_ rules
* TODO: custom _hindent_ rules

General guide lines
-------------------
### Line Length

Maximum line length is *80 characters* or *100 characters* if necessary.

Modern screens have high definition and big width.
But with some tiling managers and two terminals on one screen you are not able to
see many characters on one line.
On the other hand, restricting line size to a very small number like 80 leads to
some crazy indentation despite the fact that
shorter lines should force you to write well structured code.
That's why *100* is a reasonable compromize.


### Indentation

Tabs are illegal.  Use spaces for indenting.  Indent your code blocks
with *4 spaces*.  Indent the `where` keyword with two spaces to set it
apart from the rest of the code and indent the definitions in a
`where` clause with 2 spaces.  Some examples:

```haskell
sayHello :: IO ()
sayHello = do
    name <- getLine
    putStrLn $ greeting name
  where
    greeting name = "Hello, " ++ name ++ "!"

filter :: (a -> Bool) -> [a] -> [a]
filter _ []     = []
filter p (x:xs)
    | p x       = x : filter p xs
    | otherwise = filter p xs
```

### Blank Lines

One blank line between top-level definitions. No blank lines between
type signatures and function definitions. Add one blank line between
functions in a type class instance declaration if the function bodies
are large. You can add blank lines inside a big `do` block to separate logical
parts of it. You can also use blank lines to separate definitions inside `where`
clause. And finally you can separate `imports`'s into logical blocks by prefix.

### Whitespace

Surround binary operators with a single space on either side. In case of
currying add one space between the argument and the operation.

_Exception_: `%` operator for string formatting from `Formatting` library.
You can omit spaces if surrounding strings have spaces. It also helps in cases
when formatted strings are too long. Compare:

`("block with "%int%" entries")`

`("block with " % int % " entries")`

Use some tool to remove trailing spaces.

### Naming convention

Casing:
+ **_lowerCamelCase_** for function and variable names.
+ **_UpperCamelCase_** for types.
+ TODO: some convention for global constants?

Don't use short names like `n`, `sk`, `f` unless their meaning is clear from
context (function name, types, other variables, etc.).

For readability reasons, don't capitalize all letters when using an
abbreviation.  For example, write `HttpServer` instead of
`HTTPServer`.  Exception: two or three letter abbreviations, e.g. `IO`, `STM`.

**Library specific conventions**

Add `F` suffix to custom formatters to avoid name conflicts:

```haskell
nodeF :: Format r (NodeId -> r)
nodeF = build
```

### Comments

#### Punctuation

Write proper sentences; start with a capital letter and use proper
punctuation.

#### Top-Level Definitions

Comment every top level function (particularly exported functions),
and provide a type signature; use Haddock syntax in the comments.
Comment every exported data type.  Function example:

```haskell
-- | Send a message on a socket. The socket must be in a connected
-- state. Returns the number of bytes sent. Applications are
-- responsible for ensuring that all data has been sent.
send :: Socket      -- ^ Connected socket
     -> ByteString  -- ^ Data to send
     -> IO Int      -- ^ Bytes sent
```

For functions, the documentation should give enough information to
apply the function without looking at its definition.

Record example:

```haskell
-- | Bla bla bla.
data Person = Person
    { age  :: !Int     -- ^ Age
    , name :: !String  -- ^ First name
    }
```

For fields that require longer comments, format them this way:

```haskell
data Record = Record
    { -- | This is a very very very long comment that is split over
      -- multiple lines.
      field1 :: !Text

      -- | This is a second very very very long comment that is split
      -- over multiple lines.
    , field2 :: !Int
    }
```

#### End-of-Line Comments

Separate end-of-line comments from the code with 2 spaces. Align
comments for data type definitions. Some examples:

```haskell
data Parser = Parser
    !Int         -- Current position
    !ByteString  -- Remaining input

foo :: Int -> Int
foo n = salt * 32 + 9
  where
    salt = 453645243  -- Magic hash salt.
```

#### Links

Use in-line links economically.  You are encouraged to add links for
API names. It is not necessary to add links for all API names in a
Haddock comment. We therefore recommend adding a link to an API name
if:

* The user might actually want to click on it for more information (in
  your opinion), and

* Only for the first occurrence of each API name in the comment (don't
  bother repeating a link)

Top-down guideline
------------------

### LANGUAGE extensions section

Write each `LANGUAGE` pragma on its own line, sort them alphabetically and
align by max width among them.

```haskell
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types       #-}
{-# LANGUAGE TemplateHaskell  #-}
```

### Module name

Use singular when naming modules (e.g. use `Data.Map` and
`Data.ByteString.Internal` instead of `Data.Maps` and
`Data.ByteString.Internals`). Sometimes it's acceptable to use plural
(e. g. `Types`, `Instances`).

### Export Lists

Format export lists as follows:

```haskell
module Data.Set
       (
         -- * The @Set@ type
         Set
       , empty
       , singleton

         -- * Querying
       , member
       ) where
```

Some clarifications:

1. Use 7 spaces indentation for export list (so that bracket is below
   the first letter in module name).
2. You can split export list into sections or just write all as single section.
3. It is strongly adviced to sort each section alpabetically. However,
   classes, data types and type aliases should be written before
   functions.

### Imports

Imports should be grouped in the following order:

1. Everything from hackage packages
2. Everything from your packages outside current project
3. Everything from current project
4. Everything from current target (like `Bench.*` or `Test.*`)

Put a blank line between each group of imports. It is also okay to put blank
lines between `Data` and `Control` section of std imports because these sections
are usually big enough.

The imports in each group should be sorted alphabetically, by module name.

Always use explicit import lists or `qualified` imports.
Try to use `qualified` imports only if import list is
big enough or there are conflicts in names. This makes the code more robust
against changes in these libraries. Exceptions: 

1. _The Prelude_ or any custom prelude (e.g. [Universum](https://github.com/serokell/universum))
2. Modules that only reexport stuff from other modules

If `import` is unqualified then put _11 spaces_ between `import` keyword and
module name (i.e. length of `qualified` + 2).

Unqualified types (i.e. `Map` vs. `M.Map`) look pretty good and not so ugly.
Prefer two-line imports for such standard containers.

```haskell
import           Data.Map (Map)
import qualified Data.Map as M hiding (Map)
```

Such tools as `stylish-haskell` can make your import section look very nice!

### Data Declarations

Align the constructors in a data type definition.  Example:

```haskell
data Tree a = Branch !a !(Tree a) !(Tree a)
            | Leaf
```

For long type names the following formatting is also acceptable:

```haskell
data HttpException
    = InvalidStatusCode Int
    | MissingContentHeader
```

Format records as follows:

```haskell
data Person = Person
    { firstName :: !String  -- ^ First name
    , lastName  :: !String  -- ^ Last name
    , age       :: !Int     -- ^ Age
    } deriving (Eq, Show)
```

Type classes in `deriving` section should be always surrounded by
parentheses. Space between names is optional.

_WARNING_: try to avoid aggressive autoderiving. Deriving instances can
slowdown compilation 
(stated here: http://www.stephendiehl.com/posts/production.html)

> Deriving instances of Read/Show/Data/Generic for largely recursive ADTs can
> sometimes lead to quadratic memory behavior when the nesting gets deep.

### Function declaration

All functions must have type signatures.

Specialize function type signature for concrete types if you're using this function
with only one type for each argument. Otherwize you should use more polymorphic
version. Compiler can optimize specialized functions better
(TODO: link to haskell manual) and meaning of this function may be clearer.
Use this rule unless you are the library creator and want your library to be abstract
as possible.

It is allowed to omit parentheses for only one type class constraint.

If function type signature is very long then place type of each argument under
its own line with respect to alignment.

```haskell
putValueInState
    :: MonadIO m
    => UserState
    -> Maybe Int
    -> AppConfig
    -> (Int -> m ())
    -> m ()
```

If the line with argument names is too big then put each argument on its own line
and separate it somehow from body section.

```haskell
putValueInState
    userState
    mValue@(Just x)
    Config{..}        -- { should go after ctor name without space
    valueModificator
  = do
    <code goes here>
```

In other cases place `=` sign on the same line where function definition is.

Use `() <$` to  ignore the result of function. It looks cool. If this is not possible
due to `$` then use either `_ <-` or `void $`.

```haskell
foo = do
    _  <- forkIO $ myThread config  -- can't be used as last statement
    () <$ sendTransactionAndReport 1 "tx42"
```

### Pragmas

Put pragmas immediately following the function they apply to.
Example:

```haskell
id :: a -> a
id x = x
{-# INLINE id #-}
```

In case of data type definitions you must put the pragma before
the type it applies to. Example:

```haskell
data Array e = Array
    {-# UNPACK #-} !Int
    !ByteArray
```

### List Declarations

Align the elements in the list.  Example:

```haskell
exceptions =
    [ InvalidStatusCode
    , MissingContentHeader
    , InternalServerError
    ]
```

Optionally, you can skip the first newline. Use your judgement.

```haskell
directions = [ North
             , East
             , South
             , West
             ]
```

### Hanging Lambdas

Don't insert a space after a lambda.

You may or may not indent the code following a "hanging" lambda.  Use
your judgement. Some examples:

```haskell
bar :: IO ()
bar = forM_ [1, 2, 3] $ \n -> do
          putStrLn "Here comes a number!"
          print n

foo :: IO ()
foo = alloca 10 $ \a ->
      alloca 20 $ \b ->
      cFunction a b
```

### If-then-else clauses

Generally, guards and pattern matches should be preferred over _if-then-else_
clauses, where possible.  Short cases should usually be put on a single line
(when line length allows it).

When writing non-monadic code (i.e. when not using `do`) where guards
and pattern matches can't be used, you can align _if-then-else_ clauses
like you would normal expressions:

```haskell
foo = if ...
      then ...
      else ...
```

Or you can align _if-then-else_ in different style inside lambdas.

```haskell
foo = bar $ \qux -> if predicate qux
    then doSomethingSilly
    else someOtherCode
```

You can also write _if-then-else_ in imperative style inside do blocks

```haskell
foo = do
    someCode
    if condition then do
        someMoreCode
        andMore
    else
        return ()
```

### Case expressions

The alternatives in a case expression can be indented using either of
the two following styles:

```haskell
foobar = case something of
    Just j  -> foo
    Nothing -> bar
```

or as

```haskell
foobar = case something of
             Just j  -> foo
             Nothing -> bar
```

Align the `->` arrows when it helps readability.

### let expressions

Put `let` before each variable inside a `do` block. But beware of name shadowing
(though compiler can help with it).

```haskell
foo = do
    let x   = 10
    let f 1 = 5
        f _ = 0  -- possible shadowing here with let
    return $ x + f 2
```

### ApplicativeDo

TODO: use everywhere if possible?

Dealing with laziness
---------------------

By default, use strict data types and lazy functions.

### Data types

Constructor fields should be strict, unless there's an explicit reason
to make them lazy.  This helps to avoid many common pitfalls caused by too much
laziness and reduces the number of brain cycles the programmer has to
spend thinking about evaluation order.

```haskell
-- Good
data Point = Point
    { pointX :: !Double  -- ^ X coordinate
    , pointY :: !Double  -- ^ Y coordinate
    }
```

```haskell
-- Bad
data Point = Point
    { pointX :: Double  -- ^ X coordinate
    , pointY :: Double  -- ^ Y coordinate
    }
```

Additionally, unpacking simple fields often improves performance and
reduces memory usage:

```haskell
data Point = Point
    { pointX :: {-# UNPACK #-} !Double  -- ^ X coordinate
    , pointY :: {-# UNPACK #-} !Double  -- ^ Y coordinate
    }
```

As an alternative to the `UNPACK` pragma, you can put

```haskell
{-# OPTIONS_GHC -funbox-strict-fields #-}
```

at the top of the file.  Including this flag in the file itself instead
of e.g. in the Cabal file is preferable as the optimization will be
applied even if someone compiles the file using other means (i.e. the
optimization is attached to the source code it belongs to).

Note that `-funbox-strict-fields` applies to all strict fields, not
just small fields (e.g. `Double` or `Int`). If you're using GHC 7.4 or
later you can use `NOUNPACK` to selectively opt-out for the unpacking
enabled by `-funbox-strict-fields`.

### Functions

Have function arguments be lazy unless you explicitly need them to be
strict.

The most common case when you need strict function arguments is in
recursion with an accumulator:

```haskell
mysum :: [Int] -> Int
mysum = go 0
  where
    go !acc []    = acc
    go acc (x:xs) = go (acc + x) xs
```

Misc
----

### Point-free style ###

Avoid over-using point-free style.  For example, this is hard to read:

```haskell
-- Bad:
f = (g .) . h
```

Cabal file formatting
---------------------

### Modules & libraries

Modules and libraries should go in alphabetical order inside corresponding
sections. You can put blank lines between groups in each section.

TODO: bounds for packages?

### Warnings ###

Code should be compilable with `-Wall` without warnings.

Allowed ghc forbidding option: `-fno-warn-orphans`

### Default extensions

+ DeriveDataTypeable
+ GeneralizedNewtypeDeriving
+ OverloadedStrings
+ RecordWildCards
