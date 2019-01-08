Serokell Haskell Style Guide
============================

This style guide is derived from
[Johan Tibell's guide](https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md).
It aims to make code easy to understand and uniform,
while keeping diffs as small as possible.

We provide some configuration files for popular tools that help maintain code style:

* [`stylish-haskell` config][stylish-haskell]
* [`hlint` config](https://github.com/serokell/serokell-util/blob/master/.hlint.yaml)
* [`hlint` config](https://github.com/serokell/serokell-util/blob/master/.hlint-universum.yaml) specific to [`universum`](https://github.com/serokell/universum), our custom Prelude

General guidelines
-------------------

All existing projects _should_ continue using their current style guides, but
_may_ choose to switch to this one.
All new projects _must_ adhere to the guidelines below.

### Line Length

You _should_ keep maximum line length below *80 characters*. If necessary,
you _may_ use up to *100 characters*, although this is discouraged.

### Indentation

Tabs are illegal.  Use spaces for indenting.  Indent your code blocks
with *2 spaces*.  Indent the `where` keyword with 2 spaces and the definitions
within the `where` clause with 2 more spaces.  Some examples:

```haskell
sayHello :: IO ()
sayHello = do
  name <- getLine
  putStrLn $ greeting name
  where
    greeting name = "Hello, " ++ name ++ "!"

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs)
  | p x       = x : filter p xs
  | otherwise = filter p xs
```

### Blank Lines

* You _must_ add one blank line between top-level definitions.
* You _must not_ add any blank lines between type signatures and function definitions.
* You _should_ add one blank line between definitions in a type class instance
  declaration or inside a `where` clause if the definitions are large.
* You _may_ add blank lines inside a big `do` block to separate logical parts of it.
* See below for usage of blank lines in the import section.

### Whitespace

* You _should_ surround binary operators with a single space on either side: `3 + 5`.
  You _may_ choose not to do that to emphasize grouping of terms: `2 + 2*2`.
* When using currying with binary operators, you _must_ add one space between the argument
  and the operation: `(42 +)`.
* You _must_ remove all trailing whitespace.
* You _must_ append a trailing newline to all source files.

The last two points can be handled by [EditorConfig](https://editorconfig.org/).
You are encouraged to install an EditorConfig plugin for your editor and use
[our config][editorconfig] for Haskell files, but you _may_ also
configure your editor using specific instructions for
[removing trailing whitespace](https://github.com/editorconfig/editorconfig/wiki/Property-research:-Trim-trailing-spaces)
and
[appending a trailing newline](https://github.com/editorconfig/editorconfig/wiki/Newline-at-End-of-File-Support).

### Naming convention

You _must_ use the following cases:

+ **_lowerCamelCase_** for functions, variables, and global constants.
+ **_UpperCamelCase_** for types.

You _should not_ use short names like `n`, `sk`, `f`, unless their meaning is clear
from the context (function name, types, other variables, etc.).

You _should not_ capitalize all letters in an abbreviation.
For example, write `HttpServer` instead of `HTTPServer`.
Exception: two or three letter abbreviations, e.g. `IO`, `STM`.

**Record name conventions**

If data type has only one constructor then this data type name _should_ be the same
as the constructor name (also applies to `newtype`).

```haskell
data User = User Int String
```

Field name for `newtype` _should_ start with `un` or `run` prefix followed by type name.

* `run` for wrappers with monadic semantic.
* `un` for wrappers introduced for type safety.

Motivated by [this discussion](https://www.reddit.com/r/haskell/comments/7rl9hx/newtype_field_naming_getx_vs_runx/).

```haskell
newtype Coin = Coin { unCoin :: Int }
newtype PureDHT a = PureDHT { runPureDHT :: State (Set NodeId) a }
```

Field names for record data type should start with every capital letter in type
name.

```haskell
data NetworkConfig = NetworkConfig
  { ncDelay :: Microsecond  -- `nc` corresponds to `_N_etwork_C_onfig`
  , ncPort  :: Word
  }
```

You _should not_ adopt GHC's `-XOverloadedRecordFields` with
[`HasField`](https://www.stackage.org/haddock/lts-10.4/base-4.10.1.0/GHC-Records.html#t:HasField)
type class.

**Library-specific conventions**

Add `F` suffix to custom formatters to avoid name conflicts:

```haskell
nodeF :: NodeId -> Builder
nodeF = build
```

### Comments

#### Punctuation

If your comment is long enough to be put on a separate line, you _should_
write proper sentences. Start with a capital letter and use proper
punctuation. See below for end-of-line comments.

#### Top-Level Definitions

* You _must_ provide a type signature for every top-level definition.
* You _must_ comment every exported function and data type.
* You _should_ comment every top-level function.

You _must_ use [Haddock syntax](https://www.haskell.org/haddock/doc/html/ch03s08.html)
in the comments.

Example:

```haskell
-- | Send a message on a socket. The socket must be in a connected
-- state. Returns the number of bytes sent. Applications are
-- responsible for ensuring that all data has been sent.
send
  :: Socket      -- ^ Connected socket
  -> ByteString  -- ^ Data to send
  -> IO Int      -- ^ Bytes sent
```

For functions, the documentation _should_ give enough information to
apply the function without looking at its definition.

Record example:

```haskell
-- | Person representation used in address book.
data Person = Person
  { age  :: Int     -- ^ Age
  , name :: String  -- ^ First name
  }
```

For fields that require longer comments, format them this way:

```haskell
data Record = Record
  { -- | This is a very very very long comment that is split over
    -- multiple lines.
    field1 :: Text

    -- | This is a second very very very long comment that is split
    -- over multiple lines.
  , field2 :: Int
  }
```

#### End-of-Line Comments

Separate end-of-line comments from the code with 2 spaces. Align
comments for data type definitions. Some examples:

```haskell
data Parser = Parser
  Int         -- Current position
  ByteString  -- Remaining input

foo :: Int -> Int
foo n = salt * n + 9
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

* Only for the first occurrence of each API name in the comment (do not
  bother repeating a link)

Top-down guideline
------------------

### LANGUAGE extensions section

Write each `LANGUAGE` pragma on its own line, sort them alphabetically and
align by max width among them. `stylish-haskell` with [our config][stylish-haskell] can do this for you.

```haskell
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types       #-}
{-# LANGUAGE TemplateHaskell  #-}
```

### Module name

Use singular when naming modules (e.g. use `Data.Map` and
`Data.ByteString.Internal` instead of `Data.Maps` and
`Data.ByteString.Internals`). Sometimes it is acceptable to use plural
(e. g. `Types`, `Instances`).

### Export Lists

All modules _must_ have explicit exports.

Format export lists as follows:

```haskell
module Data.Set
  ( -- * The @Set@ type
    Set
  , empty
  , singleton

   -- * Querying
  , member
  ) where
```

Some clarifications:

1. Use 2-space indentation for export list.
2. You can split export list into sections or just write everything in one section.
3. It is strongly advised to sort each section alpabetically. However,
   within each section, classes, data types and type aliases _should_ be
   written before functions.
4. If your export list is empty, you _may_ write in on the same line as
   the `module` declaration.

### Imports

Imports should be grouped in the following order:

0. Implicit import of custom prelude (for example [`universum`](https://github.com/serokell/universum)) if you are using one. You _may_ also use [`base-noprelude`](https://hackage.haskell.org/package/base-noprelude) in order to avoid importing your custom prelude at all.
1. Everything from hackage packages or from your packages outside current project.
   "Project" is loosely defined as everything that is in your current repository.
2. Everything from current project.
3. Everything from current target (like `Bench.*` or `Test.*`).

Put a blank line between each group of imports.

The imports in each group should be sorted alphabetically. `stylish-haskell` with [our config][stylish-haskell] can do this for you.

Always use explicit import lists or `qualified` imports.
Try to use `qualified` imports only if import list is
big enough or there are conflicts in names. This makes the code more robust
against changes in these libraries. Exceptions:

1. _The Prelude_ or any custom prelude (e.g. [Universum](https://github.com/serokell/universum))
2. Modules that only reexport stuff from other modules

Unqualified types (i.e. `Map` vs. `M.Map`) look pretty good and not so ugly.
Prefer two-line imports for such standard containers.

```haskell
import Data.Map (Map)
import qualified Data.Map as Map
```

### Data Declarations

Align the constructors in a data type definition. Examples:

```haskell
data Tree a
  = Branch a (Tree a) (Tree a)
  | Leaf

data HttpException
  = InvalidStatusCode Int
  | MissingContentHeader
```

Format records as follows:

```haskell
data Person = Person
  { firstName :: String  -- ^ First name
  , lastName  :: String  -- ^ Last name
  , age       :: Int     -- ^ Age
  } deriving (Eq, Show)
```

You _must_ avoid having records with multiple constructors because
their getters are partial functions.

As usual, separate type classes with `, ` (comma and a space).

_WARNING_: try to avoid aggressive autoderiving because
[it can slow down compilation](http://www.stephendiehl.com/posts/production.html).

> Deriving instances of Read/Show/Data/Generic for largely recursive ADTs can
> sometimes lead to quadratic memory behavior when the nesting gets deep.

If you are using GHC-8.2.2 or higher you _should_ use
[`-XDerivingStrategies`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#deriving-strategies)
and specify the way you derive explicitly. Example:

```haskell
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies         #-}

newtype SpecialId = SpecialId Int
  deriving stock    (Eq, Ord, Show, Generic)
  deriving newtype  Read
  deriving anyclass (FromJSON, ToJSON)
```

### Function declaration

All top-level functions _must_ have type signatures.

All functions inside `where` _should_ have type signatures. Explicit type
signatures help avoid cryptic type errors. You _may_ choose not to specify them in
cases like working with pure arithmetics where everything is `Integer`, and explicit
type signatures look cumbersome.

> You most likely need `-XExplicitForAll` and `-XScopedTypeVariables` extensions
> to write polymorphic types of functions inside `where`.

Specialize function type signature to concrete types if you use this function with only
one type for each argument. Otherwise you should keep a more polymorphic version. With GHC,
you can also use the
[`SPECIALIZE` pragma](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#specialize-pragma)
to enable making more aggressive optimizations. Furthermore, the purpose of a specialized
function may be clearer from its type signature. You _may_ keep you signatures general if you are writing a library that you want to be
as abstract as possible.

You _should_ omit parentheses if you have only one constraint.

If function type signature is very long, you _should_ place the type of each argument
on its own line, and also align constraints similarly. Example:

```haskell
putValueInState
  :: (MonadIO m, WithLogger m)
  => UserState
  -> Maybe Int
  -> AppConfig
  -> (Int -> m ())
  -> m ()
```

If there are a lot of constraints, or they are very long, you _may_ put them
in a separate `type` definition, or format them as follows:

```haskell
parseTree
  :: ( KnownSpine components
     , AllConstrained (ComponentTokenizer components) components
     , AllConstrained (ComponentTokenToLit components) components
     )
  => Text
  -> Either (ParseError components) (Expr ParseTreeExt CommandId components)
```

If the line with argument names is too long, you _should_ put each argument
on a separate line with the usual 2-space indentation.
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

Do not use `() <$` to ignore the result of function, because it less legible
than `_ <-` or `void $`.

```haskell
foo = do
  _ <- sendCert ourVssCertificate  -- cannot be used as last statement
  void $ sendTransactionAndReport 1 "tx42"
```

Put operator fixity before operator signature:

```haskell
-- | Append a piece to the URI.
infixl 5 />
(/>) :: Uri -> PathPiece -> Uri
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
  {-# UNPACK #-} Int
  ByteArray
```

### List Declarations

Depending on the length of list elements, you _should_ either keep the list on one
line or put each element on a separate line. In the latter case, put a trailing `]`
on a separate line, like `}` when you format records. Example:

```haskell
numbers = [1, 2, 4]

exceptions =
  [ InvalidStatusCode
  , MissingContentHeader
  , InternalServerError
  ]
```

### Hanging Lambdas

Do not insert a space after a lambda.

You may or may not indent the code following a "hanging" lambda.  Use
your judgement. Some examples:

```haskell
bar :: IO ()
bar =
  forM_ [1, 2, 3] $ \n -> do
    putStrLn "Here comes a number!"
    print n

foo :: IO ()
foo =
  alloca 10 $ \a ->
  alloca 20 $ \b ->
  cFunction a b
```

### If-then-else clauses

Generally, guards and pattern matches should be preferred over _if-then-else_
clauses, where possible.  Short cases should usually be put on a single line
(when line length allows it).

When writing non-monadic code (i.e. when not using `do`) where guards
and pattern matches cannot be used, you can align _if-then-else_ clauses
like you would normal expressions:

```haskell
foo =
  if ...
  then ...
  else ...
```

Or you can align _if-then-else_ in different style inside lambdas.

```haskell
foo =
  bar $ \qux -> if predicate qux
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

Use `-XMultiwayIf` only if you need complex `if-then-else` inside `do`-blocks.

### Case expressions

The alternatives in a case expression can be indented as follows:

```haskell
foobar =
  case something of
    Just j  -> foo
    Nothing -> bar
```

You _should_ align the `->` arrows whenever it helps readability.

It is suggested to use `-XLambdaCase`.
See [this discussion](https://github.com/jaspervdj/stylish-haskell/issues/186) for some usage examples.

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

Use `-XApplicativeDo` only when necessary.

You _should_ use `-XApplicativeDo` when you are writing a parser of CLI arguments with
`optparse-applicative` because it is very easy to mess up the order of the arguments.
However, be aware of [some non-obvious behavior](https://www.reddit.com/r/haskell/comments/7679g8/ghc_821_applicativedo_possible_bug/)
when you use pattern-matching inside `do`-blocks.

Dealing with laziness
---------------------

By default, use strict data types and lazy functions.

### Data types

You _should_ enable [`StrictData`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-StrictData)
globally for your project, marking fields explicitly with `~` when you need laziness.

```haskell
{-# LANGUAGE StrictData #-}

-- Good
data Point = Point
  { pointX :: Double  -- ^ X coordinate
  , pointY :: Double  -- ^ Y coordinate
  }

-- OK, but needs justification
data Point = Point
  { pointX :: ~Double  -- ^ X coordinate
  , pointY :: ~Double  -- ^ Y coordinate
  }
```

Additionally, unpacking simple fields often improves performance and
reduces memory usage:

```haskell
data Point = Point
  { pointX :: {-# UNPACK #-} Double  -- ^ X coordinate
  , pointY :: {-# UNPACK #-} Double  -- ^ Y coordinate
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
just small fields (e.g. `Double` or `Int`). If you are using GHC 7.4 or
later you can use `NOUNPACK` to selectively opt-out for the unpacking
enabled by `-funbox-strict-fields`.

For backwards compatibility, you _may_ choose to keep `StrictData` off globally, but you are
strongly advised not to. If you choose not to enable it, you _should_ mark all record
fields as strict, unless there is an explicit reason to make them lazy.

```haskell
-- Good
data Point = Point
  { pointX :: !Double  -- ^ X coordinate
  , pointY :: !Double  -- ^ Y coordinate
  }

-- Needs justification for implicit laziness
data Point = Point
  { pointX :: Double  -- ^ X coordinate
  , pointY :: Double  -- ^ Y coordinate
  }
```

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
sections. You can put blank lines between groups in each section and sort
each group independently.

[stylish-haskell]: https://github.com/serokell/serokell-util/blob/master/.stylish-haskell.yaml
[editorconfig]: https://github.com/serokell/serokell-util/blob/master/.editorconfig
