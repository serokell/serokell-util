Serokell Haskell Style Guide
============================
> This style guide is mostly a copy of [Johan Tibell's guide](https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md)
> with some restructuring, elaboration on some topics and some additions.
> The aims of this style guide are code beauty, readability and understandability.

You can find our other formatting utilites and guidelines which expand the code style:

* [`stylish-haskell` config](https://github.com/serokell/serokell-util/blob/master/.stylish-haskell.yaml)
* [`hlint` config](https://github.com/serokell/serokell-util/blob/master/.hlint.yaml)
* [`hlint` config](https://github.com/serokell/serokell-util/blob/master/.hlint-universum.yaml) specific to [`universum`](https://github.com/serokell/universum), our custom Prelude

General guidelines
-------------------
### Line Length

Maximum line length is *80 characters* or *100 characters* if necessary.

Modern screens are wide and have high resolution.
Yet, when you have two terminals open side-by-side or edit two files simultaneously,
you will not be able to fit many characters on a line.
On the other hand, restricting line length to a very small number like 80 leads
to crazy indentation despite the fact that shorter lines should supposedly force you
to write well-structured code.
That is why we pick *100* as a reasonable compromise.


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
clause. Blank lines are used to separate imports (this is described in a section below).

### Whitespace

You _should_ surround binary operators with a single space on either side.
In case of currying add one space between the argument and the operation.
Use some tool to remove trailing spaces automatically. See
[this page](https://github.com/editorconfig/editorconfig/wiki/Property-research:-Trim-trailing-spaces)
for instructions for some editors.

### Naming convention

Casing:

+ **_lowerCamelCase_** for functions, variables, and global constants.
+ **_UpperCamelCase_** for types.

Do not use short names like `n`, `sk`, `f` unless their meaning is clear from
context (function name, types, other variables, etc.).

Do not capitalize all letters when using an
abbreviation.  For example, write `HttpServer` instead of
`HTTPServer`.  Exception: two or three letter abbreviations, e.g. `IO`, `STM`.

**Records name conventions**

If data type has only one constructor then this data type name should be same
as constructor name (also applies to `newtype`).

```haskell
data User = User Int String
```

Field name for `newtype` should start with `un` or `run` prefix followed by type name.

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

We do *not* adopt GHC's `-XOverloadedRecordFields` with
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

Comment every top level function (particularly exported functions),
and provide a type signature; use Haddock syntax in the comments.
Comment every exported data type.  Function example:

```haskell
-- | Send a message on a socket. The socket must be in a connected
-- state. Returns the number of bytes sent. Applications are
-- responsible for ensuring that all data has been sent.
send
    :: Socket      -- ^ Connected socket
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
align by max width among them. `stylish-haskell` with [our config](https://github.com/serokell/serokell-util/blob/master/.stylish-haskell.yaml) can do this for you.

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

1. Use 7 spaces indentation for export list (so that bracket is below
   the first letter in module name).
2. You can split export list into sections or just write all as single section.
3. It is strongly advised to sort each section alpabetically. However,
   classes, data types and type aliases should be written before
   functions.

### Imports

Imports should be grouped in the following order:

0. Implicit import of custom prelude (for example [`universum`](https://github.com/serokell/universum)) if you are using one. You _may_ also use [`base-noprelude`](https://hackage.haskell.org/package/base-noprelude) and import your custom prelude implicitly.
1. Everything from hackage packages or from your packages outside current project.
2. Everything from current project.
3. Everything from current target (like `Bench.*` or `Test.*`).

Put a blank line between each group of imports.

The imports in each group should be sorted alphabetically, by module name. `stylish-haskell` with [our config](https://github.com/serokell/serokell-util/blob/master/.stylish-haskell.yaml) can do this for you.

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
    = Branch !a !(Tree a) !(Tree a)
    | Leaf

data HttpException
    = InvalidStatusCode !Int
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

You should generally avoid having records with multiple constructors because
their getters are partial functions. This is somewhat alleviated with.
`-XRecordWildCards`, but there is still temptation to use getters directly,
resulting in potential errors. If you are certain you need them, add a usual
4-space indentation before `{`, but put `}` on the same line as the last field.
Example:

```haskell
data Address
    = PubKeyAddress
        { addrKeyHash :: !(AddressHash PublicKey) }
    | ScriptAddress
        { addrScriptHash   :: !(AddressHash Script)
        , addrDistribution :: ![(AddressHash PublicKey, Coin)] }
    deriving (Show, Eq)
```

If there is only one field for every constructor, you _may_ write it more compactly.

```haskell
data Address
    = PubKeyAddress { addrKeyHash    :: !(AddressHash PublicKey) }
    | ScriptAddress { addrScriptHash :: !(AddressHash Script)    }
    deriving (Show, Eq)
```

You _may_ omit parentheses around type classes in the `deriving` section
if there is only one type class. As usual, separate type classes with `, ` (comma
and a space).

_WARNING_: try to avoid aggressive autoderiving. Deriving instances can
slow down compilation
(source: http://www.stephendiehl.com/posts/production.html).

> Deriving instances of Read/Show/Data/Generic for largely recursive ADTs can
> sometimes lead to quadratic memory behavior when the nesting gets deep.

If you are using GHC-8.2.2 or higher you _should_ use the
[`-XDerivingStrategies`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#deriving-strategies)
extension and specify the way you derive explicitly. Example:

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

Do not use `() <$` to ignore the result of function, because it harms readability.
Instead, use either `_ <-` or `void $`.

```haskell
foo = do
    _  <- forkIO $ myThread config  -- cannot be used as last statement
    void $ sendTransactionAndReport 1 "tx42"
```

Put operator fixity before operator signature:

```haskell
-- | This operator looks cool!
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
    {-# UNPACK #-} !Int
    !ByteArray
```

### List Declarations

Depending on the length of list elements, you _should_ either keep the list on one
line or put each element on a separate line. In the latter case, put a trailing `]`
on a separate line, like `}` when you format records. Example:

```haskell
numbers = [1, 2, 3]

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
sections. You can put blank lines between groups in each section.

### Warnings ###

The following GHC options _should_ be enabled globally for your project
([source](https://medium.com/mercury-bank/enable-all-the-warnings-a0517bc081c3)):

```
-Weverything
-Wno-missing-exported-signatures
-Wno-missing-import-lists
-Wno-missed-specialisations
-Wno-all-missed-specialisations
-Wno-unsafe
-Wno-safe
-Wno-missing-local-signatures
-Wno-monomorphism-restriction
-Wno-implicit-prelude
-Werror
```

A successful build _must_ not trigger any of the enabled warnings.

You _may_ use `{-# OPTIONS_GHC -fno-warn-orphans #-}` on a per-module basis.

### Default extensions

The following list of extensions is allowed to be specified inside
`default-extensions` section inside `.cabal` files.

+ ConstraintKinds
+ DefaultSignatures
+ DeriveDataTypeable
+ DeriveGeneric
+ GeneralizedNewtypeDeriving
+ LambdaCase
+ NoImplicitPrelude
+ OverloadedStrings
+ RecordWildCards
+ ScopedTypeVariables
+ StandaloneDeriving
+ TupleSections
+ TypeApplications
+ ViewPatterns
