# Serokell Haskell Style Guide

This style guide is derived from
[Johan Tibell's guide](https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md).
It aims to make code easy to understand and uniform,
while keeping diffs as small as possible.

We provide some configuration files for popular tools that help maintain code style:

* [`stylish-haskell` config][stylish-haskell]
* [`hlint` config](https://github.com/serokell/serokell-util/blob/master/.hlint.yaml)
* [`hlint` config](https://github.com/serokell/universum/blob/master/.hlint.yaml) specific to [`universum`](https://github.com/serokell/universum), our custom Prelude
* [EditorConfig config][editorconfig]

Note for Serokell people:

* All existing projects _should_ continue using their current style guides, but
_may_ choose to switch to this one.
* All new projects _must_ adhere to the guidelines below.

## General guidelines

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
  declaration or inside a `where` clause or `let` block if the definitions are large.
* You _may_ add blank lines inside a big `do` block to separate logical parts of it.
* See below for usage of blank lines in the import section.

### Whitespace

* You _may_ align blocks of code with extra whitespace if alignment
  emphasizes common structure.
  ```haskell
  data WalletApiRecord route = WalletApiRecord
    { _test      :: route :- WTestApi             -- /test
    , _wallets   :: route :- WWalletsApi          -- /wallets
    , _accounts  :: route :- WAccountsApi         -- /accounts
    , _addresses :: route :- WAddressesApi        -- /addresses
    , _profile   :: route :- WProfileApi          -- /profile
    -- ...
    } deriving Generic
  ```
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

### Naming conventions

#### General identifiers

You _must_ use the following cases:

+ `lowerCamelCase` for functions, variables, and global constants.
+ `UpperCamelCase` for types.

You _should not_ use short names like `n`, `sk`, `f`, unless their meaning is clear
from the context (function name, types, other variables, etc.).

You _should not_ capitalize all letters in an abbreviation.
For example, write `HttpServer` instead of `HTTPServer`.
Exception: two or three letter abbreviations, e.g. `IO`, `STM`.

#### Records

If data type has only one constructor then this data type name _should_ be the same
as the constructor name (also applies to `newtype`).

```haskell
data User = User Int String
```

Field name for `newtype` _should_ start with `un` or `run` prefix followed by type name
(motivated by [this discussion](https://www.reddit.com/r/haskell/comments/7rl9hx/newtype_field_naming_getx_vs_runx/)):

* `run` for wrappers with monadic semantic
* `un` for wrappers introduced for type safety

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

#### Library-specific conventions

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
    salt = 453645243  -- Magic hash salt
```

#### Links

Your documentation _should_ include links to definitions, but use
them sparingly. We recommend adding a link to an API name
if:

* The user might actually want to click on it for more information (in
  your opinion), and

* Only for the first occurrence of each API name in the comment (do not
  bother repeating a link)

## Top-down guideline

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
2. You _may_ split export list into sections or just write everything in one section.
3. You _should_ sort each section alpabetically. However,
   within each section, classes, data types and type aliases _should_ be
   written before functions.
4. If your export list is empty, you _may_ write in on the same line as
   the `module` declaration.

You _may_ use [`weeder`][weeder] to detect unused exports.

### Imports

Imports _should_ be grouped in the following order:

0. Implicit import of custom prelude (for example [`universum`](https://github.com/serokell/universum)) if you are using one. You _may_ also use [`base-noprelude`](https://hackage.haskell.org/package/base-noprelude) in order to avoid importing your custom prelude at all.
1. Everything from hackage packages or from your packages outside current project.
   "Project" is loosely defined as everything that is in your current repository.
2. Everything from current project.
3. Everything from current target (like `Bench.*` or `Test.*`).

Put a blank line between each group of imports.

The imports in each group should be sorted alphabetically. `stylish-haskell` with [our config][stylish-haskell] can do this for you.

You _may_ use implicit imports for modules within your
current project.

You _should_ use explicit import lists or `qualified` imports
for everything outside of your current project.
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

You _must_ not declare records with multiple constructors because
their getters are partial functions.

As usual, separate type classes with `, ` (comma and a space).

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

You _should_ avoid overly general signatures for functions that are actually used
with only one type for each parameter. If you need the polymorhic version (i.e.
if you are instantiating it more than once or if you are writing a library), you
_may_ use GHC's
[`SPECIALIZE` pragma](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#specialize-pragma).

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
    = do               -- note how this line uses 4-space indentation
  <code goes here>
```

In other cases place `=` sign on the same line where function definition is.

You _must_ put operator fixity before operator signature:

```haskell
-- | Append a piece to the URI.
infixl 5 />
(/>) :: Uri -> PathPiece -> Uri
```

### Pragmas

If you need to use pragmas, you _must_ put them next to
the function that they apply to. Example:

```haskell
id :: a -> a
id x = x
{-# INLINE id #-}
```

For data type definitions, you _must_ put the pragma before
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

You _must not_ insert a space after a lambda.

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

Generally, guards and pattern matches should be preferred over `if-then-else`
clauses, where possible.  Short cases should usually be put on a single line
(when line length allows it).

When writing non-monadic code (i.e. when not using `do`) where guards
and pattern matches cannot be used, you _may_ align `if-then-else` clauses
like you would normal expressions:

```haskell
foo =
  if ...
  then ...
  else ...
```

You _may_ align `if-then-else` in different style inside lambdas.

```haskell
foo =
  bar $ \qux -> if predicate qux
    then doSomethingSilly
    else someOtherCode
```

You _may_ also write `if-then-else` in imperative style inside do blocks:

```haskell
foo = do
  someCode
  if condition
  then do
    someMoreCode
    andMore
  else  -- you _may_ omit the `do` if the block is a one-liner
    return ()
```

Use `-XMultiwayIf` only if you need complex `if-then-else` inside `do`-blocks.

### Case expressions

The alternatives in a case expression _should_ be indented as follows:

```haskell
foobar =
  case something of
    Just j  -> foo
    Nothing -> bar
```

You _should_ align the `->` arrows whenever it helps readability.

It is suggested to use `-XLambdaCase`. See
[this discussion](https://github.com/jaspervdj/stylish-haskell/issues/186)
for some usage examples.

## Misc

### Point-free style

Avoid over-using point-free style.  For example, this is hard to read:

```haskell
-- Bad:
f = (g .) . h
```

## Cabal file formatting

Modules and libraries should go in alphabetical order inside corresponding
sections. You _may_ put blank lines between groups in each section and sort
each group independently.

You _may_ use [`weeder`][weeder] to detect unused dependencies and exported modules.

[stylish-haskell]: https://github.com/serokell/serokell-util/blob/master/.stylish-haskell.yaml
[editorconfig]: https://github.com/serokell/serokell-util/blob/master/.editorconfig
[weeder]: https://hackage.haskell.org/package/weeder
