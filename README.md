_This repository is [deprecated](#deprecated)_


# Serokell Utils

A random assortment of various helpers and utilities used in Serokell projects.


## Deprecated

For a long time, this repository has been a one-stop shop (i.e. a trash can).
We are now splitting it into multiple repositories with a better structure and
narrower scope.

### Haskell style guide

What used to be `serokell-style.md` now has its very [own repository][style].

### Default configurations

* `.editorconfig`
* `.hindent.yaml`
* `.hlint.yaml`
* `.stylish-haskell.yaml`

All were moved to the [Metatemplates repository][configs].

### Haskell library

The library of Haskell definitions is still [available on Hackage][hackage],
but is no longer maintained. We will be splitting it into a set of smaller
and more specific libraries and all improvements will be happening in them.

Details of where each individual piece of functionality has been moved
will be added here.


[style]: https://github.com/serokell/style
[configs]: https://github.com/serokell/metatemplates/tree/master/templates/haskell
[hackage]: https://hackage.haskell.org/package/serokell-util


## Section for Serokell developers

If you are a Serokell developer and you loved `serokell-util`, then you heart is
probably broken right now. We are sorry! But, please, understand that this had
to happen and try to move on. This section is here to help you.

### I just want to use a function that I know was in `serokell-util`

Feel free to use the version of the library from Hackage. However, consider
doign a quick search first: maybe what you need has already been implemented
in some other package, and maybe that implementation is even better.

### I want to change something in `serokell-util`

It is not possible, `serokell-util` is dead. We know you can be in a hurry,
but, plase, stop where the functionality you want to change really belongs.
If it is a utility function, maybe it makes sense to actually add it to the
upstream package? If it is something more-or-less self-contained, then
let’s start a new library, focused specifically on providing the functionality
that you need, – feel free to do it! Just don’t forget to update this readme
with details of where you moved the functionality.

### I have other questions

Great! You can always ask them in the `#libraries` Slack channel.
