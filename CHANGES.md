0.9.0
=====

* [#45](https://github.com/serokell/serokell-util/issues/45):
  Move `Aeson.Options` to separate package [`aeson-options`][1].
* [#34](https://github.com/serokell/serokell-util/issues/34):
  Remove `Serokell.Util.Base` module, remove `fromStr`, `strArgument`, `strOption`.
  Remove `directory`, `extra`, `filepath`, `monad-control`, `semigroups` dependencies.
* [#32](https://github.com/serokell/serokell-util/issues/32):
  Remove `show` and `show'` functions from `Util.Text` module.

0.8.0
=====

* [#54](https://github.com/serokell/serokell-util/issues/54):
  Change the return type of formatting functions in `Verify`
  to `Text` instead of `Builder`.

0.7.0
=====

* Bump up dependencies to `universum >= 1.1.0` and `o-clock >= 0.1.0`.
* [#46](https://github.com/serokell/serokell-util/issues/46):
  Add `Group` module with grouping functions.
* [#28](https://github.com/serokell/serokell-util/issues/28):
  Use `NonEmpty` list in `VerificationRes` instead of ordinary list.
* [#22](https://github.com/serokell/serokell-util/issues/22):
  Remove `Util.Time` module, remove `threadDelay` function.
  Move to `o-clock` from `time-units` in `Util.Bench` module.
  `Util.Bench` now is not supported in `ghc` `< 8.2.2`.

0.6.0
=====

* [#23](https://github.com/serokell/serokell-util/issues/23):
  Remove `Serokell.AcidState.*` modules and `acid-state` with `safecopy` from dependencies.
* [#25](https://github.com/serokell/serokell-util/issues/25):
  Add Travis CI. Upgrade to `universum-1.0.2` and `log-warper-1.8.5`.

0.5.4
=====

* Generalize `listL`.
* TODO

0.5.3
=====

* Bump to the newest versions, fix the haddock build.

0.5.2
=====

* Added `listChunkedJson`.

0.5.1
=====

* Added `colorizeDull`.


[1]: https://github.com/serokell/aeson-options
