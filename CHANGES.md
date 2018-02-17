0.7.0
=====

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
