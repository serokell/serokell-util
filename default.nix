{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, acid-state, base, exceptions, lens, mtl
      , optparse-applicative, stdenv, template-haskell, text, text-format
      , transformers
      }:
      mkDerivation {
        pname = "serokell-core";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [
          acid-state base exceptions lens mtl optparse-applicative
          template-haskell text text-format transformers
        ];
        homepage = "http://gitlab.serokell.io/serokell-team/serokell-core";
        description = "General-purpose functions by Serokell";
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
