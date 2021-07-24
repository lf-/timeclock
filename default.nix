let sources = import ./nix/sources.nix;
in
{ nixpkgs ? import sources.nixpkgs { }, compiler ? "ghc8104" }:
nixpkgs.haskell.packages.${compiler}.callCabal2nix "timeclock" ./. { }
