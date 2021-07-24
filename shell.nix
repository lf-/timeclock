let sources = import ./nix/sources.nix;
in
{ nixpkgs ? import sources.nixpkgs { }, compiler ? "ghc8104" }:
let hsPkgs = nixpkgs.haskell.packages."${compiler}";
in
hsPkgs.shellFor {
  packages = p: [ (import ./default.nix { inherit compiler; }) ];
  nativeBuildInputs = with hsPkgs; [
    cabal-install
    Cabal
    haskell-language-server
  ];

  withHoogle = true;
}
