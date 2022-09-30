with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "j1";
  nativeBuildInputs = [
    verilog verilator
    ghc
    haskellPackages.hindent haskell-language-server
  ];
  src = ./.;
}
