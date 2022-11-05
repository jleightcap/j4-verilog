with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "j4";
  nativeBuildInputs = [
    verilog verilator
    haskellPackages.ghc
    haskellPackages.hindent haskellPackages.haskell-language-server
  ];
  src = ./.;
}
