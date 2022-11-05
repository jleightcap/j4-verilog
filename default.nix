with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "j4";


  nativeBuildInputs = [
    verilog
  ] ++ (with haskellPackages; [
    ghc
  ]);

  buildInputs = [
    verilator
  ] ++ (with haskellPackages; [
    haskell-language-server
    hindent
    hlint
  ]);

  src = ./.;
}
