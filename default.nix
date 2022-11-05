{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/4f09cfce9c1d54fb56b65125061a632849de1a49.tar.gz") {}
}:
with pkgs; stdenv.mkDerivation {
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
