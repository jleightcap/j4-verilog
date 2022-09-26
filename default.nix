with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "j1";
  nativeBuildInputs = [ verilog verilator ];
  src = ./.;
}
