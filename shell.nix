{ nixpkgsSrc ? builtins.fetchTarball {
  url =
    "https://github.com/nixos/nixpkgs/archive/17af7a98d2e7dfdfdae5292b8dbd7bab28eeaf8d.tar.gz"; # refs/heads/master
  sha256 = "11yk5q0jk7l30siv28b8qhb4s55ypmif6mp2nv90mwq1c6n11p1x";
}, pkgs ? import nixpkgsSrc { }, compiler ? null }:

with pkgs;

let
  baseHaskellPackages = if compiler == null then
    pkgs.haskellPackages
  else
    pkgs.haskell.packages.${compiler};
  haskellPackages = baseHaskellPackages.extend (self: _super:
    lib.mapAttrs
    (_: p: with haskell.lib; dontCheck (disableLibraryProfiling p)) {
      clash-shake = self.callCabal2nix "" clashShakeSrc { };
      clash-ghc = self.callCabal2nix "" (clashSrc + "/clash-ghc") { };
      clash-lib = self.callCabal2nix "" (clashSrc + "/clash-lib") { };
      clash-prelude = self.callCabal2nix "" (clashSrc + "/clash-prelude") { };
    });

  # clashSrc = /home/j/src/clash-compiler;
  clashSrc = pkgs.fetchFromGitHub {
    owner = "expipiplus1";
    repo = "clash-compiler";
    rev = "a41d61c9109357e034691896798ebfc1b83b24af"; # joe-yosys-sva
    sha256 = "0c5f201h5cfr8zibrjki9v791fipqd2b9rs6n5ck003wv50nsvsr";
  };

  clashShakeSrc = /home/j/src/clash-shake;
  # clashShakeSrc = pkgs.fetchFromGitHub {
  #   owner = "expipiplus1";
  #   repo = "clash-shake";
  #   rev = "c12baa61050a44152d6e8aaa5b27f30d4753e5d2"; # sf-ecp5
  #   sha256 = "14mp4qg2k8z1bqhhzggjww2fw72vzdkrgglqba90897mvssw43zh";
  # };

  hask = haskellPackages.ghcWithHoogle
    (p: with p; [ clash-shake clash-ghc clash-prelude ]);
in mkShell {
  nativeBuildInputs =
    [ ghdl yosys yosys-ghdl z3 symbiyosys nextpnr trellis openocd ecpdap hask ];
}

