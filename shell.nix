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
    lib.mapAttrs (_: haskell.lib.disableLibraryProfiling) {
      clash-shake = self.callCabal2nix "" (/home/j/src/clash-shake) { };
      # pkgs.fetchFromGitHub {
      # owner = "gergoerdi";
      # repo = "clash-shake";
      # rev = "ed20d982c034a285f18403b76eb9d511524649fb"; # master
      # sha256 = "1n0cdfmblasl4bnsa3sv7x7h790r41pf1dhkgi2p31mkn9mzc5rp";
      # }
      clash-ghc = self.callCabal2nix "" (clashSrc + "/clash-ghc") { };
      clash-lib = self.callCabal2nix "" (clashSrc + "/clash-lib") { };
      clash-prelude = self.callCabal2nix "" (clashSrc + "/clash-prelude") { };
    });

  clashSrc = pkgs.fetchFromGitHub {
    owner = "expipiplus1";
    repo = "clash-compiler";
    rev = "8fcb2c2e857d1fc8ca92a33113b38b9d1d1572ac"; # joe-check
    sha256 = "07pbccvv0vf4nzk2hv7vx0za5mh72fxfj13a0b8zj9s13k08bh51";
  };

  hask = haskellPackages.ghcWithHoogle
    (p: with p; [ clash-shake clash-ghc clash-prelude ]);
in mkShell {
  nativeBuildInputs = [ yosys symbiyosys nextpnr trellis openocd ecpdap hask ];
}

