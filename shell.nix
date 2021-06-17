{ nixpkgsSrc ? builtins.fetchTarball {
  url =
    "https://github.com/nixos/nixpkgs/archive/fa0326ce5233f7d592271df52c9d0812bec47b84.tar.gz"; # refs/heads/nixos-unstable
  sha256 = "1rzgjhzp5gnd49fl123cbd70zl4gmf7175150aj51h796mr7aah3";
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

  clashSrc = pkgs.fetchFromGitHub {
    owner = "expipiplus1";
    repo = "clash-compiler";
    rev = "b493ff8f07c902dcb0c904d76978f28460638c89"; # joe-yosys-sva
    sha256 = "1ln0y7ys7x9qgl1qqhs7k7cxclr9l9sk3sxg32pv9npmdwg5vmcq";
  };

  clashShakeSrc = pkgs.fetchFromGitHub {
    owner = "expipiplus1";
    repo = "clash-shake";
    rev = "dcee5b0e5e1c8a8af177bb0b5465de8835c05a8e"; # sf-ecp5
    sha256 = "07z01dd0pmz1x9n1z4kzjix18zids5gs5f5jzhkzsx53qg783lzd";
  };

  hask = haskellPackages.ghcWithHoogle
    (p: with p; [ clash-shake clash-ghc clash-prelude ]);
in mkShell {
  nativeBuildInputs =
    [ ghdl yosys yosys-ghdl z3 symbiyosys nextpnr trellis openocd ecpdap hask ];
}

