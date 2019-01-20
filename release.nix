let
  localLib = import ./lib.nix;
in
{ system ? builtins.currentSystem
, config ? import ./nix/config.nix
, pkgs ? localLib.iohkNix.getPkgs { inherit system config; }

, chain ? { outPath = ./.; rev = "abcdef"; }

, scrubJobs ? true
, supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ]
, nixpkgsArgs ? {
    config = config // { allowUnfree = false; inHydra = true; };
  }
}:
with (import (localLib.iohkNix.nixpkgs + "/pkgs/top-level/release-lib.nix") {
  inherit supportedSystems scrubJobs nixpkgsArgs;
  packageSet = import ./.;
});
with pkgs.lib;
let
  packages = [ "cardano-chain" "cs-ledger" "small-steps" "cs-blockchain" ];

  traceId = x: builtins.trace (builtins.deepSeq x x) x;

  packageSet = import ./. {};
  nix-tools-pkgs = supportedSystems: {
    nix-tools.libs =
      mapAttrs (_: _: supportedSystems)
        (filterAttrs (n: v: builtins.elem n packages && v != null) packageSet.nix-tools.libs);
    nix-tools.exes =
      mapAttrs (_: mapAttrs (_: _: supportedSystems))
        (filterAttrs (n: v: builtins.elem n packages && v != null) packageSet.nix-tools.exes);
    nix-tools.tests =
      mapAttrs (_: mapAttrs (_: _: supportedSystems))
        (filterAttrs (n: v: builtins.elem n packages && v != null) packageSet.nix-tools.tests);
    nix-tools.benchmarks =
      mapAttrs (_: mapAttrs (_: _: supportedSystems))
        (filterAttrs (n: v: builtins.elem n packages && v != null) packageSet.nix-tools.benchmarks);
  };

  mapped-pkgs = mapTestOn (nix-tools-pkgs supportedSystems);
  mapped-pkgs-mingw32 = mapTestOnCross lib.systems.examples.mingwW64 (nix-tools-pkgs [ "x86_64-linux" "x86_64-darwin" ]);

  mapped-pkgs-all
    = lib.recursiveUpdate
        (mapped-pkgs)
        (lib.mapAttrs (_: (lib.mapAttrs (_: (lib.mapAttrs' (n: v: lib.nameValuePair (lib.systems.examples.mingwW64.config + "-" + n) v)))))
          mapped-pkgs-mingw32);


  latex-specs = {
    byronLedgerSpec = import ./specs/ledger/latex {};
    byronChainSpec = import ./specs/chain/latex {};
    semanticsSpec = import ./specs/semantics/latex {};
   };

in fix (self: latex-specs // mapped-pkgs-all
// {
  forceNewEval = pkgs.writeText "forceNewEval" chain.rev;

  required = pkgs.lib.hydraJob (pkgs.releaseTools.aggregate {
    name = "cardano-chain-required-checks";
    constituents = with self;
      [ byronLedgerSpec byronChainSpec semanticsSpec
        forceNewEval
        nix-tools.libs.cardano-chain.x86_64-linux
        nix-tools.libs.cardano-chain.x86_64-darwin
        nix-tools.tests.cardano-chain.cardano-chain-test.x86_64-linux
        nix-tools.tests.cardano-chain.cardano-chain-test.x86_64-darwin
      ];
  });

})
