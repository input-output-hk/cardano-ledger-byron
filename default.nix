let
  localLib = import ./lib.nix;
in
{ system ? builtins.currentSystem
, crossSystem ? null
, config ? {}
# TODO: for cross to actually start working iohk-nix needs to support
#       crossSystem.
, pkgs ? localLib.iohkNix.getPkgs { inherit system config; }
}:
with builtins; with pkgs.lib;
let  nix-tools = import ./nix/pkgs.nix { nixpkgs = _: pkgs; };
in {
    _iohk = localLib.iohkNix;

    # This will allow us to build
    # nix-tools.libs.cardano-chain to obtain all libs in a single derivation
    # nix-tools.exes.cardano-chain for all executables, same for tests and benchmarks.
    #
    # The alternative syntax is: nix-tools._raw.cardano-chain.components.$comp
    # if you want to build only a single component.
    nix-tools = { _raw = nix-tools; }
      # some shorthands
      // { libs = mapAttrs (k: v: if   v ? components && v.components ? "library"
                                  then v.components.library
                                  else null) nix-tools; }
      // { exes = mapAttrs (k: v: if   (v ? components && length (attrValues v.components.exes) > 0)
                                  then if pkgs.stdenv.targetPlatform.isWindows then pkgs.copyJoin else pkgs.symlinkJoin
                                       { name = "${k}-exes"; paths = attrValues v.components.exes; }
                                  else null) nix-tools; }
      // { tests = mapAttrs (k: v: if v ? components && length (attrValues v.components.tests) > 0
                                   then v.components.tests
                                   else null) nix-tools; }
      // { benchmarks = mapAttrs (k: v: if v ? components && length (attrValues v.components.benchmarks) > 0
                                   then v.components.benchmarks
                                   else null) nix-tools; }
      ;
  }
