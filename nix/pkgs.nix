{ args ? { config = import ./config.nix; }
, nixpkgs ? import <nixpkgs>
}:
let
  pkgs = nixpkgs args;
  overrideWith = override: default:
   let
     try = builtins.tryEval (builtins.findFile builtins.nixPath override);
   in if try.success then
     builtins.trace "using search host <${override}>" try.value
   else
     default;
in
let
  # save the nixpkgs value in pkgs'
  # so we can work with `pkgs` provided by modules.
  pkgs' = pkgs;
  # all packages from hackage as nix expressions
  hackage = import (overrideWith "hackage"
                    (pkgs.fetchFromGitHub { owner  = "angerman";
                                            repo   = "hackage.nix";
                                            rev    = "d8e03ec0e3c99903d970406ae5bceac7d993035d";
                                            sha256 = "0c7camspw7v5bg23hcas0r10c40fnwwwmz0adsjpsxgdjxayws3v";
                                            name   = "hackage-exprs-source"; }))
                   ;
  # a different haskell infrastructure
  haskell = import (overrideWith "haskell"
                    (pkgs.fetchFromGitHub { owner  = "angerman";
                                            repo   = "haskell.nix";
                                            rev    = "2a3b2612a15fd7f14d32c3519aba2b64bd7b1e43";
                                            sha256 = "181dv1zlf381kkb82snjmpibhgmkyw1n5qsvpqjrv8dxmcjqjl2k";
                                            name   = "haskell-lib-source"; }))
                   hackage;

  # the set of all stackage snapshots
  stackage = import (overrideWith "stackage"
                     (pkgs.fetchFromGitHub { owner  = "angerman";
                                             repo   = "stackage.nix";
                                             rev    = "67675ea78ae5c321ed0b8327040addecc743a96c";
                                             sha256 = "1ds2xfsnkm2byg8js6c9032nvfwmbx7lgcsndjgkhgq56bmw5wap";
                                             name   = "stackage-snapshot-source"; }))
                   ;


  ################################################################################
  # S T A C K
  ################################################################################
  # our packages
  stack-pkgs = import ./.stack-pkgs.nix;

  # Build the packageset with module support.
  # We can essentially override anything in the modules
  # section.
  #
  #  packages.cbors.patches = [ ./one.patch ];
  #  packages.cbors.flags.optimize-gmp = false;
  #
  pkgSet = haskell.mkNewPkgSet {
    inherit pkgs;
    pkg-def = stackage.${stack-pkgs.resolver};
    pkg-def-overlays = [
      stack-pkgs.overlay
      (hackage: {
          hsc2hs = hackage.hsc2hs."0.68.4".revisions.default;
          # stackage 12.17 beautifully omitts the Win32 pkg
          Win32 = hackage.Win32."2.6.2.0".revisions.default;
      })
    ];
    modules = [
    # { packages.xxx.flags.yyy = true; }
    ];
  };

  packages = pkgSet.config.hsPkgs // { _config = pkgSet.config; };

  ################################################################################
  # C A B A L (spec folder)
  ################################################################################

  specPkgSet = haskell.mkNewPkgSet {
    inherit pkgs;
    pkg-def = import ./spec-plan.nix;
    pkg-def-overlays = [
      {
        cs-blockchain = ./cs-blockchain.nix;
        cs-ledger = ./cs-ledger.nix;
        small-steps = ./small-steps.nix;
      }
    ];
  };

  spec-packages = specPkgSet.config.hsPkgs // { _config = specPkgSet.config; };

in { stack-pkgs = packages; spec-pkgs = spec-packages; }
