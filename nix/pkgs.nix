{ args ? { config = import ./config.nix; }
, nixpkgs ? import <nixpkgs>
}:
let
  pkgs = nixpkgs args;
in
let

  inherit (import ./haskell-hackage-stackage.nix { inherit pkgs; }) haskell hackage stackage;

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
    # The overlay allows extension or restriction of the set of
    # packages we are interested in. By using the stack-pkgs.overlay
    # we restrict our package set to the ones provided in stack.yaml.
    pkg-def-overlays = [
      stack-pkgs.overlay
      (hackage: {
          hsc2hs = hackage.hsc2hs."0.68.4".revisions.default;
          # stackage beautifully omitts the Win32 pkg
          Win32 = hackage.Win32."2.6.2.0".revisions.default;
      })
    ];
    # package customizations
    modules = [
      haskell.ghcHackagePatches.${(stack-pkgs.overlay hackage).compiler.nix-name}
      {
        # clock hasn't had a release since 2016(!) that is for three(3) years
        # now.
        packages.clock.patches = [ ./patches/clock-0.7.2.patch ];
      }
    ];
  };
in
  pkgSet.config.hsPkgs // { _config = pkgSet.config; }
