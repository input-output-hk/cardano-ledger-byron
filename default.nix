################################################################################
# See `skeleton` project at https://github.com/input-output-hk/iohk-nix/
################################################################################

{ system ? builtins.currentSystem
, crossSystem ? null
, config ? {}
# Import IOHK common nix lib
, iohkLib ? import ./nix/iohk-common.nix { inherit system crossSystem config; }
# Use nixpkgs pin from iohkLib
, pkgs ? iohkLib.pkgs
}:

let
  haskell = pkgs.callPackage iohkLib.nix-tools.haskell {};
  src = iohkLib.cleanSourceHaskell ./.;
  util = pkgs.callPackage ./nix/util.nix {};

  # Import the Haskell package set.
  haskellPackages = import ./nix/pkgs.nix {
    inherit pkgs haskell src;
    # Provide cross-compiling secret sauce
    inherit (iohkLib.nix-tools) iohk-extras iohk-module;
  };

in {
  inherit pkgs iohkLib src haskellPackages;
  inherit (haskellPackages.cardano-ledger.identifier) version;

  tests = util.collectComponents "tests" util.isCardanoLedger haskellPackages;
  benchmarks = util.collectComponents "benchmarks" util.isCardanoLedger haskellPackages;

  # This provides a development environment that can be used with nix-shell or
  # lorri. See https://input-output-hk.github.io/haskell.nix/user-guide/development/
  shell = haskellPackages.shellFor {
    name = "cardano-ledger-shell";
    # List all local packages in the project.
    packages = ps: with ps; [
      cardano-ledger
    ];
    # These programs will be available inside the nix-shell.
    buildInputs =
      with pkgs.haskellPackages; [ hlint stylish-haskell weeder ghcid lentil ]
      # You can add your own packages to the shell.
      ++ [  ];
  };

}
