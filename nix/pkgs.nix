{ pkgs
, src
, haskellCompiler ? "ghc865"
, profiling ? false
}:
let
  haskell = pkgs.haskell-nix;

  # The cardano-mainnet-mirror used during testing
  cardano-mainnet-mirror = import ./cardano-mainnet-mirror.nix {inherit pkgs;};

  exe-extension =
    pkgs.lib.optionalString pkgs.stdenv.targetPlatform.isWindows ".exe";

  recRecurseIntoAttrs = with pkgs; pred: x: if pred x then recurseIntoAttrs (lib.mapAttrs (n: v: if n == "buildPackages" then v else recRecurseIntoAttrs pred v) x) else x;
  pkgSet = recRecurseIntoAttrs (x: with pkgs; lib.isAttrs x && !lib.isDerivation x)
    # we are only intersted in listing the project packages
    (pkgs.lib.filterAttrs (with pkgs.haskell-nix.haskellLib; (n: p: p != null && (isLocalPackage p && isProjectPackage p) || n == "shellFor"))
      # from our project which is based on a cabal project.
      (pkgs.haskell-nix.cabalProject {
          src = pkgs.haskell-nix.haskellLib.cleanGit { inherit src; };
          ghc = pkgs.haskell-nix.compiler.${haskellCompiler};
          configureArgs = "-ftest-normal-form";
          modules = [
      {
        # katip has an version bound of Win32 < 2.6; this however
        # implies that it's incompatible with ghc-8.6 (on windows).
        # Let's force it to accept out packageset.
        packages.katip.doExactConfig = true;
        packages.ekg-core.doExactConfig = true;

        # The generated iohk-monitoring.nix doesn't turn off prometheus yet,
        # because it doesn't respect the flags section of stack.yaml
        packages.iohk-monitoring.flags.disable-prometheus = true;

        packages.cardano-ledger.preBuild =
          "export CARDANO_MAINNET_MIRROR=${cardano-mainnet-mirror}/epochs";

        packages.cardano-ledger.flags.test-normal-form = true;

        packages.cardano-ledger.components.all.postInstall = pkgs.lib.mkForce "";

        packages.cardano-ledger.components.tests.cardano-ledger-test = {
          build-tools = [ pkgs.makeWrapper ];
          testFlags = [ "--scenario=ContinuousIntegration" ];
          postInstall = ''
            makeWrapper \
              $out/bin/cardano-ledger-test${exe-extension} \
              $out/bin/cardano-ledger-test${exe-extension} \
              --set CARDANO_MAINNET_MIRROR ${cardano-mainnet-mirror}/epochs
          '';
        };
      }
          ];
      }));
 in pkgSet
