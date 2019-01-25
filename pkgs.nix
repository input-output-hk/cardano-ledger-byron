let
  nixpkgs = builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/38db6fdfb9672a0206a2042447132094bc05a5ea.tar.gz";
in
import nixpkgs {
  config = {
    packageOverrides = pkgs: {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          ghc861 = pkgs.haskell.packages.ghc861.override {
            overrides = self: super: {
              basement = self.callHackage "basement" "0.0.8" {};
              aeson = self.callHackage "aeson" "1.3.1.1" {};
              nonempty-containers = self.callCabal2nix "nonempty-containers"
                              (pkgs.fetchFromGitHub { owner = "mstksg";
                                                      repo = "nonempty-containers";
                                                      rev = "27fb9d78269be1b31351fff8ac24268f6a107964";
                                                      sha256 = "1bj95siah7fv4wapzk4p28qx5yvadznc78cnfqcr3whmi1g03vna"; }) {};
              cardano-prelude = self.callCabal2nix "cardano-prelude"
                              (pkgs.fetchFromGitHub { owner = "input-output-hk";
                                                      repo = "cardano-prelude";
                                                      rev = "be0836078ca0c8e521376baf7cf9a01abdd3664b";
                                                      sha256 = "1rdimji8a5v7wcvsc0q1g57hmlxpzm736ilcxspky3wvy55xjab0"; }) {};
              memory = pkgs.haskell.lib.doJailbreak (self.callHackage "memory" "0.14.18" {});
              small-steps = self.callCabal2nix "small-steps" ./specs/semantics/hs {};
              cs-ledger = pkgs.haskell.lib.overrideCabal (self.callCabal2nix "cs-ledger" ./specs/ledger/hs {}) (old: {
                enableParallelBuilding = false;
              });
              cs-blockchain = self.callCabal2nix "cs-blockchain" ./specs/chain/hs {};
            };
          };
        };
      };
    };
  };
}
