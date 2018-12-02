{ chain ? { outPath = ./.; rev = "abcdef"; } }:
let
  pkgs = import ./pkgs.nix;
  nix-tools-pkgs = import ./nix/pkgs.nix { nixpkgs = _: pkgs; };
in pkgs.lib.fix (self: {
  inherit (nix-tools-pkgs) stack-pkgs spec-pkgs;
  forceNewEval = pkgs.writeText "forceNewEval" chain.rev;

  required = pkgs.lib.hydraJob (pkgs.releaseTools.aggregate {
    name = "cardano-chain-required-checks";
    constituents = with self; [ byronLedgerSpec byronChainSpec semanticsSpec forceNewEval ]
               ++ [ stack-pkgs.cardano-chain.components.library
                    stack-pkgs.cardano-chain-test.components.library
                    stack-pkgs.cardano-binary.components.library
                    stack-pkgs.cardano-binary-test.components.library
                    stack-pkgs.cardano-crypto.components.library
                    stack-pkgs.cardano-crypto-wrapper.components.library
                    stack-pkgs.cardano-crypto-test.components.library
                    stack-pkgs.cardano-prelude.components.library
                    stack-pkgs.cardano-prelude-test.components.library
                    stack-pkgs.plutus-prototype.components.library
                  ]
               ++ [ spec-pkgs.cs-blockchain.components.library
                    spec-pkgs.cs-ledger.components.library
                    spec-pkgs.small-steps.components.library
                  ];
  });

  byronLedgerSpec = import ./specs/ledger/latex {};
  byronChainSpec = import ./specs/chain/latex {};
  semanticsSpec = import ./specs/semantics/latex {};
})
