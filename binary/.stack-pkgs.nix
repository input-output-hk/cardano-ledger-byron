{
  extraDeps = hsPkgs:
    {
      "aeson-options" = hsPkgs.aeson-options."0.0.0";
      "base58-bytestring" = hsPkgs.base58-bytestring."0.1.0";
      "half" = hsPkgs.half."0.2.2.3";
      "micro-recursion-schemes" = hsPkgs.micro-recursion-schemes."5.0.2.2";
      "pvss" = hsPkgs.pvss."0.2.0";
    };
  packages = hsPkgs:
    {
      cborg = ./.stack.nix/cborg.nix;
      canonical-json = ./.stack.nix/canonical-json.nix;
      cardano-crypto = ./.stack.nix/cardano-crypto.nix;
      plutus-prototype = ./.stack.nix/plutus-prototype.nix;
      hedgehog = ./.stack.nix/hedgehog.nix;
      cardano-binary = ./.stack.nix/cardano-binary.nix;
      cardano-binary-test = ./.stack.nix/cardano-binary-test.nix;
      cardano-prelude = ./.stack.nix/cardano-prelude.nix;
      cardano-prelude-test = ./.stack.nix/cardano-prelude-test.nix;
    };
  resolver = "lts-12.10";
}
