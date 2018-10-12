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
      cardano-crypto-wrapper = ./.stack.nix/cardano-crypto-wrapper.nix;
      cardano-crypto-test = ./.stack.nix/cardano-crypto-test.nix;
    };
  resolver = "lts-12.10";
}
