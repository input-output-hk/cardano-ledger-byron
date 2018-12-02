{ system
, compiler
, flags
, pkgs
, hsPkgs
, pkgconfPkgs
, ... }:
  {
    flags = {};
    package = {
      specVersion = "2.0";
      identifier = {
        name = "cs-ledger";
        version = "0.1.0.0";
      };
      license = "MIT";
      copyright = "";
      maintainer = "formal.methods@iohk.io";
      author = "IOHK Formal Methods Team";
      homepage = "https://github.com/input-output-hk/cardano-chain";
      url = "";
      synopsis = "Executable specification of Cardano ledger";
      description = "";
      buildType = "Simple";
    };
    components = {
      sublibs = {
        "utxo-sig" = {
          depends  = [
            (hsPkgs.base)
            (hsPkgs.bytestring)
            (hsPkgs.containers)
            (hsPkgs.cryptonite)
            (hsPkgs.memory)
            (hsPkgs.text)
          ];
        };
        "utxo-impl-simple" = {
          depends  = [
            (hsPkgs.base)
            (hsPkgs.cryptonite)
            (hsPkgs.memory)
          ];
        };
        "ledger-simple" = {
          depends  = [
            (hsPkgs.base)
            (hsPkgs.bytestring)
            (hsPkgs.containers)
            (hsPkgs.cryptonite)
            (hsPkgs.memory)
            (hsPkgs.small-steps)
            (hsPkgs.utxo-sig)
            (hsPkgs.utxo-impl-simple)
          ];
        };
        "ledger-witnessed" = {
          depends  = [
            (hsPkgs.base)
            (hsPkgs.containers)
            (hsPkgs.lens)
            (hsPkgs.small-steps)
            (hsPkgs.utxo-sig)
            (hsPkgs.utxo-impl-simple)
            (hsPkgs.ledger-simple)
          ];
        };
      };
    };
  } // rec {
    src = ../specs/ledger/hs;
  }
