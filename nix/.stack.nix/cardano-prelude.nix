{ system
, compiler
, flags
, pkgs
, hsPkgs
, pkgconfPkgs
, ... }:
  {
    flags = {
      development = false;
    };
    package = {
      specVersion = "1.10";
      identifier = {
        name = "cardano-prelude";
        version = "0.1.0.0";
      };
      license = "MIT";
      copyright = "2018 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "A Prelude replacement for the Cardano project";
      description = "A Prelude replacement for the Cardano project";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends  = [
          (hsPkgs.base)
          (hsPkgs.aeson)
          (hsPkgs.base16-bytestring)
          (hsPkgs.bytestring)
          (hsPkgs.canonical-json)
          (hsPkgs.cborg)
          (hsPkgs.containers)
          (hsPkgs.formatting)
          (hsPkgs.hashable)
          (hsPkgs.mtl)
          (hsPkgs.protolude)
          (hsPkgs.tagged)
          (hsPkgs.text)
          (hsPkgs.time)
        ];
      };
    };
  } // {
    src = pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-prelude";
      rev = "0c3c12ff4b95644c22731f979be0e2e6ebb8fe63";
      sha256 = "16cgbhmhczv616rimzx7gqxi6dlldmfh5jwslfhh3q04lp2m97qr";
    };
  }