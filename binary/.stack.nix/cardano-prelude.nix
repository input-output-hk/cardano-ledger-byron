{ system
, compiler
, flags ? {}
, pkgs
, hsPkgs
, pkgconfPkgs }:
  let
    _flags = {
      development = false;
    } // flags;
  in {
    flags = _flags;
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
      "cardano-prelude" = {
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
          (hsPkgs.tagged)
          (hsPkgs.text)
          (hsPkgs.time)
          (hsPkgs.universum)
        ];
      };
    };
  } // rec {
    src = ../../prelude;
  }