with import ../../nix/lib.nix;
with pkgs;

let

  stack-hpc-coveralls = pkgs.haskell.lib.dontCheck
    (haskellPackages.callPackage ./stack-hpc-coveralls.nix {});

  buildTools =
      [ git nix gnumake stack gnused gnutar coreutils stack-hpc-coveralls
        gzip lz4 haskellPackages.weeder
      ];

  libs = ps: with ps; [turtle safe transformers extra async];

  ghc' = haskellPackages.ghcWithPackages libs;

  stackRebuild = runCommand "stack-rebuild" {
      buildInputs = [ ghc' makeWrapper ];
  } ''
    mkdir -p $out/bin
    ghc -Wall -threaded -o $out/bin/rebuild ${./rebuild.hs}
    wrapProgram $out/bin/rebuild --set PATH "${lib.makeBinPath buildTools}"
  '';

in
  stackRebuild
