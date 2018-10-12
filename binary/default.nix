{ pkgs ? import <nixpkgs> {} }:
let
  base = import ./pkgs.nix { inherit pkgs; };
  overlays = [ (self: super: with pkgs.haskell.lib; {
      # you can explicilty mark packages as dontCheck.
      # e.g. if they depend on doctest or lead to cyclic
      # dependencies.
      #
      #  aeson = dontCheck super.aeson;
      nanospec = dontCheck super.nanospec;
      hspec = dontCheck super.hspec;
      time = dontCheck super.time;
      containers = dontCheck super.containers;
      directory = dontCheck super.directory;
      aeson = dontCheck super.aeson;
      attoparsec = dontCheck super.attoparsec;
      diretory = dontCheck super.directory;
      unix = dontCheck super.unix;
      bytestring = dontCheck super.bytestring;
      xml = dontCheck super.xml;
      text = dontCheck super.text;
      binary = dontCheck super.binary;
      scientific = dontCheck super.scientific;
      async = dontCheck super.async;
      test-framework = dontCheck super.test-framework;
      clock = dontCheck super.clock;
      tasty = dontCheck super.tasty;
      integer-logarithms = dontCheck super.integer-logarithms;
      tar = dontCheck super.tar;
      Cabal = dontCheck super.Cabal;
      dlist = dontCheck super.dlist;

      # require doctest
      universum = dontCheck super.universum;
      distributive = dontCheck super.distributive;
      comonad = dontCheck super.comonad;
      semigroupoids = dontCheck super.semigroupoids;
      lens = dontCheck super.lens;

      # no haddock files
      nats = dontHaddock super.nats;
    }) ];
in
  builtins.foldl' (pkgs: overlay: pkgs.extend overlay) base overlays
