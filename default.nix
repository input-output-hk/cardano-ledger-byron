let
  # an import like statement that allows to provide a default value
  # if the import fails.
  tryImport = p: d: if builtins.pathExists p then import p else d;
in
{ system ? builtins.currentSystem
, config ? tryImport ./config.nix
, nixpkgs ? <nixpkgs>
, pkgs ? (import nixpkgs { inherit system config; })
}:
let

  binary = import ./binary { inherit pkgs; };
  crypto = import ./crypto  { inherit pkgs; };
  prelude = import ./prelude { inherit pkgs; };

in binary // crypto // prelude
