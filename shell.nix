{ pkgs ? import /home/wavewave/tmp/test/nixpkgs/nixpkgs-f461c933a1dbfc19b22f302af30869e8b303f56a {}

}: # (import <nixpkgs>{}) }:

with pkgs;

let hsenv = haskellPackages.ghcWithPackages (p: with p; [
              random
              text
              unordered-containers
              compact
              configurator
              dhall-json
              HStringTemplate
              fgl
              #FileManip
              hslogger
              HUnit
              MissingH
              paths
              safe
              regex-base
              regex-compat
              regex-posix
              old-time
              strict
              unix-compat
              #xml-conduit split unordered-containers
              #free gtk2hs-buildtools
              #zlib
              #split
              #data-default
              #Chart
              #Chart-cairo
            ]);
in stdenv.mkDerivation {
     name = "ghc-shell";
     buildInputs = [ hsenv ]; # gd expat pkgconfig cairo glib gtk3 librsvg poppler libxml2 ];
     shellHook = ''
     '';
   }
