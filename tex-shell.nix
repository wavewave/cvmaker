{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let
 texenv = texlive.combine { 
            inherit (texlive) scheme-small collection-publishers;
            inherit (texlive) collection-fontsrecommended;
            inherit (texlive) soul xytree xypic palatino helvetic standalone;
            inherit (texlive) tikz-dependency environ trimspaces;
            inherit (texlive) enumitem;
          };
in 
stdenv.mkDerivation { 
  name = "tex-env";
  buildInputs = [ texenv graphviz ];

}
