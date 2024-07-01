let
  inputs = import ./deps;
  system = "x86_64-linux";
  pkgs = import (inputs.nixpkgs) { inherit system; };

  r-deps = with pkgs.rPackages; [
    devtools
    fdANOVA
    fda
    fda_usc
    mvnfast
    refund
    robCompositions
    compositions
    ICS
    ICSOutlier
    corrplot
    xts
    ellipse
    whitening
    memoise
    sf

    ggExtra
    GGally
    plotly
    tidyverse
    svglite

    pkgdown
    sinew
    languageserver
    viridis
    quarto
    (buildRPackage {
      name = "colorout";
      src = pkgs.fetchFromGitHub {
        owner = "jalvesaq";
        repo = "colorout";
        rev = "v1.2-2";
        sha256 = "sha256-49avzqJNajVPcj8+Ax4/tv/2196bKSi6YeOoK3kyXec=";
      };
    })
  ];
in
rec {
  devShells.default = pkgs.mkShell {
    nativeBuildInputs = [ pkgs.bashInteractive ];
    buildInputs = with pkgs; [
      (quarto.override { extraRPackages = r-deps; })
      (rWrapper.override { packages = r-deps; })
      (rstudioWrapper.override { packages = r-deps; })
      texliveFull
      npins
    ];
  };

  packages = { };

  checks.default = {
    inherit packages;
    r-cmd-check = pkgs.callPackage (
      { stdenv, rWrapper, ... }:
      stdenv.mkDerivation {
        pname = "dda";
        version = "0.0.0.9010";
        src = ./.;
        buildInputs = [ (rWrapper.override { packages = r-deps; }) ];

        buildPhase = ''
          R CMD build . && R CMD check $(ls -t . | head -n1)
        '';
      }
    ) { };
  };
}
