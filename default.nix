let
  inputs = import ./deps;
  system = "x86_64-linux";
  pkgs = import (inputs.nixpkgs) { inherit system; };

  r-deps =
    ps: with ps; [
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
      (quarto.override { extraRPackages = r-deps pkgs.rPackages; })
      (rWrapper.override { packages = r-deps pkgs.rPackages ++ [ packages.x86_64-linux.dda ]; })
      (rstudioWrapper.override { packages = r-deps pkgs.rPackages; })
      texliveFull
      npins
    ];
  };

  packages.x86_64-linux = {
    dda = pkgs.callPackage (
      { rPackages, ... }:
      rPackages.buildRPackage {
        name = "dda";
        src = ./.;
        propagatedBuildInputs = r-deps rPackages;
      }
    ) { };

    dda-website = pkgs.callPackage (
      {
        stdenv,
        rWrapper,
        rPackages,
        ...
      }:

      stdenv.mkDerivation {
        pname = "dda-website";
        version = "0.0.0.9010";
        src = ./.;
        buildInputs = [ (rWrapper.override { packages = r-deps rPackages; }) ];
        HOME = ".";

        buildPhase = ''
          Rscript -e "options(pkgdown.internet = FALSE); pkgdown::build_site()"
        '';

        installPhase = ''
          mkdir $out
          cp -r docs $out
        '';
      }
    ) { };
  };

  checks.default = {
    inherit packages;
  };
}
