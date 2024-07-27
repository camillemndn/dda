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

  pre-commit-hook = (import inputs.git-hooks).run {
    src = ./.;
    hooks.commitizen.enable = true;
  };
in
rec {
  devShells.default = pkgs.mkShell {
    buildInputs = with pkgs; [
      (quarto.override { extraRPackages = r-deps pkgs.rPackages; })
      (rWrapper.override { packages = r-deps pkgs.rPackages ++ [ packages.x86_64-linux.dda ]; })
      (rstudioWrapper.override { packages = r-deps pkgs.rPackages; })
      texliveFull
      npins
    ];
    shellHook = ''
      ${pre-commit-hook.shellHook}
      Rscript -e "devtools::document()"
    '';
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

    website = pkgs.callPackage (
      {
        stdenv,
        rWrapper,
        rPackages,
        ...
      }:

      stdenv.mkDerivation {
        name = "dda-website";
        src = ./.;
        buildInputs = [ (rWrapper.override { packages = r-deps rPackages; }) ];
        HOME = ".";

        buildPhase = ''
          Rscript -e "options(pkgdown.internet = FALSE); pkgdown::build_site()"
        '';

        installPhase = ''
          cp -r docs $out
        '';
      }
    ) { };

    dda-vignette-ics-climate-change = pkgs.callPackage (
      {
        stdenv,
        rWrapper,
        rPackages,
        ...
      }:

      stdenv.mkDerivation {
        name = "dda-vignette-ics-climate-change";
        src = ./.;
        buildInputs = [ (rWrapper.override { packages = r-deps rPackages; }) ];
        HOME = ".";

        buildPhase = ''
          (
            cd vignettes
            Rscript -e "devtools::load_all(); source(knitr::purl('ICS_climate_change.qmd', quiet=TRUE))"
          )
        '';

        installPhase = ''
          mkdir $out
          cp -r vignettes $out
        '';
      }
    ) { };

    dda-vignette-ics-grid = pkgs.callPackage (
      {
        stdenv,
        rWrapper,
        rPackages,
        ...
      }:

      stdenv.mkDerivation {
        name = "dda-vignette-ics-grid";
        src = ./.;
        buildInputs = [ (rWrapper.override { packages = r-deps rPackages; }) ];
        HOME = ".";

        buildPhase = ''
          (
            cd vignettes
            Rscript -e "devtools::load_all(); source(knitr::purl('ICS_grid.Rmd', quiet=TRUE))"
          )
        '';

        installPhase = ''
          mkdir $out
          cp -r vignettes $out
        '';
      }
    ) { };
  };

  checks.default = {
    inherit packages;
  };
}
