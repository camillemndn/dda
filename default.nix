let
  inputs = import ./deps;
  system = "x86_64-linux";
  pkgs = import inputs.nixpkgs { inherit system; };

  r-import-deps =
    ps: with ps; [
      fda
      GGally
      ggplot2
      ICS
      ICSOutlier
      memoise
    ];

  r-suggest-deps =
    ps:
    with ps;
    r-import-deps ps
    ++ [
      corrplot
      fda_usc
      sf
      tidyverse
    ];

  r-dev-deps =
    ps:
    with ps;
    r-suggest-deps ps
    ++ [
      (buildRPackage {
        name = "colorout";
        src = pkgs.fetchFromGitHub {
          owner = "jalvesaq";
          repo = "colorout";
          rev = "v1.3-1";
          hash = "sha256-jOb5Cidyi1cYN8X6XE+YyUg/2mRuMmgdSiYJbDMDsf8=";
        };
      })
      devtools
      languageserver
      pkgdown
      quarto
      sinew
      svglite
    ];

  r-deps =
    ps:
    with ps;
    r-dev-deps ps
    ++ [
      compositions
      fdANOVA
      mvnfast
      robCompositions
      xts
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

  packages.x86_64-linux =
    let
      buildVignette =
        {
          file,
          name,
          stdenv,
          rWrapper,
          rPackages,
          ...
        }:
        stdenv.mkDerivation {
          inherit name;
          src = builtins.fetchGit ./.;

          buildInputs = [ (rWrapper.override { packages = r-dev-deps rPackages; }) ];
          HOME = ".";

          buildPhase = ''
            (
              cd vignettes
              Rscript -e "devtools::load_all(); source(knitr::purl('${file}', quiet=TRUE))"
            )
          '';

          installPhase = ''
            mkdir -p $out
            cp -r vignettes $out
          '';
        };

      vignettes = builtins.filter (file: builtins.match "^.*\\.(Rmd|qmd)$" file != null) (
        builtins.attrNames (builtins.readDir ./vignettes)
      );
    in
    {
      dda = pkgs.callPackage (
        { rPackages, ... }:
        rPackages.buildRPackage {
          name = "dda";
          src = builtins.fetchGit ./.;
          propagatedBuildInputs = r-import-deps rPackages;
        }
      ) { };

      website = pkgs.callPackage (
        {
          stdenv,
          image_optim,
          rPackages,
          rWrapper,
          ...
        }:

        stdenv.mkDerivation {
          name = "dda-website";
          src = builtins.fetchGit ./.;
          buildInputs = [
            image_optim
            (rWrapper.override { packages = r-dev-deps rPackages; })
          ];
          HOME = ".";

          buildPhase = ''
            Rscript -e "options(pkgdown.internet = FALSE); pkgdown::build_site()"
            image_optim --recursive docs
          '';

          installPhase = ''
            cp -r docs $out
          '';
        }
      ) { };
    }
    // builtins.listToAttrs (
      map (file: rec {
        name = "dda-vignette-" + builtins.elemAt (pkgs.lib.splitString "." file) 0;
        value = pkgs.callPackage buildVignette { inherit file name; };
      }) vignettes
    );

  checks.default = {
    inherit packages;
  };
}
