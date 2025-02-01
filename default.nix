let
  inputs = import ./deps;
  system = "x86_64-linux";
  pkgs = import inputs.nixpkgs {
    inherit system;
    overlays = [
      (_: prev: {
        rPackages = prev.rPackages.override {
          overrides = with prev.rPackages; {
            colorout = buildRPackage {
              name = "colorout";
              src = pkgs.fetchFromGitHub {
                owner = "jalvesaq";
                repo = "colorout";
                rev = "v1.3-2";
                hash = "sha256-HBQRKqyYYAKJj2TFXBgX5Gc6EnkMG3ZaMCxuHVR9Cfc=";
              };
            };

            pkgdown = buildRPackage {
              name = "pkgdown";
              src = pkgs.fetchFromGitHub {
                owner = "r-lib";
                repo = "pkgdown";
                rev = "v2.0.9";
                hash = "sha256-2rmQxCC6GH6ZVqgifh4rXPHqOMKUPZD32gcnGFOeQPw=";
              };
              propagatedBuildInputs = [
                bslib
                callr
                cli
                desc
                digest
                downlit
                fs
                httr
                jsonlite
                magrittr
                memoise
                purrr
                ragg
                rlang
                rmarkdown
                tibble
                whisker
                withr
                xml2
                yaml
              ];
            };

            tidyfun = buildRPackage {
              name = "tidyfun";
              src = pkgs.fetchFromGitHub {
                owner = "tidyfun";
                repo = "tidyfun";
                rev = "d9c4adbd2ff1179cc1f37cb34464e42f5fe2739a";
                hash = "sha256-uSpwjZZ2+GZZpNn5PFwHfRal7o5MTd5rST8jKd6Kpdo=";
              };
              propagatedBuildInputs = [
                tf
                dplyr
                GGally
                ggplot2
                pillar
                purrr
                tibble
                tidyr
                tidyselect
              ];
            };
          };
        };
      })
    ];
  };

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
      boot
      corrplot
      DeBoinR
      fdANOVA
      fda_usc
      fdaoutlier
      knitr
      MVN
      quarto
      rmarkdown
      sf
      tf
      tidyfun
      tidyverse
    ];

  r-dev-deps =
    ps:
    with ps;
    r-suggest-deps ps
    ++ [
      devtools
      rextendr
      svglite
    ];

  r-deps =
    ps:
    with ps;
    r-dev-deps ps
    ++ [
      colorout
      compositions
      languageserver
      mvnfast
      robCompositions
      sinew
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
      cargo
      rustc
      (quarto.override { extraRPackages = r-deps pkgs.rPackages ++ [ packages.x86_64-linux.dda ]; })
      (rWrapper.override { packages = r-deps pkgs.rPackages ++ [ packages.x86_64-linux.dda ]; })
      texliveFull
      npins
    ];
    shellHook = ''
      ${pre-commit-hook.shellHook}
      Rscript -e "rextendr::document()"
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
        {
          rPackages,
          rustPlatform,
          cargo,
          rustc,
          ...
        }:
        rPackages.buildRPackage {
          name = "dda";
          src = builtins.fetchGit ./.;

          cargoDeps = rustPlatform.importCargoLock {
            lockFile = ./src/rust/Cargo.lock;
          };

          cargoRoot = "src/rust";

          nativeBuildInputs = [
            cargo
            rustc
            rustPlatform.cargoSetupHook
          ];

          propagatedBuildInputs = r-import-deps rPackages;
        }
      ) { };

      website = pkgs.callPackage (
        {
          stdenv,
          image_optim,
          pandoc,
          rPackages,
          rWrapper,
          ...
        }:

        stdenv.mkDerivation {
          name = "dda-website";
          src = builtins.fetchGit ./.;
          buildInputs = [
            image_optim
            pandoc
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
