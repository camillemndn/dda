let
  inputs = import ./lon.nix;
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
                rev = "v1.3-3";
                hash = "sha256-1aWDrvW1+X5bxJEZlm3RLy8Urx6UlYX7BqJwNF2bNYA=";
              };
            };

            pkgdown_offline = buildRPackage {
              name = "pkgdown_offline";
              src = pkgs.fetchFromGitHub {
                owner = "nanxstats";
                repo = "pkgdown.offline";
                rev = "v0.1.1";
                hash = "sha256-zu16X/WbCTdrIv5GNOUQbWuqDWCrSHAII7uf8I+2Wdk=";
              };
              propagatedBuildInputs = [
                pkgdown
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
      dplyr
      fda
      GGally
      ggplot2
      tidyr
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
      kableExtra
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
      pkgdown_offline
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
    hooks = {
      commitizen.enable = true;

      # ----- Rust -----
      # rustfmt rewrites *.rs in place. If anything changed, pre-commit
      # fails the commit and you re-stage; that's the standard flow.
      rustfmt.enable = true;
      # Clippy: real errors block, but `clippy::*` warnings stay advisory.
      clippy = {
        enable = true;
        settings.denyWarnings = false;
      };

      # ----- Nix -----
      nixfmt.enable = true;

      # ----- Generic hygiene -----
      trim-trailing-whitespace.enable = true;

      # ----- Custom: keep roxygen-generated NAMESPACE + man/ in sync -----
      roxygen-sync = {
        enable = true;
        name = "devtools::document() sync";
        description = "Regenerate NAMESPACE and man/*.Rd when R sources change";
        entry = toString (
          pkgs.writeShellScript "roxygen-sync" ''
            set -eu
            ${(pkgs.rWrapper.override { packages = r-dev-deps pkgs.rPackages; })}/bin/Rscript \
              -e 'devtools::document(quiet = TRUE)'
            git diff --quiet NAMESPACE man/ || {
              echo "roxygen regenerated NAMESPACE / man/; re-stage and retry." >&2
              git add NAMESPACE man/
              exit 1
            }
          ''
        );
        files = "(^R/.*\\.[Rr]$|^DESCRIPTION$)";
        pass_filenames = false;
      };

      # ----- Custom: regenerate savvy bindings when Rust FFI changes -----
      savvy-sync = {
        enable = true;
        name = "savvy-cli update";
        description = "Regenerate src/init.c, src/rust/api.h, R/000-wrappers.R when ffi.rs changes";
        entry = toString (
          pkgs.writeShellScript "savvy-sync" ''
            set -eu
            if ! command -v savvy-cli >/dev/null 2>&1; then
              echo "savvy-cli not on PATH; install via 'cargo install savvy-cli'" >&2
              exit 1
            fi
            savvy-cli update .
            git diff --quiet src/init.c src/rust/api.h R/000-wrappers.R || {
              echo "savvy-cli regenerated bindings; re-stage and retry." >&2
              git add src/init.c src/rust/api.h R/000-wrappers.R
              exit 1
            }
          ''
        );
        files = "^src/rust/src/ffi\\.rs$";
        pass_filenames = false;
      };
    };
  };
in
rec {
  devShells.default = pkgs.mkShell {
    buildInputs = with pkgs; [
      (quarto.override { extraRPackages = r-deps pkgs.rPackages; })
      (rWrapper.override { packages = r-deps pkgs.rPackages; })
      cargo
      clippy
      lon
      rustc
      rustfmt
      texliveFull
    ];
    shellHook = ''
      ${pre-commit-hook.shellHook}
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
          cargo,
          rustc,
          ...
        }:
        stdenv.mkDerivation {
          inherit name;
          src = builtins.fetchGit ./.;

          buildInputs = [
            cargo
            rustc
            (rWrapper.override { packages = r-dev-deps rPackages; })
          ];

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
          cargo,
          rustc,
          ...
        }:
        rPackages.buildRPackage {
          name = "dda";
          src = builtins.fetchGit ./.;
          nativeBuildInputs = [
            cargo
            rustc
          ];
          # Rust deps are pre-vendored under src/rust/vendor/ (committed to
          # the repo). src/Makevars.in detects that directory and runs
          # cargo with --offline + replace-with = "vendored-sources",
          # which works both inside the Nix sandbox and on the CRAN build
          # farm.
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
          cargo,
          rustc,
          ...
        }:

        stdenv.mkDerivation {
          name = "dda-website";
          src = builtins.fetchGit ./.;
          buildInputs = [
            cargo
            rustc
            image_optim
            pandoc
            (rWrapper.override { packages = r-dev-deps rPackages; })
          ];
          HOME = ".";

          buildPhase = ''
            Rscript -e "pkgdown.offline::build_site()"
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

    # `R CMD check` against the source tarball. Fast-ish; runs tests +
    # examples + Rd-format checks.
    R-CMD-check = pkgs.callPackage (
      {
        stdenv,
        rWrapper,
        rPackages,
        cargo,
        rustc,
        ...
      }:
      stdenv.mkDerivation {
        name = "dda-R-CMD-check";
        src = builtins.fetchGit ./.;
        nativeBuildInputs = [
          cargo
          rustc
          (rWrapper.override { packages = r-dev-deps rPackages; })
        ];
        HOME = ".";
        buildPhase = ''
          export _R_CHECK_FORCE_SUGGESTS_=false
          R CMD build .
          R CMD check --no-manual dda_*.tar.gz
        '';
        installPhase = ''
          mkdir -p $out
          cp -r dda.Rcheck $out/ || true
          cp dda_*.tar.gz $out/ || true
        '';
      }
    ) { };

    # `R CMD check --as-cran` — the stricter check CRAN's incoming queue
    # runs. Flags policy violations (license, file structure, Rust
    # vendoring, …) in addition to the regular check set.
    R-CMD-check-cran = pkgs.callPackage (
      {
        stdenv,
        rWrapper,
        rPackages,
        cargo,
        rustc,
        ...
      }:
      stdenv.mkDerivation {
        name = "dda-R-CMD-check-cran";
        src = builtins.fetchGit ./.;
        nativeBuildInputs = [
          cargo
          rustc
          (rWrapper.override { packages = r-dev-deps rPackages; })
        ];
        HOME = ".";
        buildPhase = ''
          export _R_CHECK_FORCE_SUGGESTS_=false
          R CMD build .
          R CMD check --as-cran --no-manual dda_*.tar.gz
        '';
        installPhase = ''
          mkdir -p $out
          cp -r dda.Rcheck $out/ || true
          cp dda_*.tar.gz $out/ || true
        '';
      }
    ) { };
  };
}
