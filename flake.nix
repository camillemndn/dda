{
  description = "thesis";
  inputs.nixpkgs.url = "nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        python-deps =
          p: with p; [
            plotly
            scipy
          ];

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
      {
        devShells.default = pkgs.mkShell {
          nativeBuildInputs = [ pkgs.bashInteractive ];
          buildInputs = with pkgs; [
            (python3.withPackages python-deps)
            (quarto.override {
              extraRPackages = r-deps;
              extraPythonPackages = python-deps;
            })
            (rWrapper.override { packages = r-deps; })
            (rstudioWrapper.override { packages = r-deps; })
            texliveFull
          ];
        };

        checks.default = pkgs.callPackage (
          {
            stdenv,
            lib,
            rWrapper,
            ...
          }:
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
      }
    );
}
