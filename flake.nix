{
  description = "thesis";
  inputs.nixpkgs.url = "nixpkgs";
  inputs.nixpkgs-unstable.url = "nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, nixpkgs-unstable, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        unstable = nixpkgs-unstable.legacyPackages.${system};

        python-deps = p: with p; [
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
          unstable.rPackages.ICS
          unstable.rPackages.ICSOutlier
          corrplot
          xts
          ellipse
          whitening
          memoise

          ggExtra
          GGally
          plotly
          tidyverse

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
            (quarto.override { extraRPackages = r-deps; extraPythonPackages = python-deps; })
            (rWrapper.override { packages = r-deps; })
            (rstudioWrapper.override { packages = r-deps; })
            texliveFull
          ];
        };

        packages.default = pkgs.callPackage
          ({ stdenv, lib, quarto, texliveFull, rPackages, zip, ... }:
            stdenv.mkDerivation {
              pname = "thesis";
              version = "0.1";
              src = ./.;
              buildInputs = [
                (quarto.override { extraRPackages = r-deps; extraPythonPackages = python-deps; })
                texliveFull
                zip
              ];

              buildPhase = ''
                HOME=. quarto render report/main.qmd
              '';

              installPhase = ''
                mkdir $out
                cd report 
                cp -r *.pdf $out
                zip -r $out/thesis-web-$version.zip *{.html,_files}
              '';
            }
          )
          { };
      }
    );
}

