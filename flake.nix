{
  description = "MoonBit compiler with latest moon and current moonc";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        packages.default = pkgs.stdenv.mkDerivation rec {
          pname = "moonbit-hybrid";
          version = "latest-moon-plctlab-moonc";

          # Use latest moon binary
          moonSrc = pkgs.fetchzip {
            url = "https://cli.moonbitlang.com/binaries/latest/moonbit-linux-x86_64.tar.gz";
            sha256 = "sha256-8AwukSHQ9NlFncAJAdM3ZJOYHWMqzw39L6bcYWw+0rM=";
            stripRoot = false;
          };

          # Use current GitHub repo for moonc
          src = ./.;

          nativeBuildInputs = with pkgs; [
            autoPatchelfHook
            patchelf
            makeWrapper
            dune_3
            ocaml
            pkg-config
            libffi
            gmp
          ];

          buildInputs = with pkgs; [
            stdenv.cc.cc.lib
          ];

          buildPhase = ''
            runHook preBuild

            # Build moonc from current repo
            dune build --root .

            # Prepare moon from latest release
            cp -r ${moonSrc}/* ./
            chmod -R u+w ./bin || echo "Failed to set write permissions on bin"
            chmod +x ./bin/moon || echo "moon binary not found"
            patchelf --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" ./bin/moon || echo "Failed to patch moon"
            [ -f ./bin/internal/tcc ] && patchelf --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" ./bin/internal/tcc || echo "tcc not found or failed to patch"

            # Replace moonc with our built version
            cp _build/install/default/bin/moonc ./bin/moonc
            chmod +x ./bin/moonc

            runHook postBuild
          '';

          installPhase = ''
            runHook preInstall

            mkdir -p $out/bin
            mkdir -p $out/lib

            # Install binaries
            cp ./bin/moon $out/bin/
            cp ./bin/moonc $out/bin/
            cp -r ./bin/internal $out/bin/

            # Install lib files if they exist
            if [ -d "./lib" ]; then
              cp -r ./lib/* $out/lib/
            fi

            runHook postInstall
          '';

          postFixup = ''
            wrapProgram $out/bin/moon --set MOON_HOME $out
          '';

          meta = with pkgs.lib; {
            description = "MoonBit toolchain with latest moon and current moonc";
            homepage = "https://www.moonbitlang.com";
            license = licenses.asl20;
            mainProgram = "moon";
            platforms = platforms.linux;
          };
        };

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            ocaml
            dune_3
            ocamlPackages.findlib
            ocamlPackages.menhir
            ocamlPackages.sedlex
            ocamlPackages.cmdliner
            ocamlPackages.yojson
            ocamlPackages.ppx_deriving_yojson
            ocamlPackages.zarith
            pkg-config
            libffi
            gmp
          ];

          shellHook = ''
            echo "MoonBit development shell"
            echo "OCaml version: $(ocaml -version)"
            echo "Dune version: $(dune --version)"
          '';
        };
      });
}
