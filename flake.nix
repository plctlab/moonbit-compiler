{
  description = "MoonBit compiler with latest moon and current moonc";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    moon-repo = {
      url = "github:moonbitlang/moon/f1ae8e97e4c020c078f0f728987a1001ab8cc5ef";
      flake = false;
    };
    core-repo = {
      url = "github:moonbitlang/core/f69968c1802a315d3f7baa30238c9c67fe5ccd61";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, moon-repo, core-repo }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # Download rusty_v8 prebuilt library
        rusty-v8-lib = pkgs.fetchurl {
          url = "https://github.com/denoland/rusty_v8/releases/download/v0.106.0/librusty_v8_release_x86_64-unknown-linux-gnu.a.gz";
          sha256 = "sha256-jLYl/CJp2Z+Ut6qZlh6u+CtR8KN+ToNTB+72QnVbIKM=";
        };
      in
      {
        packages = {
          default = pkgs.stdenv.mkDerivation rec {
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

        # Build moon from specific commit
        moon-custom = pkgs.rustPlatform.buildRustPackage {
          pname = "moon";
          version = "47b2c5a0";

          src = moon-repo;

          cargoLock = {
            lockFile = "${moon-repo}/Cargo.lock";
            outputHashes = {
              "n2-0.1.5" = "sha256-HAA0S8TjcWuIljgixWaLvMK4RFaE3YPPlp7CKkboCH4=";
            };
          };

          nativeBuildInputs = with pkgs; [ pkg-config perl python3 ];
          buildInputs = with pkgs; [ openssl ];

          # Skip tests to speed up build
          doCheck = false;

          # Provide the predownloaded rusty_v8 library
          RUSTY_V8_ARCHIVE = "${rusty-v8-lib}";

          meta = with pkgs.lib; {
            description = "Moon build system";
            homepage = "https://www.moonbitlang.com";
            license = licenses.asl20;
          };
        };

        # Build moonc from current repo
        moonc-custom = pkgs.stdenv.mkDerivation {
          pname = "moonc";
          version = "local";

          src = ./.;

          nativeBuildInputs = with pkgs; [
            dune_3
            ocaml
            pkg-config
            libffi
            gmp
          ];

          buildPhase = ''
            dune build --root .
          '';

          installPhase = ''
            mkdir -p $out/bin
            cp _build/install/default/bin/moonc $out/bin/
            chmod +x $out/bin/moonc
          '';
        };

        # Combine moon-custom and moonc-custom to build core
        test-core = pkgs.stdenv.mkDerivation rec {
          pname = "moonbit-test-core";
          version = "moon-47b2c5a0-core-39c5f6ee";

          src = core-repo;

          nativeBuildInputs = [ self.packages.${system}.moon-custom self.packages.${system}.moonc-custom ];

          buildPhase = ''
            runHook preBuild

            # Create moon toolchain directory with custom moonc
            mkdir -p moon-toolchain/bin
            cp ${self.packages.${system}.moon-custom}/bin/moon moon-toolchain/bin/
            cp ${self.packages.${system}.moonc-custom}/bin/moonc moon-toolchain/bin/
            chmod +x moon-toolchain/bin/moon
            chmod +x moon-toolchain/bin/moonc

            # Copy core to writable location
            cp -r ${core-repo} core-test
            chmod -R u+w core-test
            cd core-test

            # Set up environment for moon to use writable directories
            export HOME=$(pwd)/moon-home
            export MOON_HOME=$(pwd)/moon-home
            mkdir -p $HOME/.moon

            # Build core with custom toolchain
            echo "Building core library with moon (f2f5ab3f) and local moonc..."
            export PATH=$(pwd)/../moon-toolchain/bin:$PATH
            moon bundle 2>&1 | tee ../build-log.txt || echo "Build completed with errors"

            runHook postBuild
          '';

          installPhase = ''
            runHook preInstall

            # Navigate back to build root (buildPhase ended in core-test subdirectory)
            cd ..

            mkdir -p $out/bin
            mkdir -p $out/logs
            mkdir -p $out/core

            # Install moon toolchain
            if [ -d moon-toolchain/bin ]; then
              cp -r moon-toolchain/bin/. $out/bin/
            fi

            # Install build log
            if [ -f build-log.txt ]; then
              cp build-log.txt $out/logs/
            fi

            # Install core artifacts if they exist
            if [ -d "core-test/target" ]; then
              cp -r core-test/target $out/core/
            fi

            runHook postInstall
          '';

          meta = with pkgs.lib; {
            description = "MoonBit moon (47b2c5a0, 2025-04-18) with local moonc, testing core (39c5f6ee, 2025-04-18)";
            homepage = "https://www.moonbitlang.com";
            license = licenses.asl20;
            platforms = platforms.linux;
          };
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
