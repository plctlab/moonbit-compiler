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

        # Build core library from pinned revision
        core-bundled = pkgs.stdenv.mkDerivation {
          pname = "moonbit-core";
          version = "f69968c1";

          src = core-repo;

          nativeBuildInputs = [
            self.packages.${system}.moon-custom
            self.packages.${system}.moonc-custom
          ];

          buildPhase = ''
            runHook preBuild

            # Create moon toolchain directory with custom moonc
            mkdir -p moon-toolchain/bin
            cp ${self.packages.${system}.moon-custom}/bin/moon moon-toolchain/bin/
            cp ${self.packages.${system}.moonc-custom}/bin/moonc moon-toolchain/bin/
            chmod +x moon-toolchain/bin/moon
            chmod +x moon-toolchain/bin/moonc

            # Set up environment for moon
            export HOME=$(pwd)/moon-home
            export MOON_HOME=$(pwd)/moon-home
            mkdir -p $HOME/.moon

            # Build and bundle core with custom toolchain
            export PATH=$(pwd)/moon-toolchain/bin:$PATH
            moon bundle

            runHook postBuild
          '';

          installPhase = ''
            runHook preInstall

            mkdir -p $out/target
            cp -r target/* $out/target/

            runHook postInstall
          '';

          meta = with pkgs.lib; {
            description = "MoonBit core library (f69968c1)";
            homepage = "https://www.moonbitlang.com";
            license = licenses.asl20;
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
            clang
            python3
            self.packages.${system}.moon-custom
            self.packages.${system}.moonc-custom
            self.packages.${system}.core-bundled
          ];

          shellHook = ''
            echo "MoonBit development shell"
            echo "OCaml version: $(ocaml -version)"
            echo "Dune version: $(dune --version)"
            echo "Moon version: $(moon --version 2>/dev/null || echo 'not available')"
            echo "Moonc version: $(moonc -v 2>/dev/null || echo 'not available')"
            echo ""

            # Link core library to ~/.moon/lib/core
            MOON_LIB_DIR="$HOME/.moon/lib"
            CORE_LINK="$MOON_LIB_DIR/core"

            # Backup existing core if it's not a symlink
            if [ -e "$CORE_LINK" ] && [ ! -L "$CORE_LINK" ]; then
              echo "Backing up existing core library to ~/.moon/lib/core.backup"
              mv "$CORE_LINK" "$CORE_LINK.backup"
            fi

            # Remove old symlink if it exists
            [ -L "$CORE_LINK" ] && rm "$CORE_LINK"

            # Create ~/.moon/lib directory if it doesn't exist
            mkdir -p "$MOON_LIB_DIR"

            # Create symlink to pinned core
            ln -sf "${self.packages.${system}.core-bundled}" "$CORE_LINK"

            echo "Core library linked to ~/.moon/lib/core"
            echo "Core version: f69968c1802a315d3f7baa30238c9c67fe5ccd61"
            echo ""
            echo "To restore original core library, exit this shell and remove the symlink:"
            echo "  rm ~/.moon/lib/core"
            echo "  [ -e ~/.moon/lib/core.backup ] && mv ~/.moon/lib/core.backup ~/.moon/lib/core"
          '';
        };
      });
}
