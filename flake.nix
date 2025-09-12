{
  description = "MoonBit Compiler Development Environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        
        # OCaml 4.14.2 with required packages
        ocamlPackages = pkgs.ocaml-ng.ocamlPackages_4_14;
        
        buildInputs = with pkgs; [
          # OCaml toolchain (no OPAM needed with Nix)
          ocamlPackages.ocaml
          ocamlPackages.dune_3
          ocamlPackages.findlib
          ocamlPackages.ocaml-lsp
          
          # Development tools
          git
          curl
          
          # For Wasm-based compiler
          nodejs
          
          # For building moon tool (Rust-based)
          cargo
          rustc
          
          # Additional utilities
          which
          gnumake
        ];

        shellHook = ''
          echo "🌙 MoonBit Compiler Development Environment"
          echo "================================================"
          echo "OCaml version: $(ocaml -version)"
          echo "Dune version: $(dune --version)"
          echo "Node.js version: $(node --version)"
          echo "Cargo version: $(cargo --version)"
          echo ""
          echo "📋 Quick Start Commands:"
          echo "  - Build compiler: dune build -p moonbit-lang"
          echo "  - Set up core lib: See README.md section 'Build from source'"
          echo ""
          echo "💡 Tips:"
          echo "  - All OCaml dependencies managed by Nix (no OPAM needed)"
          echo "  - Core library needs to be at specific commit 4660d8b"
          echo "  - Use 'nix develop' to enter this environment"
          echo ""
          
          # Ensure we can find OCaml libraries
          export OCAML_TOPLEVEL_PATH="${ocamlPackages.ocaml}/lib/ocaml"
        '';

      in
      {
        devShells.default = pkgs.mkShell {
          inherit buildInputs shellHook;
          
          name = "moonbit-dev";
          
          # Environment variables
          OCAML_VERSION = "4.14.2";
          DUNE_PROFILE = "dev";
          
          # For building with specific OCaml version
          PATH_PREFIX = "${ocamlPackages.ocaml}/bin:${ocamlPackages.dune_3}/bin";
        };

        # Optional: package the compiler itself
        packages.moonbit-compiler = pkgs.stdenv.mkDerivation {
          pname = "moonbit-compiler";
          version = "dev";
          
          src = ./.;
          
          nativeBuildInputs = with ocamlPackages; [
            ocaml
            dune_3
            findlib
          ];
          
          buildPhase = ''
            dune build -p moonbit-lang
          '';
          
          installPhase = ''
            mkdir -p $out/bin
            cp _build/default/src/moon0_main.exe $out/bin/moonc
          '';
          
          meta = with pkgs.lib; {
            description = "MoonBit Programming Language Compiler";
            homepage = "https://www.moonbitlang.com";
            license = licenses.unfree; # MoonBit Public License
            maintainers = [ "MoonBit Team" ];
            platforms = platforms.unix;
          };
        };

        # Default package
        packages.default = self.packages.${system}.moonbit-compiler;
      });
}