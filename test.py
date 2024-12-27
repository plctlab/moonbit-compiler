#!/bin/python3
import argparse
import os

class DirContext:
    def __init__(self, path):
        self.new_path = path
        self.original = None

    def __enter__(self):
        self.original = os.getcwd()
        os.chdir(self.new_path)

    def __exit__(self, exc_type, exc_value, traceback):
        os.chdir(self.original)

parser = argparse.ArgumentParser(prog = "test", description = "Test the MoonBit compiler")

parser.add_argument("-d", "--debug", action="store_true", help="Enables stack traces on debug.")
parser.add_argument("-w", "--wasm", action="store_true", help="Builds to WASM, rather than RISC-V.")
parser.add_argument("-i", "--build-index", action="store_true", help="Builds the OCaml index only, and then exits.")

args = parser.parse_args()

if args.wasm:
    target = "wasm-gc"
    dest = "target.wat"
else:
    target = "riscv"
    dest = "target.s"

core = "~/.moon/lib/core"
bundled = f"{core}/target/wasm-gc/release/bundle"
src = "basic"

if args.debug:
    debug = "OCAMLRUNPARAM=b"
else:
    debug = ""

def try_remove(path):
    if os.path.exists(path):
        os.remove(path)

    
# Build
if args.build_index:    
    os.system("dune build @ocaml-index")
    exit(0)

print("Building MoonBit compiler...")
os.system("dune build -p moonbit-lang")

print("Building SSA interpreter...")
os.system("clang++ -std=c++20 -fuse-ld=lld test/interpreter.cpp -O3 -o test/build/interpreter")


with DirContext("test"):
    print(f"Execute task: {src}")
    # Remove all previously compiled files.
    try_remove(f"{src}.core")
    try_remove(f"{src}.mi")
    try_remove(f"{dest}")

    # Note build-package is ignorant of target. It builds to a common IR.
    os.system(f"moonc build-package src/{src}/{src}.mbt -is-main -std-path {bundled} -o build/{src}.core")

    # Linkage emits target code.
    os.system(f"{debug} moonc link-core {bundled}/core.core build/{src}.core -o build/{dest} -pkg-config-path {src}/moon.pkg.json -pkg-sources {core}:{src} -target {target}")

    # Test.
    os.system(f"build/interpreter build/{dest}.ssa > build/output.txt")
    os.system(f"diff build/output.txt src/{src}/{src}.ans")