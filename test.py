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

parser = argparse.ArgumentParser(prog = "test", description = "Tests the MoonBit compiler.")

parser.add_argument("-d", "--debug", action="store_true", help="enable stack traces on debug")
parser.add_argument("-w", "--wasm", action="store_true", help="build to WASM rather than RISC-V")
parser.add_argument("-i", "--build-index", action="store_true", help="build OCaml index and exit")
parser.add_argument("-b", "--build", action="store_true", help="rebuild compiler and interpreter")
parser.add_argument("-v", "--verbose", action="store_true", help="on rebuild, makes interpreter output detailed values")
parser.add_argument("-c", "--compile-only", action="store_true", help="compile without executing tests")
parser.add_argument("-t", "--test", type=str, help="execute this test case only")

args = parser.parse_args()

if args.wasm:
    target = "wasm-gc"
    dest = "target.wat"
else:
    target = "riscv"
    dest = "target.s"

core = "~/.moon/lib/core"
bundled = f"{core}/target/wasm-gc/release/bundle"

debug = "OCAMLRUNPARAM=b" if args.debug else ""
verbose = "-DVERBOSE" if args.verbose else ""

def try_remove(path):
    if os.path.exists(path):
        os.remove(path)

    
# Build
if args.build_index:    
    os.system("dune build @ocaml-index")
    print("Done")
    exit(0)

print("Building MoonBit compiler...")
os.system("dune build -p moonbit-lang")

# Don't build the interpreter every time. Explicitly rebuild if needed.
if args.build and not args.wasm:
    print("Building SSA interpreter...")
    os.makedirs("test/build", exist_ok=True)
    os.system(f"clang++ -std=c++20 {verbose} test/interpreter.cpp -Wall -g -o test/build/interpreter")
    print("Done.")
    exit(0)
    
success = True

with DirContext("test"):
    cases = os.listdir("src") if args.test is None else [args.test]
     
    for src in cases:
        print(f"Execute task: {src}")
        
        # Remove files from last time
        try_remove(f"build/{target}.s.ir")
        try_remove(f"build/{target}.s.ssa")
        try_remove(f"build/{target}.s-no-opt.ssa")
        try_remove(f"build/{target}.s.vasm")
        
        # Note build-package is ignorant of target. It builds to a common IR.
        os.system(f"moonc build-package src/{src}/{src}.mbt -is-main -std-path {bundled} -o build/{src}.core")

        # Linkage emits target code.
        ret = os.system(f"{debug} moonc link-core {bundled}/core.core build/{src}.core -o build/{dest} -pkg-config-path {src}/moon.pkg.json -pkg-sources {core}:{src} -target {target}")
                
        # Remove intermediate files that we don't need.
        try_remove(f"build/{src}.core")
        try_remove(f"build/{src}.mi")
        
        if ret != 0:
            print("Compiler generated an error. Failed.")
            success = False
            continue;

        if args.wasm:
            print("WASM target does not support testing. Exit.")
            break;

        # Test.
        if not args.compile_only:
            os.system(f"build/interpreter build/{dest}.ssa > build/output.txt 2> build/debug.txt")
            diff = os.system(f"diff build/output.txt src/{src}/{src}.ans")
            
            if diff == 0:
                print("Passed.")
            else:
                success = False

exit(not success)