<div align="center">
  <picture>
    <img src="logo.png" height="400px"/>
  </picture> 

  <h1> MoonBit Compiler

[MoonBit] | [Documentation] | [Tour] | [Core]
</div>

This is the source code repository for [MoonBit], a programming language that is user-friendly, builds fast, and produces high quality output.

[Moonbit]: https://www.moonbitlang.com
[Tour]: https://tour.moonbitlang.com
[Documentation]: https://docs.moonbitlang.com
[Core]: https://github.com/moonbitlang/core

## Roadmap

Building a programming language is a long journey. It took Rust 9 years and Go 5 years to reach 1.0. Made by a young and driven team, MoonBit is steadily moving forward. We understand that community adoption and expansion are key to a new language, and we’re fully committed to nurturing an engaged and collaborative community around MoonBit. So far, we have open-sourced [the core library](https://github.com/moonbitlang/core) and most tools, including [build tools](https://github.com/moonbitlang/moon), [lex](https://github.com/moonbit-community/moonlex), [markdown](https://github.com/moonbit-community/cmark), and more to come. Having the compiler source available is important for security measures. Open-sourcing the Wasm backend is another major step, and it is on our roadmap to open source more (moonfmt, moondoc) in the future.


## Build from source

### Prerequisites

- OCaml 4.14.2
- [OPAM](https://opam.ocaml.org/)

You must update or revert MoonBit to [this version](https://github.com/moonbitlang/core/commit/4660d8b3da6ed79e47462d66d40feff177060699), as the syntax of the language has changed since.

### Build

Build with following scripts:

```
opam switch create 4.14.2
opam install -y dune
dune build -p moonbit-lang
```

You would also need to build the core library, as instructed in the following section.

### Usage

MoonBit's core library is typically installed in `~/.moon/lib/core/`. In following commands, we use `$core` to denote the path. You can choose your target between `riscv` and `wasm-gc`, which we denote by `$target`. Currently, `riscv` will only produce a `.ssa` file for static single assignment IR, and does not proceed to generate assembly.

We use `$src` to denote the path to your main package. This package must contain, along with your source files, a `moon.pkg.json`; if you're not sure how this works, you can use [moon](https://github.com/moonbitlang/moon) to initialize a MoonBit repository.

We use `$obj` to indicate path where object files should be generated; they typically carry suffixes `.core` and `.mi`.

We use `$dest` to represent target files, which might be `.wat` or `.wasm`, but no other choices are allowed.

To set up the environment, execute these commands (you only need to do it once):

```bash
# Remove currently installed MoonBit version
rm -rf $core

# Install the specific version required by the compiler
git clone https://github.com/moonbitlang/core.git $core
git checkout 4660d8b

# Compile the core library
moon bundle --source-dir $core
```

We strongly recommend that you build the core library yourself via the commands above. The pre-built binaries are not always compatible with this compiler, as MoonBit is still under development.

You should verify that now there is a folder called `wasm-gc` under `$core/target`.

Now you can compile `.mbt` files with these commands:

```bash
# Even if you are targeting RISC-V, you can still use this path.
# That's because it's intermediate representation (IR) in the bundle;
# it is ignorant of target.
bundled=$core/target/wasm-gc/release/bundle

# Here, main.mbt should be a file containing `fn main`.
moonc build-package $src/main.mbt -is-main -std-path $bundled -o $obj

# If you have more than one package, remember to include all of them in -pkg-sources. They should be separated by colon ':'.
moonc link-core $bundled/core.core $obj -o $dest -pkg-config-path $src/moon.pkg.json -pkg-sources $core:$src -target $target
```

Then `$dest` would be available for use.

In case you are still in doubt, refer to the output of `moon run --dry-run`.

## Contributing

The project is evolving extremely fast that it is not yet ready for massive community contributions. 

If you do have interest in contributing, thank you!

Please sign the [CLA](https://www.moonbitlang.com/cla/moonc) first.
For small bug fixes, you are welcome to send the patch to [our email](mailto:jichuruanjian@idea.edu.cn). For large contributions, it is recommended to open a discussion first in our [community forum](https://discuss.moonbitlang.com).

## LICENSE

MoonBit adopts MoonBit Public License which is a relaxed SSPL (Server Side Public License) with two key exceptions:

-  Artifacts produced by the MoonBit compiler may be licensed by the user under any license of their choosing, users have the freedom to choose the license for their own MoonBit source code and generated artifacts.
- Modifications to the compiler are allowed for non-commercial purposes.
   
While we value openness, we chose the relaxed SSPL instead of a fully permissive license for two main reasons:

- MoonBit is still in its beta-preview stage. Introducing forks at this point could risk destabilizing the project. We aim to reach a more mature and stable status before welcoming community contributions.
- We want to safeguard against large cloud vendors leveraging our work for commercial purposes in a way that could undermine our efforts.


In the past two years, our team worked hard to improve MoonBit and its toolchain, staying true to our vision of creating a fast, simple, and efficient language. By open sourcing MoonBit, we would like to reassure our users that our team remains dedicated to MoonBit's pace of growth and innovation. We also want to ensure our users that MoonBit is not going to adopt [open-core](https://en.wikipedia.org/wiki/Open-core_model) model, all MoonBit users will get the best developed compiler and IDE support. MoonBit team will try to generate revenue through cloud hosting services and hardware SDKs in the longer term.

# Credits 

We are grateful for the support of the community. 
Special thanks to Jane Street for their excellent PPX libraries,
for this repo has used some of their [PPX functions](./src/hash.c).

