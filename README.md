## zigSelf

[![Discord](https://img.shields.io/discord/983578577029181440?logo=discord&logoColor=white&logoWidth=20&labelColor=7289DA&label=Discord&color=119334&style=plastic)](https://discord.gg/HJ62kw6yvn) [![justforfunnoreally.dev badge](https://img.shields.io/badge/justforfunnoreally-dev-9ff)](https://justforfunnoreally.dev)

An implementation of the Self programming language.

## What is Self?

[Self](https://selflanguage.org/) is a programming language designed in 1986 at
Xerox PARC. It is an object-oriented programming language which uses
_prototype-based inheritance_ as its basis and message-passing as its execution
method. It is dynamically (but strongly) typed.

Self uses an image-based approach for runtime. All the objects in the system
are stored in a _Self world_ and turned into a _snapshot_ for saving. At
runtime, objects can be interacted with, created and destroyed without losing
state on other objects.

## Build-time requirements

You need the Zig compiler, preferably one built with the known-good version
commit. You can find the source code, instructions for building, and more on the
[Zig repository](https://github.com/ziglang/zig).

Latest Zig commit known to work is [`0fb53bd24`](https://github.com/ziglang/zig/commit/0fb53bd24).
Earlier and later versions may work but there are no guarantees.

## Building zigSelf

1. Clone the repository with submodules: `git clone --recurse-submodules https://github.com/sin-ack/zigself`
2. Run the code: `zig build run -- examples/fibonacci.self`

That's it! If you want to build a release version, you can build one with
`zig build -Drelease-fast=true` and the binary will be in `zig-out/bin/`.

There is an (experimental) REPL that you can run with `zig build run -- repl.self`.

## Building for Wasm

ZigSelf is able to run as a WebAssembly module via WASI. To build it, add the
`wasm32-wasi` target when building: `zig build -Dtarget=wasm32-wasi`

You can then run the resulting executable at `zig-out/bin/self.wasm` with
[Wasmtime](https://github.com/bytecodealliance/wasmtime). Currently ZigSelf does
not support loading Self code via stdin, so you will need to give it filesystem
access like so: `wasmtime --mapdir=.::. zig-out/bin/self.wasm <script name>`

## Contributing

All contributions are welcome! Please follow [the contribution
guidelines](CONTRIBUTING.md) for details.

## License

zigSelf is licensed under the GNU General Public License version 3, so that the
VM implementation remains open for everyone including end-users to study, modify
and share. See [LICENSE](LICENSE) for more details.
