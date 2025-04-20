# Noise

Noise is a Swift wrapper around the [Racket] CS runtime intended to
simplify embedding. See `Tests/NoiseTest/RacketTest.swift` for an
example.

## Quickstart

1. Clone this repository.
2. Run `raco pkg install Racket/noise-serde{-lib,-doc}/`.
4. Run `make`.

Note: [Git LFS][LFS] is used to store the binary files in `Lib/` and in
`Sources/Noise/boot`, so you will need have [LFS] installed in order to
pull those files.

## Usage

The shared libraries and the boot files must match the version of Racket
you use to compile your Racket code. Most likely, the versions of the
files checked into the master branch _won't_ match your version of
Racket, if you're using a release build of Racket. To import your own
versions of these files, build Racket from source and run:

    ./Bin/copy-libs.sh arm64-macos /path/to/src/racket

Where the first argument depends on your target OS and architecture:

| OS            | Architecture  | Argument                |
|---------------|---------------|-------------------------|
| macOS         | x86_64        | `x86_64-macos`          |
| macOS         | arm64/aarch64 | `arm64-macos`           |
| iOS           | arm64/aarch64 | `arm64-ios`             |
| iOS Simulator | arm64/aarch64 | `arm64-iphonesimulator` |

For iOS, you have to configure Racket with the following flags in order
to generate a portable bytecode build:

    configure \
      --host=aarch64-apple-darwin \
      --enable-ios=iPhoneOS \
      --enable-pb \
      --enable-racket=auto \
      --enable-libffi

For the iPhone Simulator, change the value of the `--enable-ios` flag to
`iPhoneSimulator`. After building for either platform, you need to merge
the associated `libffi` archive into the generated `libracketcs.a`. For
example:

    libtool -s \
      -o racket/lib/libracketcs1.a \
      racket/lib/libracketcs.a \
      /path/to/libffi.a \
      && mv racket/libracketcs{1,}.a

Pre-compiled builds for recent versions of Racket are available on the
following branches:

* `racket-8.16`
* `racket-8.15` (first branch to include iOS build)
* `racket-8.14`
* `racket-8.13`
* `racket-8.12`
* `racket-8.11.1`
* `racket-8.11`
* `racket-8.10`

See [NoiseBackendExample] for an example application built with Noise.

## NoiseSerde

The `NoiseSerde` package and its associated `noise/serde` module (from
`Racket/noise-serde-lib`) provide a way to define data structures that
can automatically be shared (via serialization & deserialization)
between Racket and Swift.

To use `NoiseSerde` from Racket, you will have to install
`noise-serde-lib`:

    raco pkg install Racket/noise-serde-lib/

You may also want to install its docs:

    raco pkg install Racket/noise-serde-doc/

Run `raco docs noise` to read the docs.

## NoiseBackend

The `NoiseBackend` package and its associated `noise/backend` module
build upon `NoiseSerde` to provide a client-server implementation
where the Racket server is continuously run in a background thread and
the Swift client communicates with it via pipes.

## License

    Noise and its associated packages are licensed under the 3-Clause BSD license.

See [this page][racket-license] for information on Racket's license.

[NoiseBackendExample]: https://github.com/Bogdanp/NoiseBackendExample
[Racket]: https://racket-lang.org
[LFS]: https://git-lfs.github.com
[racket-license]: https://github.com/racket/racket/blob/82ca0f76f2e18f242db742991596eb509ce49cc1/LICENSE.txt
