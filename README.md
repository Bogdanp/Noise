# Noise

Noise is a Swift wrapper around the [Racket] CS runtime intended to
simplify embedding.  See `Tests/NoiseTest/RacketTest.swift` for an
example.

## Usage

Clone this repository locally and run `make`, then add it to your
project as a local dependency.  [Git LFS][LFS] is used to store the
binary files in `Lib/` and in `Sources/Noise/boot`, so you will need
have [LFS] installed in order to pull those files.

See [NoiseBackendExample] for an example application built with Noise.

## NoiseSerde

The `NoiseSerde` package and its associated `noise/serde` module (from
`Racket/noise-serde-lib`) provide a way to define data structures that
can automatically be shared (via serialization & deserialization)
between Racket and Swift.

## NoiseBackend

The `NoiseBackend` package and its associated `noise/backend` module
build upon `NoiseSerde` to provide a client-server implementation
where the Racket server is continuously run in a background thread and
the Swift client communicates with it via pipes.

## Updating Racket

To update the Racket libraries distributed with this package, build
Racket for your target platforms then run

    ./Bin/copy-libs.sh ARCH /path/to/racket

For example:

    ./Bin/copy-libs.sh arm64-ios /path/to/racket
    ./Bin/copy-libs.sh arm64-macos /path/to/racket
    ./Bin/copy-libs.sh x86_64-macos /path/to/racket

## License

    Noise and its associated packages are licensed under the 3-Clause BSD license.

See [this page][racket-license] for information on Racket's license.

[NoiseBackendExample]: https://github.com/Bogdanp/NoiseBackendExample
[Racket]: https://racket-lang.org
[LFS]: https://git-lfs.github.com
[racket-license]: https://github.com/racket/racket/blob/82ca0f76f2e18f242db742991596eb509ce49cc1/LICENSE.txt
