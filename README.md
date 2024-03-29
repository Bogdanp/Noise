# Noise

Noise is a Swift wrapper around the [Racket] CS runtime intended to
simplify embedding. See `Tests/NoiseTest/RacketTest.swift` for an
example.

## Usage

Clone this repository locally and run `make`, then add it to your
project as a local dependency. [Git LFS][LFS] is used to store the
binary files in `Lib/` and in `Sources/Noise/boot`, so you will need
have [LFS] installed in order to pull those files.

The shared libraries and the boot files must match the version of Racket
you use to compile your Racket code. Most likely, the versions of the
files checked into the master branch won't match your version of Racket.
To import your own versions of these files, build Racket from source and
run:

    ./Bin/copy-libs.sh arm64-macos /path/to/src/racket

Where the first argument is either `arm64-macos` if you're on an Apple
Silicon Mac or `x86_64-macos` if you're on an Intel Mac.

Pre-compiled builds for recent versions of Racket are available on the
following branches:

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

To use `NoiseSerde` from racket, you will have to install
`noise-serde-lib`:

    raco pkg install Racket/noise-serde-lib/

You may also want to install its docs:

    raco pkg install Racket/noise-serde-doc/

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
