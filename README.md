# Noise

Noise is a Swift wrapper around the [Racket] CS runtime to simplify
embedding.  See `Tests/NoiseTest/RacketTest.swift` for an example.

## Usage

Clone this repository locally and run `make`, then add it to your
project as a local dependency.

## Updating Racket

To update the Racket libraries distributed with this package, build
Racket for your target platforms then run

    ./Bin/copy-libs.sh ARCH /path/to/racket

For example:

    ./Bin/copy-libs.sh arm64-ios /path/to/racket
    ./Bin/copy-libs.sh arm64-macos /path/to/racket
    ./Bin/copy-libs.sh x86_64-macos /path/to/racket

## License

    Noise is licensed under the 3-Clause BSD license.

See [this page][racket-license] for information on Racket's license.

[Racket]: https://racket-lang.org
[racket-license]: https://github.com/racket/racket/blob/82ca0f76f2e18f242db742991596eb509ce49cc1/LICENSE.txt
