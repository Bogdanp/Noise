// swift-tools-version:5.5
import PackageDescription

let package = Package(
  name: "Noise",
  platforms: [
    .macOS(.v12)
  ],
  products: [
    .library(
      name: "Noise",
      targets: ["Noise"]
    ),
    .library(
      name: "NoiseBackend",
      targets: ["NoiseBackend"]
    ),
    .library(
      name: "NoiseSerde",
      targets: ["NoiseSerde"]
    ),
  ],
  targets: [
    .target(
      name: "Noise",
      dependencies: [
        "RacketCS",
      ],
      resources: [
        .copy("boot"),
      ],
      linkerSettings: [
        .linkedLibrary("curses", .when(platforms: [.macOS])),
        .linkedLibrary("iconv"),
      ]
    ),
    .target(
      name: "NoiseBackend",
      dependencies: [
        "Noise",
        "NoiseSerde"
      ]
    ),
    .target(
      name: "NoiseSerde"
    ),
    .testTarget(
      name: "NoiseTest",
      dependencies: [
        "Noise",
        "NoiseSerde",
      ],
      resources: [
        .copy("Modules"),
      ]
    ),
    .binaryTarget(
      name: "RacketCS",
      path: "RacketCS.xcframework"
    ),
  ]
)
