// swift-tools-version:6.0
import PackageDescription

let package = Package(
  name: "Noise",
  platforms: [
    .iOS(.v16),
    .macOS(.v13),
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
      name: "NoiseBoot_iOS",
      resources: [.copy("boot")]
    ),
    .target(
      name: "NoiseBoot_macOS",
      resources: [.copy("boot")]
    ),
    .target(
      name: "Noise",
      dependencies: [
        .target(name: "NoiseBoot_iOS", condition: .when(platforms: [.iOS])),
        .target(name: "NoiseBoot_macOS", condition: .when(platforms: [.macOS])),
        .target(name: "RacketCS-ios", condition: .when(platforms: [.iOS])),
        .target(name: "RacketCS-macos", condition: .when(platforms: [.macOS])),
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
        "NoiseBackend",
        "NoiseSerde",
      ],
      resources: [
        .copy("Modules"),
      ]
    ),
    .binaryTarget(
      name: "RacketCS-ios",
      path: "RacketCS-ios.xcframework"
    ),
    .binaryTarget(
      name: "RacketCS-macos",
      path: "RacketCS-macos.xcframework"
    ),
  ]
)
