// swift-tools-version:5.5
import PackageDescription

let package = Package(
  name: "Noise",
  platforms: [
    .iOS(.v14),
    .macOS(.v12),
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
        .target(name: "RacketCS-ios", condition: .when(platforms: [.iOS])),
        .target(name: "RacketCS-macos", condition: .when(platforms: [.macOS])),
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
