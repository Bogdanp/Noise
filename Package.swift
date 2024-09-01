// swift-tools-version:5.8
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
      swiftSettings: [
        .enableUpcomingFeature("StrictConcurrency")
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
      ],
      swiftSettings: [
        .enableUpcomingFeature("StrictConcurrency")
      ]
    ),
    .target(
      name: "NoiseSerde",
      swiftSettings: [
        .enableUpcomingFeature("StrictConcurrency")
      ]
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
      ],
      swiftSettings: [
        .enableUpcomingFeature("StrictConcurrency")
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
