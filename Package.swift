// swift-tools-version:5.5
import PackageDescription

let package = Package(
  name: "Noise",
  platforms: [
    .macOS(.v12)
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
    .testTarget(
      name: "NoiseTest",
      dependencies: ["Noise"],
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
