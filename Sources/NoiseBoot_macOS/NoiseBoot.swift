import Foundation

#if arch(x86_64)
let ARCH = "x86_64"
#elseif arch(arm64)
let ARCH = "arm64"
#else
#error("unsupported platform")
#endif

public struct NoiseBoot {
  public static let petiteURL = Bundle.module.url(forResource: "boot/\(ARCH)-macos/petite", withExtension: "boot")!
  public static let schemeURL = Bundle.module.url(forResource: "boot/\(ARCH)-macos/scheme", withExtension: "boot")!
  public static let racketURL = Bundle.module.url(forResource: "boot/\(ARCH)-macos/racket", withExtension: "boot")!
}
