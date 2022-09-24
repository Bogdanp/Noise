import Dispatch
import NoiseBackend
import XCTest

class FutureTests: XCTestCase {
  func testFail() {
    let f = Future<String, Never>()
    DispatchQueue.global(qos: .background).async {
      f.fail(with: "failed")
    }
    XCTAssertThrowsError(try f.wait())
  }

  func testResolve() {
    let f = Future<Never, Int>()
    DispatchQueue.global(qos: .background).async {
      f.resolve(with: 42)
    }
    let r = try! f.wait()
    XCTAssertEqual(42, r)
  }

  func testMap() {
    let f = Future<Never, Int>()
    let g = f.map { $0 * 2 }
    f.resolve(with: 42)
    XCTAssertEqual(84, try! g.wait())
  }

  func testTimeout() {
    let f = Future<Never, Int>()
    switch f.wait(timeout: .now() + 0.5) {
    case .ok, .error:
      XCTFail()
    case .timedOut:
      XCTAssertTrue(true)
    }
  }
}
