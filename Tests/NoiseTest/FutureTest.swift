import Dispatch
import NoiseBackend
import XCTest

class FutureTests: XCTestCase {
  func testRejected() {
    let f = Future<String, Never>.rejected(with: "failed")
    XCTAssertThrowsError(try f.wait())
  }

  func testResolved() {
    let f = Future<Never, Int>.resolved(with: 42)
    let r = try! f.wait()
    XCTAssertEqual(42, r)
  }

  func testReject() {
    let f = Future<String, Never>()
    DispatchQueue.global(qos: .background).async {
      f.reject(with: "failed")
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

  func testMapError() {
    let f = Future<String, Never>()
    f.reject(with: "failed")
    let g = f.mapError { $0.uppercased() }
    switch g.wait(timeout: .now() + 0.5) {
    case .error(let err):
      XCTAssertEqual("FAILED", err)
    default:
      XCTFail()
    }
  }

  func testAndThen() {
    let f = Future<Never, String>()
    f.resolve(with: "hello")
    let g: Future<Never, Int> = f.andThen { str in
      let g = Future<Never, Int>()
      g.resolve(with: str.count)
      return g
    }
    XCTAssertEqual(5, try! g.wait())
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
