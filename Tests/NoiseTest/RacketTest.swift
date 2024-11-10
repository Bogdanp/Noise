import Noise
import XCTest

nonisolated(unsafe) var r: Racket!

class RacketTest: XCTestCase {
  override class func setUp() {
    // Runtime-paths are relative to `(find-system-path 'exec-file)`,
    // so our bundle needs to contain an `exec-file` (that doesn't
    // have to actually be executable) from which the runtime folder
    // path can be derived.  So, the easiest solution in this case is
    // to add a "cookie" file whose sole purpose is to help us work
    // out the paths.
    let cookiePath = Bundle.module.resourceURL!
      .appendingPathComponent("Modules")
      .appendingPathComponent("cookie")
      .path

    r = Racket(execPath: cookiePath)
    r.bracket {
      r.load(zo: Bundle.module.url(forResource: "Modules/mods", withExtension: "zo")!)
    }
  }

  override class func tearDown() {
    r.destroy()
  }

  func testApplication() {
    r.bracket {
      let mod = Val.cons(Val.symbol("quote"), Val.cons(Val.symbol("fib"), Val.null))
      let fib = r.require(Val.symbol("fib"), from: mod).car()!
      XCTAssertEqual(fib.apply(Val.cons(Val.fixnum(0), Val.null))!.car()!.fixnum(), 0)
      XCTAssertEqual(fib.apply(Val.cons(Val.fixnum(1), Val.null))!.car()!.fixnum(), 1)
      XCTAssertEqual(fib.apply(Val.cons(Val.fixnum(2), Val.null))!.car()!.fixnum(), 1)
      XCTAssertEqual(fib.apply(Val.cons(Val.fixnum(8), Val.null))!.car()!.fixnum(), 21)
    }
  }

  func testBytestring() {
    r.bracket {
      let mod = Val.cons(Val.symbol("quote"), Val.cons(Val.symbol("loud"), Val.null))
      let exclaim = r.require(Val.symbol("exclaim"), from: mod).car()!
      let res = exclaim.apply(Val.cons(Val.string("hello"), Val.null))!
      XCTAssertEqual(res.car()?.bytestring()!, "hello!!!")
    }
  }

  func testBytevector() {
    r.bracket {
      let mod = Val.cons(Val.symbol("quote"), Val.cons(Val.symbol("bytes"), Val.null))
      let get = r.require(Val.symbol("get-bytes"), from: mod).car()!
      let res = get.apply(Val.null)!.car()!.bytevector()!.map({ UInt8(bitPattern: $0) })
      XCTAssertEqual(res, [1, 2, 128, 255])
    }
  }

  func testHttp() {
    r.bracket {
      let mod = Val.cons(Val.symbol("quote"), Val.cons(Val.symbol("http"), Val.null))
      let proc = r.require(Val.symbol("get"), from: mod).car()!
      let res = proc.apply(Val.cons(Val.string("defn.io"), Val.null))!
      let status = res.car()!.fixnum()!
      let content = res.cdr()!.car()!.bytestring()!
      XCTAssertEqual(status, 200)

      let re = try! NSRegularExpression(pattern: "<title>Posts &mdash; defn.io</title>")
      let matches = re.numberOfMatches(in: content, range: NSMakeRange(0, content.count))
      XCTAssertEqual(matches, 1)
    }
  }

  func testCallout() {
    r.bracket {
      let mod = Val.cons(Val.symbol("quote"), Val.cons(Val.symbol("callout"), Val.null)).locked()
      defer { mod.unlock() }

      let install = r.require(Val.symbol("install-callout!"), from: mod).unsafeCar()
      let ptr = unsafeBitCast(calloutExampleProc, to: Optional<UnsafeMutableRawPointer>.self)!
      install.unsafeApply(Val.cons(Val.pointer(ptr), Val.null))

      let call = r.require(Val.symbol("exec-callout"), from: mod).unsafeCar()
      call.unsafeApply(Val.null)
    }
    XCTAssertEqual(calloutResult, "hello")
  }
}

nonisolated(unsafe) fileprivate var calloutResult: String?
nonisolated(unsafe) fileprivate var calloutExampleProc: @convention(c) (Int, UnsafePointer<CChar>) -> Void = { len, ptr in
  let data = Data(bytes: ptr, count: len)
  calloutResult = String(data: data, encoding: .utf8)!
}
