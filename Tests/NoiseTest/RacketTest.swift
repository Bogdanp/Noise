import Noise
import XCTest

class RacketTest: XCTestCase {
  func test() {
    let r = Racket()
    r.bracket {
      r.load(zo: Bundle.module.url(forResource: "Modules/mods", withExtension: "zo")!)
    }

    r.bracket {
      let mod = Val.cons(Val.symbol("quote"), Val.cons(Val.symbol("fib"), Val.null))
      let fib = r.require(Val.symbol("fib"), from: mod).car()!
      let n = fib.apply(Val.cons(Val.fixnum(8), Val.null))!
      XCTAssertEqual(n.car()!.fixnum(), 21)
    }

    r.bracket {
      let mod = Val.cons(Val.symbol("quote"), Val.cons(Val.symbol("loud"), Val.null))
      let exclaim = r.require(Val.symbol("exclaim"), from: mod).car()!
      let res = exclaim.apply(Val.cons(Val.string("hello"), Val.null))!
      XCTAssertEqual(res.car()?.bytestring()!, "hello!!!")
    }
  }
}
