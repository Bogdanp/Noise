import NoiseSerde
import XCTest

class SerdeTests: XCTestCase {
  func testInputPortBuffering() {
    let bufsizes = [1, 2, 4, 8, 1024]
    for bufsize in bufsizes {
      let p = Pipe()
      let inp = FileHandleInputPort(withHandle: p.fileHandleForReading, andBufSize: bufsize)
      let out = FileHandleOutputPort(withHandle: p.fileHandleForWriting)
      out.write(contentsOf: "hello".data(using: .utf8)!)
      out.flush()
      var buf = Data(count: 8192)
      let nread = inp.read(&buf, count: 5)
      XCTAssertEqual(5, nread)
      XCTAssertEqual("hello", String(data: buf[0..<5], encoding: .utf8))
    }
  }

  func testFloat64Roundtrip() {
    let p = Pipe()
    let inp = FileHandleInputPort(withHandle: p.fileHandleForReading)
    let out = FileHandleOutputPort(withHandle: p.fileHandleForWriting)
    let tests: [Float64] = [0, 0.5, 1.25, -5.0]
    var buf = Data(count: 8192)
    for n in tests {
      n.write(to: out)
      out.flush()
      XCTAssertEqual(n, Float64.read(from: inp, using: &buf))
    }
  }

  func testInt32Roundtrip() {
    let p = Pipe()
    let inp = FileHandleInputPort(withHandle: p.fileHandleForReading)
    let out = FileHandleOutputPort(withHandle: p.fileHandleForWriting)
    let tests: [Int32] = [0, 0xFF, -0xFF, 0x7FFFFFFF, -0x7FFFFFFF]
    var buf = Data(count: 8192)
    for n in tests {
      n.write(to: out)
      out.flush()
      XCTAssertEqual(n, Int32.read(from: inp, using: &buf))
    }
  }

  func testVarintRoundtrip() {
    let p = Pipe()
    let inp = FileHandleInputPort(withHandle: p.fileHandleForReading)
    let out = FileHandleOutputPort(withHandle: p.fileHandleForWriting)
    let tests: [Varint] = [
      0x0, 0x1, -0x1, 0x7F, -0x7F, 0x80, -0x80,
      0xFF, -0xFF, 0xFFF, -0xFFF, 0xFFFFF, -0xFFFFF,
    ]
    var buf = Data(count: 8192)
    for n in tests {
      n.write(to: out)
      out.flush()
      XCTAssertEqual(n, Varint.read(from: inp, using: &buf))
    }
  }

  func testUVarintRoundtrip() {
    let p = Pipe()
    let inp = FileHandleInputPort(withHandle: p.fileHandleForReading)
    let out = FileHandleOutputPort(withHandle: p.fileHandleForWriting)
    let tests: [UVarint] = [0x0, 0x1, 0x7F, 0x80, 0xFF, 0xFFF, 0xFFFFF]
    var buf = Data(count: 8192)
    for n in tests {
      n.write(to: out)
      out.flush()
      XCTAssertEqual(n, UVarint.read(from: inp, using: &buf))
    }
  }

  func testArrayRoundtrip() {
    let p = Pipe()
    let inp = FileHandleInputPort(withHandle: p.fileHandleForReading)
    let out = FileHandleOutputPort(withHandle: p.fileHandleForWriting)
    let data: [String] = ["hello", "there"]
    var buf = Data(count: 8192)
    data.write(to: out)
    out.flush()
    let res = [String].read(from: inp, using: &buf)
    XCTAssertEqual(data, res)
  }

  func testOptionalRoundtrip() {
    let p = Pipe()
    let inp = FileHandleInputPort(withHandle: p.fileHandleForReading)
    let out = FileHandleOutputPort(withHandle: p.fileHandleForWriting)
    var buf = Data(count: 8192)

    // empty
    var data: String?
    data.write(to: out)
    out.flush()
    XCTAssertEqual(data, String?.read(from: inp, using: &buf))

    // full
    data = "hello, world"
    data.write(to: out)
    out.flush()
    XCTAssertEqual(data, String?.read(from: inp, using: &buf))
  }

  func testStringSmallBuffer() {
    let p = Pipe()
    let s = "hello, world!"
    let inp = FileHandleInputPort(withHandle: p.fileHandleForReading)
    let out = FileHandleOutputPort(withHandle: p.fileHandleForWriting)
    var buf = Data(count: 5)
    s.write(to: out)
    out.flush()
    XCTAssertEqual(s, String.read(from: inp, using: &buf))
    XCTAssertEqual(s.count, buf.count)
  }

  func testStringSmallBufferViaDataPorts() {
    let s = "hello, world!"
    let out = DataOutputPort()
    s.write(to: out)
    let inp = DataInputPort(data: out.data)
    var buf = Data(count: 5)
    XCTAssertEqual(s, String.read(from: inp, using: &buf))
    XCTAssertEqual(s.count, buf.count)
  }

  func testStringRoundtripPerformance() {
    let p = Pipe()
    let s = "hello, world!"
    let inp = FileHandleInputPort(withHandle: p.fileHandleForReading)
    let out = FileHandleOutputPort(withHandle: p.fileHandleForWriting)
    var buf = Data(count: 8192)
    let opts = XCTMeasureOptions()
    opts.iterationCount = 1000
    measure(options: opts) {
      s.write(to: out)
      out.flush()
      let _ = String.read(from: inp, using: &buf)
    }
  }

  func testDictionaryRoundtrip() {
    let p = Pipe()
    let v = [
      "a": UVarint(42),
      "b": UVarint(10)
    ]
    let inp = FileHandleInputPort(withHandle: p.fileHandleForReading)
    let out = FileHandleOutputPort(withHandle: p.fileHandleForWriting)
    var buf = Data(count: 8192)
    v.write(to: out)
    out.flush()
    let r = [String: UVarint].read(from: inp, using: &buf)
    XCTAssertEqual(v, r)
  }

  func testDictionaryRoundtripViaDataPorts() {
    let v = [
      "a": UVarint(42),
      "b": UVarint(10),
    ]
    let out = DataOutputPort()
    v.write(to: out)
    let inp = DataInputPort(data: out.data)
    var buf = Data(count: 8192)
    let r = [String: UVarint].read(from: inp, using: &buf)
    XCTAssertEqual(v, r)
  }
}
