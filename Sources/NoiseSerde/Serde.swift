import Foundation

/// The protocol for readable serde values.
public protocol Readable where Self: Equatable {
  static func read(from inp: InputPort, using buf: inout Data) -> Self?
}

/// The protocol for writable serde values.
public protocol Writable where Self: Equatable {
  func write(to out: OutputPort)
}

extension Bool: Readable, Writable {
  public static func read(from inp: InputPort, using buf: inout Data) -> Bool? {
    return inp.readByte() == 1
  }

  public func write(to out: OutputPort) {
    out.writeByte(self ? 1 : 0)
  }
}

extension Data: Readable, Writable {
  public static func read(from inp: InputPort, using buf: inout Data) -> Data? {
    guard let vlen = Varint.read(from: inp, using: &buf) else {
      return nil
    }
    assert(vlen >= 0)
    if vlen == 0 {
      return Data(count: 0)
    }
    let len = Int(vlen)
    buf.grow(upTo: len)
    if inp.read(&buf, count: len) < len {
      return nil
    }
    return buf[0..<len]
  }

  public func write(to out: OutputPort) {
    Varint(count).write(to: out)
    out.write(contentsOf: self)
  }
}

public typealias Varint = Int64

extension Varint: Readable, Writable {
  public static func read(from inp: InputPort, using buf: inout Data) -> Varint? {
    var s = Varint(0)
    var n = Varint(0)
    while true {
      guard let b = inp.readByte() else {
        return nil
      }
      let x = Int64(b)
      if x & 0x80 == 0 {
        n += x << s
        break
      }
      n += (x & 0x7F) << s
      s += 7
    }
    if n & 1 == 0 {
      return n >> 1
    }
    return ~(n >> 1)
  }

  public func write(to out: OutputPort) {
    var n = (self << 1) ^ (self < 0 ? -1 : 0)
    while true {
      let (q, r) = n.quotientAndRemainder(dividingBy: 0x80)
      if q == 0 {
        out.writeByte(UInt8(truncatingIfNeeded: r))
        break
      } else {
        out.writeByte(UInt8(truncatingIfNeeded: r|0x80))
        n = q
      }
    }
  }
}

public typealias UVarint = UInt64

extension UVarint: Readable, Writable {
  public static func read(from inp: InputPort, using buf: inout Data) -> UVarint? {
    var s = UVarint(0)
    var n = UVarint(0)
    while true {
      guard let b = inp.readByte() else {
        return nil
      }
      let x = UInt64(b)
      if x & 0x80 == 0 {
        n += x << s
        break
      }
      n += (x & 0x7F) << s
      s += 7
    }
    return n
  }

  public func write(to out: OutputPort) {
    var n = self
    while true {
      let (q, r) = n.quotientAndRemainder(dividingBy: 0x80)
      if q == 0 {
        out.writeByte(UInt8(truncatingIfNeeded: r))
        break
      } else {
        out.writeByte(UInt8(truncatingIfNeeded: r|0x80))
        n = q
      }
    }
  }
}

extension String: Readable, Writable {
  public static func read(from inp: InputPort, using buf: inout Data) -> String? {
    guard let vlen = Varint.read(from: inp, using: &buf) else {
      return nil
    }
    assert(vlen >= 0)
    if vlen == 0 {
      return ""
    }
    let len = Int(vlen)
    buf.grow(upTo: len)
    if inp.read(&buf, count: len) < len {
      return nil
    }
    guard let str = String(data: buf[0..<len], encoding: .utf8) else {
      return nil
    }
    return str
  }

  public func write(to out: OutputPort) {
    let data = data(using: .utf8)!
    Varint(data.count).write(to: out)
    out.write(contentsOf: data)
  }
}

typealias Symbol = String

extension Array where Element: Readable, Element: Writable {
  public static func read(from inp: InputPort, using buf: inout Data) -> [Element]? {
    guard let len = Varint.read(from: inp, using: &buf) else {
      return nil
    }
    assert(len >= 0)
    if len == 0 {
      return []
    }
    var res = [Element]()
    for _ in 0..<len {
      guard let r = Element.read(from: inp, using: &buf) else {
        return nil
      }
      res.append(r)
    }
    return res
  }

  public func write(to out: OutputPort) {
    Varint(count).write(to: out)
    for r in self {
      r.write(to: out)
    }
  }
}

fileprivate extension Data {
  mutating func grow(upTo n: Int) {
    let want = n - count
    if want <= 0 {
      return
    }
    reserveCapacity(n)
    append(Data(count: want))
  }
}
