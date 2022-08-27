import Foundation

/// The protocol for readable serde values.
public protocol Readable {
  static func read(from inp: InputPort, using buf: inout Data) -> Self?
}

/// The protocol for writable serde values.
public protocol Writable {
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

extension Float32: Readable, Writable {
  public static func read(from inp: InputPort, using buf: inout Data) -> Float32? {
    guard let b0 = inp.readByte() else { return nil }
    guard let b1 = inp.readByte() else { return nil }
    guard let b2 = inp.readByte() else { return nil }
    guard let b3 = inp.readByte() else { return nil }
    return Float32(bitPattern: (
      UInt32(b0) << 24 |
      UInt32(b1) << 16 |
      UInt32(b2) << 8  |
      UInt32(b3)
    ))
  }

  public func write(to out: OutputPort) {
    let bits = self.bitPattern
    out.writeByte(UInt8(truncatingIfNeeded: bits >> 24 & 0xFF))
    out.writeByte(UInt8(truncatingIfNeeded: bits >> 16 & 0xFF))
    out.writeByte(UInt8(truncatingIfNeeded: bits >> 8  & 0xFF))
    out.writeByte(UInt8(truncatingIfNeeded: bits       & 0xFF))
  }
}

extension Float64: Readable, Writable {
  public static func read(from inp: InputPort, using buf: inout Data) -> Float64? {
    guard let b0 = inp.readByte() else { return nil }
    guard let b1 = inp.readByte() else { return nil }
    guard let b2 = inp.readByte() else { return nil }
    guard let b3 = inp.readByte() else { return nil }
    guard let b4 = inp.readByte() else { return nil }
    guard let b5 = inp.readByte() else { return nil }
    guard let b6 = inp.readByte() else { return nil }
    guard let b7 = inp.readByte() else { return nil }
    return Float64(bitPattern: (
      UInt64(b0) << 56 |
      UInt64(b1) << 48 |
      UInt64(b2) << 40 |
      UInt64(b3) << 32 |
      UInt64(b4) << 24 |
      UInt64(b5) << 16 |
      UInt64(b6) << 8  |
      UInt64(b7)
    ))
  }

  public func write(to out: OutputPort) {
    let bits = self.bitPattern
    out.writeByte(UInt8(truncatingIfNeeded: bits >> 56 & 0xFF))
    out.writeByte(UInt8(truncatingIfNeeded: bits >> 48 & 0xFF))
    out.writeByte(UInt8(truncatingIfNeeded: bits >> 40 & 0xFF))
    out.writeByte(UInt8(truncatingIfNeeded: bits >> 32 & 0xFF))
    out.writeByte(UInt8(truncatingIfNeeded: bits >> 24 & 0xFF))
    out.writeByte(UInt8(truncatingIfNeeded: bits >> 16 & 0xFF))
    out.writeByte(UInt8(truncatingIfNeeded: bits >> 8  & 0xFF))
    out.writeByte(UInt8(truncatingIfNeeded: bits       & 0xFF))
  }
}

extension Int16: Readable, Writable {
  public static func read(from inp: InputPort, using buf: inout Data) -> Int16? {
    guard let b0 = inp.readByte() else { return nil }
    guard let b1 = inp.readByte() else { return nil }
    return Int16(bitPattern: UInt16(b0) << 8 | UInt16(b1))
  }

  public func write(to out: OutputPort) {
    out.writeByte(UInt8(truncatingIfNeeded: self >> 8 & 0xFF))
    out.writeByte(UInt8(truncatingIfNeeded: self      & 0xFF))
  }
}

extension Int32: Readable, Writable {
  public static func read(from inp: InputPort, using buf: inout Data) -> Int32? {
    guard let b0 = inp.readByte() else { return nil }
    guard let b1 = inp.readByte() else { return nil }
    guard let b2 = inp.readByte() else { return nil }
    guard let b3 = inp.readByte() else { return nil }
    return Int32(bitPattern: UInt32(b0) << 24 | UInt32(b1) << 16 | UInt32(b2) << 8 | UInt32(b3))
  }

  public func write(to out: OutputPort) {
    out.writeByte(UInt8(truncatingIfNeeded: self >> 24 & 0xFF))
    out.writeByte(UInt8(truncatingIfNeeded: self >> 16 & 0xFF))
    out.writeByte(UInt8(truncatingIfNeeded: self >> 8  & 0xFF))
    out.writeByte(UInt8(truncatingIfNeeded: self       & 0xFF))
  }
}

extension UInt16: Readable, Writable {
  public static func read(from inp: InputPort, using buf: inout Data) -> UInt16? {
    guard let b0 = inp.readByte() else { return nil }
    guard let b1 = inp.readByte() else { return nil }
    return UInt16(bigEndian: UInt16(b0) << 8 | UInt16(b1))
  }

  public func write(to out: OutputPort) {
    out.writeByte(UInt8(truncatingIfNeeded: self >> 8 & 0xFF))
    out.writeByte(UInt8(truncatingIfNeeded: self      & 0xFF))
  }
}

extension UInt32: Readable, Writable {
  public static func read(from inp: InputPort, using buf: inout Data) -> UInt32? {
    guard let b0 = inp.readByte() else { return nil }
    guard let b1 = inp.readByte() else { return nil }
    guard let b2 = inp.readByte() else { return nil }
    guard let b3 = inp.readByte() else { return nil }
    return UInt32(bigEndian: UInt32(b0) << 24 | UInt32(b1) << 16 | UInt32(b2) << 8 | UInt32(b3))
  }

  public func write(to out: OutputPort) {
    out.writeByte(UInt8(truncatingIfNeeded: self >> 24 & 0xFF))
    out.writeByte(UInt8(truncatingIfNeeded: self >> 16 & 0xFF))
    out.writeByte(UInt8(truncatingIfNeeded: self >> 8  & 0xFF))
    out.writeByte(UInt8(truncatingIfNeeded: self       & 0xFF))
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

public typealias Symbol = String

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
