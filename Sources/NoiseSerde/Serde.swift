import Foundation

/// The protocol for readable serde values.
public protocol Readable {
  static func read(from inp: InputPort, using buf: inout Data) -> Self
}

/// The protocol for writable serde values.
public protocol Writable {
  func write(to out: OutputPort)
}

// MARK: - Bool
extension Bool: Readable, Writable {
  public static func read(from inp: InputPort, using buf: inout Data) -> Bool {
    return inp.readByte() == 1
  }

  public func write(to out: OutputPort) {
    out.writeByte(self ? 1 : 0)
  }
}

// MARK: - Data
extension Data: Readable, Writable {
  public static func read(from inp: InputPort, using buf: inout Data) -> Data {
    let vlen = Varint.read(from: inp, using: &buf)
    assert(vlen >= 0)
    if vlen == 0 {
      return Data(count: 0)
    }
    let len = Int(vlen)
    buf.grow(upTo: len)
    let nread = inp.read(&buf, count: len)
    if nread < len {
      preconditionFailure("Data: received \(nread) bytes but expected \(len)")
    }
    return try! buf.withUnsafeBytes { ptr throws in
      return Data(bytes: ptr.baseAddress!, count: len)
    }
  }

  public func write(to out: OutputPort) {
    Varint(count).write(to: out)
    out.write(contentsOf: self)
  }
}

// MARK: - Float32
extension Float32: Readable, Writable {
  public static func read(from inp: InputPort, using buf: inout Data) -> Float32 {
    return Float32(bitPattern: (
      UInt32(inp.readByte()) << 24 |
      UInt32(inp.readByte()) << 16 |
      UInt32(inp.readByte()) << 8  |
      UInt32(inp.readByte())
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

// MARK: - Float64
extension Float64: Readable, Writable {
  public static func read(from inp: InputPort, using buf: inout Data) -> Float64 {
    return Float64(bitPattern: (
      UInt64(inp.readByte()) << 56 |
      UInt64(inp.readByte()) << 48 |
      UInt64(inp.readByte()) << 40 |
      UInt64(inp.readByte()) << 32 |
      UInt64(inp.readByte()) << 24 |
      UInt64(inp.readByte()) << 16 |
      UInt64(inp.readByte()) << 8  |
      UInt64(inp.readByte())
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

// MARK: - Int16
extension Int16: Readable, Writable {
  public static func read(from inp: InputPort, using buf: inout Data) -> Int16 {
    return Int16(bitPattern: (
      UInt16(inp.readByte()) << 8 |
      UInt16(inp.readByte())
    ))
  }

  public func write(to out: OutputPort) {
    out.writeByte(UInt8(truncatingIfNeeded: self >> 8 & 0xFF))
    out.writeByte(UInt8(truncatingIfNeeded: self      & 0xFF))
  }
}

// MARK: - Int32
extension Int32: Readable, Writable {
  public static func read(from inp: InputPort, using buf: inout Data) -> Int32 {
    return Int32(bitPattern: (
      UInt32(inp.readByte()) << 24 |
      UInt32(inp.readByte()) << 16 |
      UInt32(inp.readByte()) <<  8 |
      UInt32(inp.readByte())
    ))
  }

  public func write(to out: OutputPort) {
    out.writeByte(UInt8(truncatingIfNeeded: self >> 24 & 0xFF))
    out.writeByte(UInt8(truncatingIfNeeded: self >> 16 & 0xFF))
    out.writeByte(UInt8(truncatingIfNeeded: self >> 8  & 0xFF))
    out.writeByte(UInt8(truncatingIfNeeded: self       & 0xFF))
  }
}

// MARK: - UInt16
extension UInt16: Readable, Writable {
  public static func read(from inp: InputPort, using buf: inout Data) -> UInt16 {
    return UInt16(bitPattern: (
      Int16(inp.readByte()) << 8 |
      Int16(inp.readByte())
    ))
  }

  public func write(to out: OutputPort) {
    out.writeByte(UInt8(truncatingIfNeeded: self >> 8 & 0xFF))
    out.writeByte(UInt8(truncatingIfNeeded: self      & 0xFF))
  }
}

// MARK: - UInt32
extension UInt32: Readable, Writable {
  public static func read(from inp: InputPort, using buf: inout Data) -> UInt32 {
    return UInt32(bitPattern: (
      Int32(inp.readByte()) << 24 |
      Int32(inp.readByte()) << 16 |
      Int32(inp.readByte()) <<  8 |
      Int32(inp.readByte())
    ))
  }

  public func write(to out: OutputPort) {
    out.writeByte(UInt8(truncatingIfNeeded: self >> 24 & 0xFF))
    out.writeByte(UInt8(truncatingIfNeeded: self >> 16 & 0xFF))
    out.writeByte(UInt8(truncatingIfNeeded: self >> 8  & 0xFF))
    out.writeByte(UInt8(truncatingIfNeeded: self       & 0xFF))
  }
}

// MARK: - Varint
public typealias Varint = Int64

extension Varint: Readable, Writable {
  public static func read(from inp: InputPort, using buf: inout Data) -> Varint {
    var s = Varint(0)
    var n = Varint(0)
    while true {
      let b = inp.readByte()
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

// MARK: - UVarint
public typealias UVarint = UInt64

extension UVarint: Readable, Writable {
  public static func read(from inp: InputPort, using buf: inout Data) -> UVarint {
    var s = UVarint(0)
    var n = UVarint(0)
    while true {
      let b = inp.readByte()
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

// MARK: - String
extension String: Readable, Writable {
  public static func read(from inp: InputPort, using buf: inout Data) -> String {
    let vlen = Varint.read(from: inp, using: &buf)
    assert(vlen >= 0)
    if vlen == 0 {
      return ""
    }
    let len = Int(vlen)
    buf.grow(upTo: len)
    let nread = inp.read(&buf, count: len)
    if nread < len {
      preconditionFailure("String: received \(nread) bytes, but expected \(len)")
    }
    guard let str = String(data: buf[0..<len], encoding: .utf8) else {
      preconditionFailure("String: invalid UTF-8 bytes")
    }
    return str
  }

  public func write(to out: OutputPort) {
    let data = data(using: .utf8)!
    Varint(data.count).write(to: out)
    out.write(contentsOf: data)
  }
}

// MARK: - Symbol
public typealias Symbol = String

// MARK: - Array
extension Array: Readable where Element: Readable {
  public static func read(from inp: InputPort, using buf: inout Data) -> [Element] {
    let len = Varint.read(from: inp, using: &buf)
    assert(len >= 0)
    if len == 0 {
      return []
    }
    var res = [Element]()
    res.reserveCapacity(Int(len))
    for _ in 0..<len {
      res.append(Element.read(from: inp, using: &buf))
    }
    return res
  }
}

extension Array: Writable where Element: Writable {
  public func write(to out: OutputPort) {
    Varint(count).write(to: out)
    for r in self {
      r.write(to: out)
    }
  }
}

// MARK: - Dictionary
extension Dictionary: Readable where Key: Readable, Value: Readable {
  public static func read(from inp: InputPort, using buf: inout Data) -> [Key: Value] {
    let len = Varint.read(from: inp, using: &buf)
    assert(len >= 0)
    if len == 0 {
      return [:]
    }
    var res = [Key: Value]()
    res.reserveCapacity(Int(len))
    for _ in 0..<len {
      let k = Key.read(from: inp, using: &buf)
      let v = Value.read(from: inp, using: &buf)
      res[k] = v
    }
    return res
  }
}

extension Dictionary: Writable where Key: Writable, Value: Writable {
  public func write(to out: OutputPort) {
    Varint(count).write(to: out)
    for (k, v) in self {
      k.write(to: out)
      v.write(to: out)
    }
  }
}

// MARK: - Optional
extension Optional: Readable where Wrapped: Readable {
  public static func read(from inp: InputPort, using buf: inout Data) -> Wrapped? {
    if inp.readByte() == 0 {
      return nil
    }
    return Wrapped.read(from: inp, using: &buf)
  }
}

extension Optional: Writable where Wrapped: Writable {
  public func write(to out: OutputPort) {
    switch self {
    case .none:
      out.writeByte(0)
    case .some(let v):
      out.writeByte(1)
      v.write(to: out)
    }
  }
}

// MARK: - Data
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
