import Foundation

/// An input port that reads from an in-memory buffer.
public final class DataInputPort: InputPort {
  var data: Data
  var pos = 0

  public init(data: Data) {
    self.data = data
  }

  public func read(_ data: inout Data, count n: Int) -> Int {
    let n = min(self.data.count-pos, n)
    _ = data.withUnsafeMutableBytes { (buf: UnsafeMutableRawBufferPointer) in
      self.data.copyBytes(to: buf, from: pos..<pos+n)
    }
    pos += n
    return n
  }

  public func readByte() -> UInt8 {
    let b = data[pos]
    pos += 1
    return b
  }
}
