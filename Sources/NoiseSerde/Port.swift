import Foundation

public protocol InputPort {
  /// Reads up to `count` bytes from the handle into `data`, returning
  /// the number of bytes read.  Returns 0 at EOF.
  func read(_ data: inout Data, count n: Int) -> Int
  /// Reads up to `count` bytes from the handle into `out`, returning
  /// the number of bytes read.  Returns 0 at EOF.
  func readByte() -> UInt8
}

public protocol OutputPort {
  /// Buffers `data` into memory if there is sufficient capacity.
  /// Otherwise, flushes any previously-buffered data and writes the
  /// new data to the handle.
  func write(contentsOf data: Data)
  /// Buffers a single byte into memory if there is sufficient
  /// capacity.  Otherwise, flushes any previously-buffered data and
  /// then buffers the byte.
  func writeByte(_ b: UInt8)
  /// Writes any buffered data to the handle.
  func flush()
}
