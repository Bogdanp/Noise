import Foundation

/// Wraps a `FileHandle` to add buffered writing support.
public class OutputPort {
  private let handle: FileHandle
  private var buf: Data!
  private let bufsize: Int
  private var cnt = 0

  public init(withHandle h: FileHandle, andBufSize bufsize: Int = 8192) {
    self.handle = h
    self.buf = Data(capacity: bufsize)
    self.bufsize = bufsize
  }

  /// Buffers `data` into memory if there is sufficient capacity.
  /// Otherwise, flushes any previously-buffered data and writes the
  /// new data to the handle.
  public func write(contentsOf data: Data) {
    let remaining = bufsize - cnt
    if data.count > remaining {
      flush()
      try! handle.write(contentsOf: data)
      return
    }
    buf.append(data)
    cnt += data.count
  }

  /// Buffers a single byte into memory if there is sufficient
  /// capacity.  Otherwise, flushes any previously-buffered data and
  /// then buffers the byte.
  public func writeByte(_ b: UInt8) {
    let remaining = bufsize - cnt
    if remaining == 0 {
      flush()
    }
    buf.append(b)
    cnt += 1
  }

  /// Writes any buffered data to the handle.
  public func flush() {
    if cnt == 0 {
      return
    }
    try! handle.write(contentsOf: buf[0..<cnt])
    buf.removeAll(keepingCapacity: true)
    cnt = 0
  }
}
