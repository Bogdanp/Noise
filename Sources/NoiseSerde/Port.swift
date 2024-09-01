import Foundation

/// Wraps a `FileHandle` to add buffered reading support.
public final class InputPort: Sendable {
  private let fd: Int32
  private let bufsize: Int
  private var buf: Data!
  private var cnt = 0
  private var idx = 0

  public init(withHandle h: FileHandle, andBufSize bufsize: Int = 8192) {
    self.fd = h.fileDescriptor
    self.buf = Data(count: bufsize)
    self.bufsize = bufsize
  }

  /// Reads up to `count` bytes from the handle into `data`, returning
  /// the number of bytes read.  Returns 0 at EOF.
  public func read(_ data: inout Data, count n: Int) -> Int {
    var pos = 0
    var want = n
    while want > 0 {
      let nread = data[pos..<n].withUnsafeMutableBytes { read($0, count: want) }
      if nread == 0 {
        return pos
      }
      want -= nread
      pos += nread
    }
    return pos
  }

  /// Reads up to `count` bytes from the handle into `out`, returning
  /// the number of bytes read.  Returns 0 at EOF.
  public func read(_ out: UnsafeMutableRawBufferPointer, count want: Int) -> Int {
    more()
    if cnt == 0 {
      return 0
    }
    let have = cnt - idx
    if have >= want {
      out.copyBytes(from: buf[idx..<idx+want])
      idx += want
      return want
    }
    out.copyBytes(from: buf[idx..<cnt])
    idx += have
    return have
  }

  /// Reads a single byte from the handle.
  public func readByte() -> UInt8 {
    repeat {
      more()
    } while (cnt == 0)
    let res = buf[idx]
    idx += 1
    return res
  }

  private func more() {
    if idx < cnt {
      return
    }
    cnt = buf.withUnsafeMutableBytes{ Darwin.read(fd, $0.baseAddress!, bufsize) }
    idx = 0
  }
}

/// Wraps a `FileHandle` to add buffered writing support.
public final class OutputPort: Sendable {
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
