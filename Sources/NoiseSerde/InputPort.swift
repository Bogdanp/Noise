import Foundation

/// Wraps a `FileHandle` to add buffered reading support.
public class InputPort {
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

  /// Reads a single byte from the handle, returning `nil` on EOF.
  public func readByte() -> UInt8? {
    more()
    if cnt == 0 {
      return nil
    }
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
