import Foundation

/// An output port that buffers all writes into memory.
public final class DataOutputPort: OutputPort {
  public var data: Data!

  public init(capacity: Int = 64*1024) {
    data = Data(capacity: capacity)
  }

  public func write(contentsOf data: Data) {
    self.data.append(data)
  }

  public func writeByte(_ b: UInt8) {
    self.data.append(contentsOf: [b])
  }

  public func flush() {}
}
