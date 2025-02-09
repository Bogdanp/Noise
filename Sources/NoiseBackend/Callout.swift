import Dispatch
import Foundation
import NoiseSerde

public func installCallback(
  id: UInt64,
  rpc: (UVarint, Varint) -> Future<String, Void>,
  proc: @escaping (InputPort) -> Void
) -> Future<String, Void> {
  callbacksSema.wait()
  callbacks[id] = proc
  callbacksSema.signal()
  let addr = Int(bitPattern: unsafeBitCast(callbackHandler, to: Optional<UnsafeRawPointer>.self)!)
  return rpc(id, Int64(addr))
}

nonisolated(unsafe) fileprivate var callbacks = [UInt64: (InputPort) -> Void]()
nonisolated(unsafe) fileprivate var callbacksSema = DispatchSemaphore(value: 1)
nonisolated(unsafe) fileprivate let callbackHandler: @convention(c) (UInt64, Int, UnsafePointer<CChar>) -> Void = { id, len, ptr in
  let data = Data(bytes: ptr, count: len)
  let pipe = Pipe()
  DispatchQueue.global(qos: .background).async {
    try! pipe.fileHandleForWriting.write(contentsOf: data)
    try! pipe.fileHandleForWriting.close()
  }
  callbacksSema.wait()
  let proc = callbacks[id]
  callbacksSema.signal()
  proc!(FileHandleInputPort(withHandle: pipe.fileHandleForReading))
}
