import Dispatch
import Foundation
import Noise
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

fileprivate var callbacks = [UInt64: (InputPort) -> Void]()
fileprivate var callbacksSema = DispatchSemaphore(value: 1)
fileprivate let callbackHandler: @convention(c) (UInt64, Int, UnsafePointer<CChar>) -> Void = { id, len, ptr in
  let data = Data(bytes: ptr, count: len)
  let pipe = Pipe()
  DispatchQueue.main.async(qos: .background) {
    try! pipe.fileHandleForWriting.write(contentsOf: data)
  }
  callbacksSema.wait()
  let proc = callbacks[id]
  callbacksSema.signal()
  proc!(InputPort(withHandle: pipe.fileHandleForReading))
}
