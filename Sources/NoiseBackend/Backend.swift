import Dispatch
import Foundation
import Noise
import NoiseSerde

/// Statistics reported by Backends.
public struct BackendStats {
  public let totalRequests: UInt64
  public let totalWaitNanos: UInt64
  public let totalReadNanos: UInt64
  public let totalWriteNanos: UInt64
}

/// Client implementation for an async Racket backend.
public class Backend {
  private let ip = Pipe() // in  from Racket's perspective
  private let op = Pipe() // out from Racket's perspective

  private let mu = DispatchSemaphore(value: 1) // mu guards everything below here
  private let out: OutputPort!
  private var seq = UVarint(0)
  fileprivate var pending = [UInt64: ResponseHandler]()
  private var totalRequests = UInt64(0)
  private var totalWaitNanos = UInt64(0)
  private var totalReadNanos = UInt64(0)
  private var totalWriteNanos = UInt64(0)

  public init(withZo zo: URL, andMod mod: String, andProc proc: String) {
    out = OutputPort(withHandle: ip.fileHandleForWriting)
    Thread.detachNewThread {
      self.serve(zo, mod, proc)
    }
    Thread.detachNewThread {
      self.read()
    }
  }

  private func serve(_ zo: URL, _ mod: String, _ proc: String) {
    let r = Racket(execPath: zo.path)
    r.bracket {
      r.load(zo: zo)
      let serve = r.require(Val.symbol(proc), from: Val.cons(Val.symbol("quote"), Val.cons(Val.symbol(mod), Val.null))).car()!
      let ifd = Val.fixnum(Int(ip.fileHandleForReading.fileDescriptor))
      let ofd = Val.fixnum(Int(op.fileHandleForWriting.fileDescriptor))
      let _ = serve.apply(Val.cons(ifd, Val.cons(ofd, Val.null)))!
      preconditionFailure("Racket server exited")
    }
  }

  private func read() {
    let inp = InputPort(withHandle: op.fileHandleForReading)
    var buf = Data(count: 8*1024) // will grow as needed
    while true {
      let id = UVarint.read(from: inp, using: &buf)
      mu.wait()
      guard let handler = pending[id] else{
        mu.signal()
        continue
      }
      mu.signal()
      let dt = handler.handle(from: inp, using: &buf)
      mu.wait()
      pending.removeValue(forKey: id)
      totalRequests += 1
      totalWaitNanos += DispatchTime.now().uptimeNanoseconds - handler.time.uptimeNanoseconds
      totalReadNanos += dt
      mu.signal()
    }
  }

  public func send<T>(
    writeProc write: (OutputPort) -> Void,
    readProc read: @escaping (InputPort, inout Data) -> T
  ) -> Future<T> {
    mu.wait()
    let id = seq
    seq += 1
    let t0 = DispatchTime.now()
    id.write(to: out)
    write(out)
    out.flush()
    let dt = DispatchTime.now().uptimeNanoseconds - t0.uptimeNanoseconds
    let fut = Future<T>()
    let handler = ResponseHandlerImpl<T>(id: id, fut: fut, read: read)
    pending[id] = handler
    totalWriteNanos += dt
    mu.signal()
    return fut
  }

  public func stats() -> BackendStats {
    mu.wait()
    defer { mu.signal() }
    return BackendStats(
      totalRequests: totalRequests,
      totalWaitNanos: totalWaitNanos,
      totalReadNanos: totalReadNanos,
      totalWriteNanos: totalWriteNanos
    )
  }
}

fileprivate struct Request<Data: Writable>: Writable {
  let id: UVarint
  let data: Data

  func write(to out: OutputPort) {
    id.write(to: out)
    data.write(to: out)
  }
}

fileprivate protocol ResponseHandler {
  var time: DispatchTime { get }

  func handle(from inp: InputPort, using buf: inout Data) -> UInt64
}

fileprivate struct ResponseHandlerImpl<T>: ResponseHandler {
  let id: UInt64
  let fut: Future<T>
  let read: (InputPort, inout Data) -> T
  let time = DispatchTime.now()

  func handle(from inp: InputPort, using buf: inout Data) -> UInt64 {
    let t0 = DispatchTime.now()
    let data = read(inp, &buf)
    let dt = DispatchTime.now().uptimeNanoseconds - t0.uptimeNanoseconds
    fut.resolve(with: data)
    return dt
  }
}
