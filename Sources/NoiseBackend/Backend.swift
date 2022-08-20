import Dispatch
import Foundation
import Noise
import NoiseSerde

/// Statistics reported by Backends.
public struct BackendStats {
  let totalRequests: UInt64
  let totalWaitNanos: UInt64
}

/// Client implementation for an async Racket backend.
public class Backend<Record: Readable & Writable> {
  private let ip = Pipe() // in  from Racket's perspective
  private let op = Pipe() // out from Racket's perspective

  private let mu = DispatchSemaphore(value: 1) // mu guards everything below here
  private let out: OutputPort!
  private var seq = UInt64(0)
  fileprivate var pending = [UInt64: PendingResponse<Record>]()
  private var totalRequests = UInt64(0)
  private var totalWaitNanos = UInt64(0)

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
    let r = Racket()
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
      guard let id = UVarint.read(from: inp, using: &buf) else {
        preconditionFailure("failed to read response id")
      }
      guard let data = Record.read(from: inp, using: &buf) else {
        preconditionFailure("failed to read response data")
      }
      mu.wait()
      guard let req = pending[id] else {
        mu.signal()
        continue
      }
      pending.removeValue(forKey: id)
      mu.signal()
      req.fut.resolve(with: data)
      totalRequests += 1
      totalWaitNanos += DispatchTime.now().uptimeNanoseconds - req.time.uptimeNanoseconds
    }
  }

  public func send(data: Record) -> Future<Record> {
    mu.wait()
    defer { mu.signal() }
    let id = seq
    let req = Request(id: id, data: data)
    seq += 1
    req.write(to: out)
    out.flush()
    let fut = Future<Record>()
    pending[id] = PendingResponse<Record>(id: id, fut: fut)
    return fut
  }

  public func stats() -> BackendStats {
    return BackendStats(
      totalRequests: totalRequests,
      totalWaitNanos: totalWaitNanos
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

fileprivate struct PendingResponse<Record: Readable & Writable> {
  let id: UInt64
  let fut: Future<Record>
  let time = DispatchTime.now()
}
