import Dispatch
import Foundation
import Noise
import NoiseSerde
import OSLog

nonisolated(unsafe) fileprivate let logger = Logger(
  subsystem: "io.defn.NoiseBackend",
  category: "Backend"
)

/// Statistics reported by Backends.
public struct BackendStats {
  public let totalRequests: UInt64
  public let totalWaitNanos: UInt64
  public let totalReadNanos: UInt64
  public let totalWriteNanos: UInt64
}

/// Client implementation for an async Racket backend.
public final class Backend: @unchecked Sendable {
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

  public init(withZo zo: URL, andMod modname: String, andProc proc: String) {
    out = OutputPort(withHandle: ip.fileHandleForWriting)
    let server = Thread {
      self.serve(zo, modname, proc)
    }
    server.name = "Noise Backend (Server)"
    server.qualityOfService = .userInitiated
    server.start()
    let reader = Thread {
      self.read()
    }
    reader.name = "Noise Backend (Reader)"
    reader.qualityOfService = .userInitiated
    reader.start()
  }

  private func serve(_ zo: URL, _ modname: String, _ proc: String) {
    let r = Racket(execPath: zo.path)
    r.bracket {
      r.load(zo: zo)
      let mod = Val.cons(Val.symbol("quote"), Val.cons(Val.symbol(modname), Val.null))
      let ifd = Val.fixnum(Int(ip.fileHandleForReading.fileDescriptor))
      let ofd = Val.fixnum(Int(op.fileHandleForWriting.fileDescriptor))
      let serve = r.require(Val.symbol(proc), from: mod).unsafeCar()
      serve.unsafeApply(Val.cons(ifd, Val.cons(ofd, Val.null)))
      preconditionFailure("Racket server exited")
    }
  }

  private func read() {
    let inp = InputPort(withHandle: op.fileHandleForReading)
    var buf = Data(count: 8*1024) // will grow as needed
    while true {
      let id = UVarint.read(from: inp, using: &buf)
      logger.debug("reading response \(id), bufsize: \(buf.count)b")
      mu.wait()
      guard let handler = pending[id] else {
        logger.fault("future for response \(id) gone")
        mu.signal()
        continue
      }
      mu.signal()
      let readDuration = handler.handle(from: inp, using: &buf)
      logger.debug("took \(Duration(nanos: readDuration), privacy: .public) to read response \(id)")
      mu.wait()
      pending.removeValue(forKey: id)
      let requestDuration = DispatchTime.now().uptimeNanoseconds - handler.time.uptimeNanoseconds
      totalRequests += 1
      totalWaitNanos += requestDuration
      totalReadNanos += readDuration
      mu.signal()
      logger.debug("took \(Duration(nanos: requestDuration), privacy: .public) to fulfill request \(id)")
    }
  }

  public func send<T>(
    writeProc write: (OutputPort) -> Void,
    readProc read: @escaping (InputPort, inout Data) -> T
  ) -> Future<String, T> {
    mu.wait()
    let id = seq
    seq += 1
    let t0 = DispatchTime.now()
    id.write(to: out)
    write(out)
    out.flush()
    let dt = DispatchTime.now().uptimeNanoseconds - t0.uptimeNanoseconds
    let fut = Future<String, T>()
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

fileprivate struct ResponseHandlerImpl<T>: ResponseHandler where T: Sendable {
  let id: UInt64
  let fut: Future<String, T>
  let read: (InputPort, inout Data) -> T
  let time = DispatchTime.now()

  func handle(from inp: InputPort, using buf: inout Data) -> UInt64 {
    let t0 = DispatchTime.now()
    if inp.readByte() == 1 {
      fut.resolve(with: read(inp, &buf))
    } else {
      fut.reject(with: String.read(from: inp, using: &buf))
    }
    return DispatchTime.now().uptimeNanoseconds - t0.uptimeNanoseconds
  }
}

fileprivate struct Duration: CustomStringConvertible {
  let nanos: UInt64

  var description: String {
    if nanos > 1_000_000_000 {
      return "\(nanos/1_000_000_000)s"
    } else if nanos > 1_000_000 {
      return "\(nanos/1_000_000)ms"
    } else if nanos > 1_000 {
      return "\(nanos/1_000)µs"
    }
    return "\(nanos)ns"
  }
}
