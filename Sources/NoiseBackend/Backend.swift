import Dispatch
import Foundation
import Noise
import NoiseSerde
import OSLog

fileprivate let logger = Logger(
  subsystem: "io.defn.NoiseBackend",
  category: "Backend"
)

/// Statistics reported by Backends.
public struct BackendStats: Sendable {
  public let totalRequests: UInt64
  public let totalWaitDuration: Duration
  public let totalReadDuration: Duration
  public let totalWriteDuration: Duration
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
  private var totalWaitDuration = Duration.zero
  private var totalReadDuration = Duration.zero
  private var totalWriteDuration = Duration.zero

  public init(withZo zo: URL, andMod modname: String, andProc proc: String) {
    out = FileHandleOutputPort(withHandle: ip.fileHandleForWriting)
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
    logger.debug("\(#function): booting Racket")
    let t0 = ContinuousClock.now
    let r = Racket(execPath: zo.path)
    let dt = ContinuousClock.now - t0
    logger.debug("\(#function): took \(dt) to boot Racket")
    r.bracket {
      logger.debug("\(#function): loading backend")
      let t0 = ContinuousClock.now
      r.load(zo: zo)
      let dt = ContinuousClock.now - t0
      logger.debug("\(#function): took \(dt) to load backend")
      let mod = Val.cons(Val.symbol("quote"), Val.cons(Val.symbol(modname), Val.null))
      let ifd = Val.fixnum(Int(ip.fileHandleForReading.fileDescriptor))
      let ofd = Val.fixnum(Int(op.fileHandleForWriting.fileDescriptor))
      let serve = r.require(Val.symbol(proc), from: mod).unsafeCar()
      serve.unsafeApply(Val.cons(ifd, Val.cons(ofd, Val.null)))
      preconditionFailure("Racket server exited")
    }
  }

  private func read() {
    let inp = FileHandleInputPort(withHandle: op.fileHandleForReading)
    var buf = Data(count: 8*1024) // will grow as needed
    while true {
      let id = UVarint.read(from: inp, using: &buf)
      //logger.debug("\(RequestId(value: id)): reading response; bufsize: \(buf.count)b")
      mu.wait()
      guard let handler = pending[id] else {
        logger.fault("\(RequestId(value: id)): future is gone")
        mu.signal()
        continue
      }
      mu.signal()
      let readDuration = handler.handle(from: inp, using: &buf)
      //logger.debug("\(RequestId(value: id)): took \(Duration(nanos: readDuration), privacy: .public) to read")
      mu.wait()
      pending.removeValue(forKey: id)
      let requestDuration = ContinuousClock.now - handler.time
      totalRequests += 1
      totalWaitDuration += requestDuration
      totalReadDuration += readDuration
      mu.signal()
      logger.debug("\(RequestId(value: id)): took \(requestDuration) to fulfill")
    }
  }

  public func send<T>(
    writeProc write: (OutputPort) -> Void,
    readProc read: @escaping (InputPort, inout Data) -> T,
    commandName: String = #function
  ) -> Future<String, T> {
    mu.wait()
    let id = seq
    seq += 1
    logger.debug("\(RequestId(value: id)): \(commandName)")
    let t0 = ContinuousClock.now
    id.write(to: out)
    write(out)
    out.flush()
    let dt = ContinuousClock.now - t0
    let fut = Future<String, T>()
    let handler = ResponseHandlerImpl<T>(id: id, fut: fut, read: read)
    pending[id] = handler
    totalWriteDuration += dt
    mu.signal()
    return fut
  }

  public func stats() -> BackendStats {
    mu.wait()
    defer { mu.signal() }
    return BackendStats(
      totalRequests: totalRequests,
      totalWaitDuration: totalWaitDuration,
      totalReadDuration: totalReadDuration,
      totalWriteDuration: totalWriteDuration
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
  var time: ContinuousClock.Instant { get }

  func handle(from inp: InputPort, using buf: inout Data) -> Duration
}

fileprivate struct ResponseHandlerImpl<T>: ResponseHandler where T: Sendable {
  let id: UInt64
  let fut: Future<String, T>
  let read: (InputPort, inout Data) -> T
  let time = ContinuousClock.now

  func handle(from inp: InputPort, using buf: inout Data) -> Duration {
    let t0 = ContinuousClock.now
    if inp.readByte() == 1 {
      fut.resolve(with: read(inp, &buf))
    } else {
      fut.reject(with: String.read(from: inp, using: &buf))
    }
    return ContinuousClock.now - t0
  }
}

fileprivate struct RequestId: CustomStringConvertible {
  let value: UInt64

  var description: String {
    return String(format: "#%06d", value)
  }
}
