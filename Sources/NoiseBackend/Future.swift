import Dispatch
import Foundation

public struct FutureWaitError<Err>: Error {
  let error: Err
}

public enum FutureWaitResult<Err, Res> {
  case ok(Res)
  case error(Err)
  case timedOut
}

/// A container for data that will be received from a Backend at some
/// point.
public class Future<Err, Res> {
  private let mu = DispatchSemaphore(value: 1)
  private var waiters = [DispatchSemaphore]()
  private var data: Res? = nil
  private var error: Err? = nil

  public init() {}

  /// Resolve the future with `d` and signal all the waiters (if any).
  public func resolve(with d: Res) {
    mu.wait()
    data = d
    touch()
    mu.signal()
  }

  /// Fail the future with `e` and signal all the waiters (if any).
  public func reject(with e: Err) {
    mu.wait()
    error = e
    touch()
    mu.signal()
  }

  private func touch() {
    waiters.forEach { $0.signal() }
    waiters.removeAll()
  }

  /// Returns a new Future containing the result of applying `proc` to
  /// the data contained in the Future (once available).
  public func map<R>(_ proc: @escaping (Res) -> R) -> Future<Err, R> {
    let f = Future<Err, R>()
    DispatchQueue.global(qos: .default).async {
      switch self.wait(timeout: .distantFuture) {
      case .ok(let data):
        f.resolve(with: proc(data))
      case .error(let error):
        f.reject(with: error)
      case .timedOut:
        preconditionFailure("unreachable")
      }
    }
    return f
  }

  /// Returns a new Future containing the result of applying `proc` to
  /// the error contained in the Future (once available and if any).
  public func mapError<E>(_ proc: @escaping (Err) -> E) -> Future<E, Res> {
    let f = Future<E, Res>()
    DispatchQueue.global(qos: .default).async {
      switch self.wait(timeout: .distantFuture) {
      case .ok(let data):
        f.resolve(with: data)
      case .error(let error):
        f.reject(with: proc(error))
      case .timedOut:
        preconditionFailure("unreachable")
      }
    }
    return f
  }

  /// Chains two Futures together.
  public func andThen<R>(_ proc: @escaping (Res) -> Future<Err, R>) -> Future<Err, R> {
    let f = Future<Err, R>()
    DispatchQueue.global(qos: .default).async {
      switch self.wait(timeout: .distantFuture) {
      case .ok(let data):
        switch proc(data).wait(timeout: .distantFuture) {
        case .ok(let res):
          f.resolve(with: res)
        case .error(let err):
          f.reject(with: err)
        case .timedOut:
          preconditionFailure("unreachable")
        }
      case .error(let err):
        f.reject(with: err)
      case .timedOut:
        preconditionFailure("unreachable")
      }
    }
    return f
  }

  /// Runs `proc` on `queue` with the future's result once ready.
  public func onComplete(queue: DispatchQueue = DispatchQueue.main, _ proc: @escaping (Res) -> Void) {
    DispatchQueue.global(qos: .default).async {
      let res = self.wait(timeout: .distantFuture)
      queue.async {
        switch res {
        case .ok(let res):
          proc(res)
        case .error(let err):
          preconditionFailure("unexpected error: \(err)")
        case .timedOut:
          preconditionFailure("unreachable")
        }
      }
    }
  }

  /// Block the current thread until data is available.
  public func wait() throws -> Res {
    mu.wait()
    if let d = data {
      mu.signal()
      return d
    } else if let e = error {
      mu.signal()
      throw FutureWaitError(error: e)
    }
    let waiter = DispatchSemaphore(value: 0)
    waiters.append(waiter)
    mu.signal()
    waiter.wait()
    if let d = data {
      return d
    }
    throw FutureWaitError(error: error!)
  }

  /// Block the current thread until data is available or the timeout expires.
  public func wait(timeout t: DispatchTime) -> FutureWaitResult<Err, Res> {
    mu.wait()
    if let d = data {
      mu.signal()
      return .ok(d)
    } else if let e = error {
      mu.signal()
      return .error(e)
    }
    let waiter = DispatchSemaphore(value: 0)
    waiters.append(waiter)
    mu.signal()
    switch waiter.wait(timeout: t) {
    case .success:
      if let d = data {
        return .ok(d)
      }
      return .error(error!)
    case .timedOut:
      mu.wait()
      waiters.removeAll { $0 == waiter }
      mu.signal()
      return .timedOut
    }
  }
}
