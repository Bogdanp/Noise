import Dispatch
import Foundation

public enum FutureWaitResult<R> {
  case success(R)
  case timedOut
}

/// A container for data that will be received from a Backend at some
/// point.
public class Future<T> {
  private let mu = DispatchSemaphore(value: 1)
  private var waiters = [DispatchSemaphore]()
  private var data: T? = nil

  func resolve(with data: T) {
    mu.wait()
    defer { mu.signal() }
    self.data = data
    for w in waiters {
      w.signal()
    }
    waiters.removeAll()
  }

  /// Return a new future containing the result of `proc` once data is
  /// available on this future.
  public func map<R>(_ proc: @escaping (T) -> R) -> Future<R> {
    let fut = Future<R>()
    DispatchQueue.global(qos: .default).async {
      fut.resolve(with: proc(self.wait()))
    }
    return fut
  }

  /// Runs `proc` on `queue` with the future's result once ready.
  public func onComplete(queue: DispatchQueue = DispatchQueue.main, _ proc: @escaping (T) -> Void) {
    DispatchQueue.global(qos: .default).async {
      let res = self.wait()
      queue.async {
        proc(res)
      }
    }
  }

  /// Block the current thread until data is available.
  public func wait() -> T {
    mu.wait()
    if let d = data {
      mu.signal()
      return d
    }
    let waiter = DispatchSemaphore(value: 0)
    waiters.append(waiter)
    mu.signal()
    waiter.wait()
    return data!
  }

  /// Block the current thread until data is available or the timeout expires.
  public func wait(timeout t: DispatchTime) -> FutureWaitResult<T> {
    mu.wait()
    if let d = data {
      mu.signal()
      return .success(d)
    }
    let waiter = DispatchSemaphore(value: 0)
    waiters.append(waiter)
    mu.signal()
    switch waiter.wait(timeout: t) {
    case .success:
      return .success(data!)
    case .timedOut:
      mu.wait()
      waiters.removeAll { $0 == waiter }
      mu.signal()
      return .timedOut
    }
  }
}
