import Dispatch
import Foundation

nonisolated(unsafe) private var defaultErrorHandlerMu = DispatchSemaphore(value: 1)
nonisolated(unsafe) private var defaultErrorHandler: (Any) -> Void = { err in
  preconditionFailure("unexpected error: \(err)")
}

/// A container for data that will be received from a Backend at some
/// point.
public final class Future<Err, Res>: @unchecked Sendable where Err: Sendable, Res: Sendable {
  private let mu = DispatchSemaphore(value: 1)
  private var waiters = [DispatchSemaphore]()

  private enum State {
    case pending
    case canceled
    case error(Err)
    case ok(Res)
  }

  private var state = State.pending

  /// Thrown by `Future.wait` when a Future is canceled.
  public struct Canceled: Error {}

  /// Thrown by `Future.wait` on error.
  public struct WaitError<E>: LocalizedError where E: Sendable {
    let error: E

    public var errorDescription: String? {
      if let err = error as? String {
        return err
      }
      return "\(error)"
    }
  }

  /// Represents the disjoint result values that may be returned by
  /// calls to `Future.wait(timeout:)`.
  public enum WaitResult<E, R>: Sendable where E: Sendable, R: Sendable {
    case timedOut
    case canceled
    case error(E)
    case ok(R)
  }

  public init() {}

  /// Resolve the future with `d` and signal all the waiters (if any).
  public func resolve(with res: Res) {
    transition(toState: .ok(res))
  }

  /// Fail the future with `e` and signal all the waiters (if any).
  public func reject(with err: Err) {
    transition(toState: .error(err))
  }

  /// Cancel the future, preventing any completion callbacks from
  /// running if they haven't already.  Does not affect the underlying
  /// work.
  public func cancel() {
    transition(toState: .canceled)
  }

  private func transition(toState: State) {
    mu.wait()
    switch state {
    case .pending:
      state = toState
      touch()
    default:
      ()
    }
    mu.signal()
  }

  private func touch() {
    waiters.forEach { $0.signal() }
    waiters.removeAll()
  }

  /// Returns a new Future containing the result of applying `proc` to
  /// the data contained in the Future (once available).
  public func map<R>(_ proc: @escaping @Sendable (Res) -> R) -> Future<Err, R> {
    let f = Future<Err, R>()
    DispatchQueue.global(qos: .default).async {
      switch self.wait(timeout: .distantFuture) {
      case .timedOut:
        preconditionFailure("unreachable")
      case .canceled:
        f.cancel()
      case .error(let error):
        f.reject(with: error)
      case .ok(let data):
        f.resolve(with: proc(data))
      }
    }
    return f
  }

  /// Returns a new Future containing the result of applying `proc` to
  /// the error contained in the Future (once available and if any).
  public func mapError<E>(_ proc: @escaping @Sendable (Err) -> E) -> Future<E, Res> {
    let f = Future<E, Res>()
    DispatchQueue.global(qos: .default).async {
      switch self.wait(timeout: .distantFuture) {
      case .timedOut:
        preconditionFailure("unreachable")
      case .canceled:
        f.cancel()
      case .error(let error):
        f.reject(with: proc(error))
      case .ok(let data):
        f.resolve(with: data)
      }
    }
    return f
  }

  /// Chains two Futures together.
  public func andThen<R>(_ proc: @escaping @Sendable (Res) -> Future<Err, R>) -> Future<Err, R> {
    let f = Future<Err, R>()
    DispatchQueue.global(qos: .default).async {
      switch self.wait(timeout: .distantFuture) {
      case .timedOut:
        preconditionFailure("unreachable")
      case .canceled:
        f.cancel()
      case .error(let err):
        f.reject(with: err)
      case .ok(let data):
        switch proc(data).wait(timeout: .distantFuture) {
        case .timedOut:
          preconditionFailure("unreachable")
        case .canceled:
          f.cancel()
        case .error(let err):
          f.reject(with: err)
        case .ok(let res):
          f.resolve(with: res)
        }
      }
    }
    return f
  }

  /// Executes `errorProc` or `completeProc` on `queue` depending on
  /// whether the Future succeeds or fails.  The default error proc
  /// has no effect.
  public func sink(
    queue: DispatchQueue = DispatchQueue.main,
    onCancel cancelProc: @escaping @Sendable () -> Void = { },
    onError errorProc: @escaping @Sendable (Err) -> Void = { _ in },
    onComplete completeProc: @escaping @Sendable (Res) -> Void
  ) {
    DispatchQueue.global(qos: .default).async {
      let res = self.wait(timeout: .distantFuture)
      queue.async {
        switch res {
        case .timedOut:
          preconditionFailure("unreachable")
        case .canceled:
          cancelProc()
        case .error(let err):
          errorProc(err)
        case .ok(let res):
          completeProc(res)
        }
      }
    }
  }

  /// Executes `proc` on `queue` with the Future's data if and once
  /// available.
  public func onComplete(
    queue: DispatchQueue = DispatchQueue.main,
    _ proc: @escaping @Sendable (Res) -> Void
  ) {
    sink(
      queue: queue,
      onError: { error in
        defaultErrorHandlerMu.wait()
        defer { defaultErrorHandlerMu.signal() }
        defaultErrorHandler(error)
      },
      onComplete: proc
    )
  }

  /// Block the current thread until data is available.
  public func wait() throws -> Res {
    mu.wait()
    switch state {
    case .pending:
      let waiter = DispatchSemaphore(value: 0)
      waiters.append(waiter)
      mu.signal()
      waiter.wait()
      switch state {
      case .pending:
        preconditionFailure("impossible state")
      case .canceled:
        throw Canceled()
      case .ok(let res):
        return res
      case .error(let err):
        throw WaitError(error: err)
      }
    case .canceled:
      mu.signal()
      throw Canceled()
    case .ok(let res):
      mu.signal()
      return res
    case .error(let err):
      mu.signal()
      throw WaitError(error: err)
    }
  }

  /// Block the current thread until data is available or the timeout expires.
  public func wait(timeout t: DispatchTime) -> WaitResult<Err, Res> {
    mu.wait()
    switch state {
    case .pending:
      let waiter = DispatchSemaphore(value: 0)
      waiters.append(waiter)
      mu.signal()
      switch waiter.wait(timeout: t) {
      case .success:
        switch state {
        case .pending:
          preconditionFailure("impossible state")
        case .canceled:
          return .canceled
        case .error(let err):
          return .error(err)
        case .ok(let res):
          return .ok(res)
        }
      case .timedOut:
        mu.wait()
        waiters.removeAll { $0 == waiter }
        mu.signal()
        return .timedOut
      }
    case .canceled:
      mu.signal()
      return .canceled
    case .error(let err):
      mu.signal()
      return .error(err)
    case .ok(let res):
      mu.signal()
      return .ok(res)
    }
  }
}

public class FutureUtil {
  /// Represents asyncified future errors.
  public enum AsyncError: LocalizedError {
    case error(String)

    public var errorDescription: String? {
      switch self {
      case .error(let s):
        return s
      }
    }
  }

  /// Converts a future into an async task.
  public static func asyncify<Res>(_ future: Future<String, Res>) async throws -> Res {
    return try await withTaskCancellationHandler {
      return try await withUnsafeThrowingContinuation { k in
        future.sink(
          queue: .global(qos: .userInitiated),
          onCancel: { k.resume(throwing: CancellationError()) },
          onError: { k.resume(throwing: AsyncError.error($0)) },
          onComplete: { k.resume(returning: $0) })
      }
    } onCancel: {
      future.cancel()
    }
  }

  /// Returns a future that is immediately resolved.
  public static func resolved<Res>(with d: Res) -> Future<Never, Res> {
    let fut = Future<Never, Res>()
    fut.resolve(with: d)
    return fut
  }

  /// Returns a future that is immediately rejected.
  public static func rejected<Err>(with err: Err) -> Future<Err, Never> {
    let fut = Future<Err, Never>()
    fut.reject(with: err)
    return fut
  }

  /// Sets the default error handler that is used by `Future.onComplete`.
  public static func set(defaultErrorHandler hdl: @escaping (Any) -> Void) {
    defaultErrorHandlerMu.wait()
    defaultErrorHandler = hdl
    defaultErrorHandlerMu.signal()
  }
}
