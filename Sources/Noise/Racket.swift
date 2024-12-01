import Foundation
import RacketCS

#if arch(x86_64)
let ARCH = "x86_64"
#elseif arch(arm64)
let ARCH = "arm64"
#else
#error("unsupported platform")
#endif

#if os(macOS)
import NoiseBoot_macOS

let OS = "macos"
#elseif os(iOS)
import NoiseBoot_iOS

let OS = "ios"
#else
#error("unsupported OS")
#endif

/// The Racket runtime.
///
/// # Threading
///
/// The thread on which the runtime is instantiated is considered the
/// main Racket place.  All Racket operations must be run on that
/// thread.  You may work with Chez Scheme values and call Chez Scheme
/// primitives from other threads (see `bracket` and `activate`).
///
/// - Warning: Only one instance may be created per process.
public struct Racket {
  public init(execPath: String = "racket") {
    var args = racket_boot_arguments_t()
    args.exec_file = execPath.utf8CString.cstring()
    args.boot1_path = NoiseBoot.petiteURL.path.utf8CString.cstring()
    args.boot2_path = NoiseBoot.schemeURL.path.utf8CString.cstring()
    args.boot3_path = NoiseBoot.racketURL.path.utf8CString.cstring()
    racket_boot(&args)
    racket_deactivate_thread()
    args.exec_file.deallocate()
    args.boot1_path.deallocate()
    args.boot2_path.deallocate()
    args.boot3_path.deallocate()
  }

  /// Loads compiled Racket code.
  ///
  /// - Warning: This function must be called from the same thread the
  /// Racket runtime was initialized on.
  public func load(zo: URL) {
    let path = zo.path.utf8CString.cstring()
    racket_embedded_load_file(path, 1)
    path.deallocate()
  }

  /// Requires `what` from the module at path `mod`.
  ///
  /// - Warning: This function must be called from the same thread the
  /// Racket runtime was initialized on.
  public func require(_ what: Val, from mod: Val) -> Val {
    return Val(ptr: racket_dynamic_require(mod.ptr, what.ptr))
  }

  /// Calls `proc` after activating the current thread.  Deactivates
  /// the thread before returning `proc`'s result.
  public func bracket<T>(proc: () -> T) -> T {
    racket_activate_thread()
    let res = proc()
    racket_deactivate_thread()
    return res
  }

  /// Makes the current thread known to Chez Scheme.
  ///
  /// - Warning: Accessing Chez Scheme data from inactive threads
  /// races against the garbage collector.
  /// - Warning: Active threads that aren't running Scheme code block
  /// the garbage collector, so deactivate unused threads with
  /// `deactivate`.
  public func activate() {
    racket_activate_thread()
  }

  public func deactivate() {
    racket_deactivate_thread()
  }

  /// Tears down the Chez Scheme runtime.
  public func destroy() {
    racket_destroy()
  }
}

/// An unsafe wrapper for Chez Scheme values.
///
/// - Note: On macOS x86_64 and aarch64, `ptr` values are real OS
/// pointers represented as `void *`s. On iOS, we use the portable
/// bytecode interpreter, so pointers are represented as `unsigned
/// long long`s. For now, we handle the difference here using platform
/// conditionals. If ever we're able to use native code on iOS, that'll
/// have to change at least.
///
/// - Warning: Values may be moved by the GC at any time, so these
/// helpers should mainly be used to create data to be passed into
/// Racket, and to copy data from Racket within activated threads.
public struct Val {
  #if os(iOS)
  let ptr: ptr
  #else
  let ptr: ptr?
  #endif

  /// The empty list.
  nonisolated(unsafe) public static let null = Val(ptr: racket_nil())

  /// The true value.
  nonisolated(unsafe) public static let f = Val(ptr: racket_false())

  /// The false value.
  nonisolated(unsafe) public static let t = Val(ptr: racket_true())

  /// Creates a Chez Scheme fixnum.
  public static func fixnum(_ i: Int) -> Val {
    #if os(iOS)
    return Val(ptr: racket_fixnum(Int64(i)))
    #else
    return Val(ptr: racket_fixnum(i))
    #endif
  }

  /// Creates a Chez Scheme integer representing a pointer address.
  public static func pointer(_ p: UnsafeMutableRawPointer) -> Val {
    #if os(iOS)
    return Val(ptr: racket_pointer(UInt64(UInt(bitPattern: p))))
    #else
    return Val(ptr: racket_pointer(p))
    #endif
  }

  /// Creates a Chez Scheme symbol by copying a String.
  public static func symbol(_ s: String) -> Val {
    let c = s.utf8CString.cstring()
    let p = racket_symbol(c)
    c.deallocate()
    return Val(ptr: p)
  }

  /// Creates a Chez Scheme string by copying a regular String.
  public static func string(_ s: String) -> Val {
    let utf8 = s.utf8CString
    let c = utf8.cstring()
    #if os(iOS)
    let p = racket_string(c, UInt64(utf8.underestimatedCount - 1))
    #else
    let p = racket_string(c, UInt(utf8.underestimatedCount - 1))
    #endif
    c.deallocate()
    return Val(ptr: p)
  }

  /// Creates a pair of two Values.
  public static func cons(_ a: Val, _ b: Val) -> Val {
    return Val(ptr: racket_cons(a.ptr, b.ptr))
  }

  /// Locks the value to prevent the GC from moving it.  Panics if
  /// called with an immediate value.
  public func lock() {
    #if os(iOS)
    racket_lock_object(ptr)
    #else
    racket_lock_object(ptr!)
    #endif
  }

  /// Unlocks the value to let the GC move it.  Panics if called with
  /// an immediate value.
  public func unlock() {
    #if os(iOS)
    racket_unlock_object(ptr)
    #else
    racket_unlock_object(ptr!)
    #endif
  }

  /// Like `lock`, but returns the value.
  public func locked() -> Val {
    lock()
    return self
  }

  /// Like `unlock`, but returns the value.
  public func unlocked() -> Val {
    unlock()
    return self
  }

  /// Applies the value using `args`.
  @discardableResult
  public func apply(_ args: Val) -> Val? {
    if racket_procedurep(ptr) == 1{
      return unsafeApply(args)
    }
    return nil
  }

  /// Applies the value using `args`, panicking if the value is not a
  /// procedure.
  @discardableResult
  public func unsafeApply(_ args: Val) -> Val {
    return Val(ptr: racket_apply(ptr, args.ptr))
  }

  // Returns the `car` of a pair.
  public func car() -> Val? {
    if racket_pairp(ptr) == 1 {
      return unsafeCar()
    }
    return nil
  }

  // Returns the `cdr` of a pair.
  public func cdr() -> Val? {
    if racket_pairp(ptr) == 1 {
      return unsafeCdr()
    }
    return nil
  }

  /// Returns the `car` of a pair, panicking if the value is not a
  /// pair.
  public func unsafeCar() -> Val {
    #if os(iOS)
    return Val(ptr: racket_car(ptr))
    #else
    return Val(ptr: racket_car(ptr!))
    #endif
  }

  /// Returns the `cdr` of a pair, panicking if the value is not a
  /// pair.
  public func unsafeCdr() -> Val {
    #if os(iOS)
    return Val(ptr: racket_cdr(ptr))
    #else
    return Val(ptr: racket_cdr(ptr!))
    #endif
  }

  /// Extracts the integer value of a fixnum.
  public func fixnum() -> Int? {
    if racket_fixnump(ptr) == 1 {
      return unsafeFixnum()
    }
    return nil
  }

  /// Extracts the integer value of a fixnum, panicking if the value
  /// is not a fixnum.
  public func unsafeFixnum() -> Int {
    #if os(iOS)
    return Int(racket_fixnum_value(ptr))
    #else
    return racket_fixnum_value(ptr)
    #endif
  }

  /// Copies a Chez Scheme bytevector value into a String.
  public func bytestring() -> String? {
    if racket_bytevectorp(ptr) == 1 {
      return unsafeBytestring()
    }
    return nil
  }

  /// Copies a Chez Scheme bytevector value into a String, panicking
  /// if the value is not a bytevector.
  public func unsafeBytestring() -> String? {
    return unsafeBytevector(nulTerminated: true).withUnsafeBufferPointer { buf -> String? in
      return String(validatingCString: buf.baseAddress!)
    }
  }

  /// Copies a Chez Scheme bytevector into an `Array` of `CChar`s.
  ///
  /// - Warning: Chez Scheme `byte`s are unsigned, but they get
  /// converted to signed in Swift.
  public func bytevector(nulTerminated nul: Bool = false) -> [CChar]? {
    if racket_bytevectorp(ptr) == 1 {
      return unsafeBytevector(nulTerminated: nul)
    }
    return nil
  }

  /// Copies a Chez Scheme bytevector into a Swift array, panicking if
  /// the value is not a bytevector.
  ///
  /// When the `nulTerminated` argument is `true`, the bytevector will
  /// contain an extra 0 byte at the end.
  public func unsafeBytevector(nulTerminated: Bool = false) -> [CChar] {
    #if os(iOS)
    let len = Int(racket_bytevector_length(ptr))
    let data = racket_bytevector_data(ptr)
    #else
    let len = Int(racket_bytevector_length(ptr!))
    let data = racket_bytevector_data(ptr!)
    #endif
    var res = Array<CChar>(repeating: 0, count: nulTerminated ? len+1 : len)
    res.withUnsafeMutableBytes { dst in
      dst.copyBytes(from: UnsafeRawBufferPointer(start: data, count: Int(len)))
    }
    return res
  }
}

fileprivate extension ContiguousArray where Element == CChar {
  func cstring() -> UnsafePointer<CChar> {
    let p = UnsafeMutablePointer<CChar>.allocate(capacity: self.underestimatedCount)
    self.withUnsafeBufferPointer { ptr in
      p.initialize(from: ptr.baseAddress!, count: self.underestimatedCount)
    }
    return UnsafePointer(p)
  }
}
