import Foundation
import RacketCS

#if arch(x86_64)
let ARCH = "x86_64-macos"
#elseif arch(arm64) && os(macOS)
let ARCH = "arm64-macos"
#elseif arch(arm64) && os(iOS)
let ARCH = "arm64-ios"
#endif

/// The Racket runtime.
public struct Racket {
  public init(execPath: String = "racket") {
    let petiteURL = Bundle.module.url(forResource: "boot/\(ARCH)/petite", withExtension: "boot")!
    let schemeURL = Bundle.module.url(forResource: "boot/\(ARCH)/scheme", withExtension: "boot")!
    let racketURL = Bundle.module.url(forResource: "boot/\(ARCH)/racket", withExtension: "boot")!

    var args = racket_boot_arguments_t()
    args.exec_file = execPath.utf8CString.cstring()
    args.boot1_path = petiteURL.path.utf8CString.cstring()
    args.boot2_path = schemeURL.path.utf8CString.cstring()
    args.boot3_path = racketURL.path.utf8CString.cstring()
    racket_boot(&args)
    racket_deactivate_thread()
    args.exec_file.deallocate()
    args.boot1_path.deallocate()
    args.boot2_path.deallocate()
    args.boot3_path.deallocate()
  }

  /// Loads compiled Racket code.
  public func load(zo: URL) {
    let path = zo.path.utf8CString.cstring()
    racket_embedded_load_file(path, 1)
    path.deallocate()
  }

  /// Requires `what` from the module at path `mod`.
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

  /// Makes the current thread known to Racket.
  ///
  /// - Warning: Accessing Racket data from inactive threads races
  /// against the garbage collector.
  /// - Warning: Active threads that aren't running Racket code block
  /// the garbage collector, so deactivate unused threads with <#method
  /// deactivate>.
  public func activate() {
    racket_activate_thread()
  }

  public func deactivate() {
    racket_deactivate_thread()
  }
}

/// An unsafe wrapper for Racket values.
///
/// - Warning: Values may be moved by the Racket GC at any time, so
/// these helpers should mainly be used to create data to be passed
/// into Racket, and to copy data from Racket within activated
/// threads.
public struct Val {
  let ptr: ptr

  public static let null = Val(ptr: racket_nil!)
  public static let f = Val(ptr: racket_false!)
  public static let t = Val(ptr: racket_true!)

  /// Creates a Racket fixnum.
  public static func fixnum(_ i: Int) -> Val {
    return Val(ptr: racket_fixnum(i));
  }

  /// Creates a Racket symbol by copying a String.
  public static func symbol(_ s: String) -> Val {
    let c = s.utf8CString.cstring()
    let p = racket_symbol(c)
    c.deallocate()
    return Val(ptr: p!)
  }

  /// Creates a Racket string by copying a regular String.
  public static func string(_ s: String) -> Val {
    let utf8 = s.utf8CString
    let c = utf8.cstring()
    let p = racket_string(c, UInt(utf8.underestimatedCount - 1))
    c.deallocate()
    return Val(ptr: p!)
  }

  /// Creates a pair of two Values.
  public static func cons(_ a: Val, _ b: Val) -> Val {
    return Val(ptr: racket_cons(a.ptr, b.ptr))
  }

  /// Locks the value to prevent the GC from moving it.
  public func lock() {
    racket_lock_object(ptr)
  }

  /// Unlocks the value to let the GC move it.
  public func unlock() {
    racket_unlock_object(ptr)
  }

  /// Applies the value using `args`.
  public func apply(_ args: Val) -> Val? {
    if racket_procedurep(ptr) == 1{
      return unsafeApply(args)
    }
    return nil
  }

  /// Applies the value using `args`, panicking if the value is not a
  /// procedure.
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
    return Val(ptr: racket_car(ptr))
  }

  /// Returns the `cdr` of a pair, panicking if the value is not a
  /// pair.
  public func unsafeCdr() -> Val {
    return Val(ptr: racket_cdr(ptr))
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
    return racket_fixnum_value(ptr)
  }

  /// Copies a Racket bytevector into a Swift array, panicking if
  /// the value is not a bytevector.
  ///
  /// When the `nullTerminated` argument is `true`, the bytevector
  /// will contain an extra 0 byte at the end.
  public func unsafeBytevector(nullTerminated: Bool = false) -> [CChar] {
    let len = Int(racket_bytevector_length(ptr))
    let data = racket_bytevector_data(ptr)!
    var res = Array<CChar>(repeating: 0, count: nullTerminated ? len+1 : len)
    res.withUnsafeMutableBytes { dst in
      dst.copyBytes(from: UnsafeRawBufferPointer(start: data, count: Int(len)))
    }
    return res
  }

  /// Copies a Racket bytevector value into a String.
  public func bytestring() -> String? {
    if racket_bytevectorp(ptr) == 1 {
      return unsafeBytestring()
    }
    return nil
  }

  /// Copies a Racket bytevector value into a String, panicking if the
  /// value is not a bytevector.
  public func unsafeBytestring() -> String? {
    return unsafeBytevector(nullTerminated: true).withUnsafeBufferPointer { buf -> String? in
      return String(validatingUTF8: buf.baseAddress!)
    }
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
