#ifndef RACKET_H
#define RACKET_H

#if defined(__x86_64__)
# include "chezscheme-x86_64-macos.h"
#elif defined(__arm64__)
# include "TargetConditionals.h"
# if TARGET_OS_IPHONE
#  define _Nonnull
#  define _Nullable
#  include "chezscheme-arm64-ios.h"
# else
#  include "chezscheme-arm64-macos.h"
# endif
#else
# error "unsupported platform"
#endif

#include "racketcs.h"

int racket_activate_thread() {
  return Sactivate_thread();
}

void racket_deactivate_thread() {
  Sdeactivate_thread();
}

int racket_destroy_thread() {
  return Sdestroy_thread();
}

void racket_destroy() {
  return Sscheme_deinit();
}

ptr _Nonnull racket_nil() {
  return Snil;
}

ptr _Nonnull racket_false() {
  return Sfalse;
}

ptr _Nonnull racket_true() {
  return Strue;
}

int racket_fixnump(ptr _Nullable p) {
  return Sfixnump(p);
}

ptr _Nullable racket_fixnum(iptr i) {
  return Sfixnum(i);
}

iptr racket_fixnum_value(ptr _Nullable p)  {
  return Sfixnum_value(p);
}

ptr _Nullable racket_pointer(ptr _Nullable p) {
  return Sinteger((iptr)p);
}

ptr _Nonnull racket_symbol(const char * _Nonnull s) {
  return Sstring_to_symbol(s);
}

ptr _Nonnull racket_string(const char * _Nonnull s, uptr len) {
  return Sstring_utf8(s, len);
}

int racket_pairp(ptr _Nullable p) {
  return Spairp(p);
}

ptr _Nonnull racket_cons(ptr _Nullable a, ptr _Nullable b) {
  return Scons(a, b);
}

ptr _Nullable racket_car(ptr _Nonnull l) {
  return Scar(l);
}

ptr _Nullable racket_cdr(ptr _Nonnull l) {
  return Scdr(l);
}

int racket_procedurep(ptr _Nullable p) {
  return Sprocedurep(p);
}

int racket_bytevectorp(ptr _Nullable p) {
  return Sbytevectorp(p);
}

uptr racket_bytevector_length(ptr _Nonnull p) {
  return Sbytevector_length(p);
}

unsigned char * _Nonnull racket_bytevector_data(ptr _Nonnull p) {
  return (unsigned char *)Sbytevector_data(p);
}

void racket_lock_object(ptr _Nonnull o) {
  Slock_object(o);
}

void racket_unlock_object(ptr _Nonnull o) {
  Sunlock_object(o);
}

#endif
