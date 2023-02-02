#if defined(__x86_64__)
# include "chezscheme-x86_64-macos.h"
#elif defined(__arm64__)
# include "chezscheme-arm64-macos.h"
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

ptr racket_nil() {
  return Snil;
}

ptr racket_false() {
  return Sfalse;
}

ptr racket_true() {
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

ptr racket_symbol(const char *s) {
  return Sstring_to_symbol(s);
}

ptr racket_string(const char *s, uptr len) {
  return Sstring_utf8(s, len);
}

int racket_pairp(ptr p) {
  return Spairp(p);
}

ptr racket_cons(ptr a, ptr b) {
  return Scons(a, b);
}

ptr racket_car(ptr l) {
  return Scar(l);
}

ptr racket_cdr(ptr l) {
  return Scdr(l);
}

int racket_procedurep(ptr p) {
  return Sprocedurep(p);
}

int racket_bytevectorp(ptr p) {
  return Sbytevectorp(p);
}

uptr racket_bytevector_length(ptr p) {
  return Sbytevector_length(p);
}

unsigned char *racket_bytevector_data(ptr p) {
  return (unsigned char *)Sbytevector_data(p);
}

void racket_lock_object(ptr o) {
  Slock_object(o);
}

void racket_unlock_object(ptr o) {
  Sunlock_object(o);
}
