#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <errno.h>
#include <dlfcn.h>
#include <stdint.h>

#include "scheme-private.h"
#include "scheme.h"

#include "libdis.h"

#ifndef RTLD_DEFAULT
#define RTLD_DEFAULT NULL
#endif

static void die(char const *format, ...) {
  va_list vl;
  va_start(vl, format);
  vfprintf(stderr, format, vl);
  va_end(vl);
  exit(1);
}

static void apply_relocations(char *codevec, int len, pointer relocations) {
  printf("New function is at 0x%08x = %d\n", (unsigned) codevec, (unsigned) codevec);
  while (is_pair(relocations)) {
    pointer relocation = pair_car(relocations);
    int code_offset = ivalue(pair_cdr(relocation));
    long target = ivalue(pair_car(relocation));
    uint32_t v = target - (uint32_t) codevec - (code_offset + 4);

    relocations = pair_cdr(relocations);

    printf("Patching offset %d to target 0x%08x; relative offset is 0x%08x\n",
	   code_offset, (unsigned) target, v);
    memcpy(&codevec[code_offset], &v, sizeof(uint32_t));
  }
}


static pointer scm_build_native(scheme *sc, pointer args) {
  if (!is_pair(args)) {
    return sc->F;
  } else {
    pointer s = pair_car(args);
    char *bytes;
    int len;
    char *newbytes;

    if (!is_string(s)) {
      return sc->F;
    }

    len = s->_object._string._length;
    bytes = s->_object._string._svalue;
    newbytes = malloc(len);
    memcpy(newbytes, bytes, len);
    if (is_pair(pair_cdr(args))) {
      apply_relocations(newbytes, len, pair_car(pair_cdr(args)));
    }

    return mk_foreign_func(sc, (foreign_func) newbytes);
  }
}

static void disassemble(void *bytes, int len) {
  char line[128];
  int pos = 0;
  x86_insn_t i;

  x86_init(opt_none, NULL, NULL);

  while (pos < len) {
    int i_size = x86_disasm((unsigned char *) bytes, len, 0, pos, &i);
    if (i_size) {
      x86_format_insn(&i, line, sizeof(line), native_syntax);
      printf("%s\n", line);
      pos += i_size;
    } else {
      printf("Invalid instruction\n");
      pos++;
    }
  }

  x86_cleanup();
}

static pointer scm_disassemble(scheme *sc, pointer args) {
  pointer s;

  if (!is_pair(args)) return sc->F;
  s = pair_car(args);

  if (is_string(s)) {
    int len = s->_object._string._length;
    char *bytes = s->_object._string._svalue;
    disassemble(bytes, len);
  } else if (is_number(s)) {
    char *bytes = (char *) ivalue(s);
    int len = (int) ivalue(pair_car(pair_cdr(args)));
    disassemble(bytes, len);
  } else {
    return sc->F;
  }

  return sc->T;
}

static pointer scm_lookup_native(scheme *sc, pointer args) {
  pointer s;
  char *bytes;
  int len;

  if (!is_pair(args)) return sc->F;
  s = pair_car(args);
  if (!is_string(s)) return sc->F;

  len = s->_object._string._length;
  bytes = s->_object._string._svalue;

  {
    char sym[128];
    void *p;

    if (len >= sizeof(sym)) len = sizeof(sym) - 1;
    memcpy(sym, bytes, len);
    sym[len] = '\0';
    p = dlsym(RTLD_DEFAULT, sym);

    if (p != NULL) {
      return mk_integer(sc, (long) p);
    } else {
      return sc->F;
    }
  }
}

static pointer scm_string_address(scheme *sc, pointer args) {
  pointer s;
  if (!is_pair(args)) return sc->F;
  s = pair_car(args);
  if (!is_string(s)) return sc->F;
  return mk_integer(sc, (long) s->_object._string._svalue);
}

static pointer scm_shr(scheme *sc, pointer args) {
  pointer n, m;
  if (!is_pair(args)) return sc->F;
  n = pair_car(args);
  args = pair_cdr(args);
  if (!is_number(n)) return sc->F;
  if (!is_pair(args)) return sc->F;
  m = pair_car(args);
  if (!is_number(m)) return sc->F;
  return mk_integer(sc, ivalue(n) >> ivalue(m));
}

int main(int argc, char *argv[]) {
  scheme *sc;
  FILE *f;
  scheme_registerable fns[] = {
    { &scm_build_native, "build-native-function" },
    { &scm_disassemble, "disassemble" },
    { &scm_lookup_native, "lookup-native-symbol" },
    { &scm_string_address, "string-address" },
    { &scm_shr, "shr" },
  };

  sc = scheme_init_new();
  if (!sc) die("Could not initialise scheme\n");

  f = fopen("codegen.scm", "rt");
  if (!f) die("Could not open codegen.scm: %s\n", strerror(errno));

  scheme_set_input_port_file(sc, stdin);
  scheme_set_output_port_file(sc, stdout);
  scheme_register_foreign_func_list(sc, &fns[0], sizeof(fns) / sizeof(fns[0]));
  scheme_load_file(sc, f);
  fclose(f);
  scheme_load_file(sc, stdin);
  scheme_deinit(sc);
  free(sc);

  return 0;
}
