#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <errno.h>

#include "scheme.h"

static void die(char const *format, ...) {
  va_list vl;
  va_start(vl, format);
  vfprintf(stderr, format, vl);
  va_end(vl);
  exit(1);
}

int main(int argc, char *argv[]) {
  scheme *sc;
  FILE *f;

  sc = scheme_init_new();
  if (!sc) die("Could not initialise scheme\n");

  f = fopen("codegen.scm", "rt");
  if (!f) die("Could not open codegen.scm: %s\n", strerror(errno));

  scheme_set_input_port_file(sc, stdin);
  scheme_set_output_port_file(sc, stdout);
  scheme_load_file(sc, f);
  fclose(f);
  scheme_load_file(sc, stdin);
  scheme_deinit(sc);
  free(sc);

  return 0;
}
