#include <sys/ioctl.h>
#include <caml/alloc.h>
#include <caml/memory.h>

CAMLprim value theaterminal_terminal_size(value fd)
{
  CAMLparam1(fd);
  CAMLlocal1(size);
  struct winsize dimensions;
  int rows = 24, columns = 80;
  if (ioctl(Int_val(fd), TIOCGWINSZ, &dimensions) == 0 &&
      dimensions.ws_row > 0 && dimensions.ws_col > 0) {
    rows = dimensions.ws_row;
    columns = dimensions.ws_col;
  }
  size = caml_alloc_tuple(2);
  Store_field(size, 0, Val_int(rows));
  Store_field(size, 1, Val_int(columns));
  CAMLreturn(size);
}
