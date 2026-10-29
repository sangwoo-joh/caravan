#define _XOPEN_SOURCE 600
#include <errno.h>
#include <fcntl.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/unixsupport.h>

CAMLprim value theaterminal_test_open_pty(value unit)
{
  CAMLparam1(unit);
  CAMLlocal1(pair);
  int master = posix_openpt(O_RDWR | O_NOCTTY);
  if (master == -1) uerror("posix_openpt", Nothing);
  if (grantpt(master) == -1 || unlockpt(master) == -1) {
    int error = errno;
    close(master);
    errno = error;
    uerror("grantpt/unlockpt", Nothing);
  }
  char *name = ptsname(master);
  int slave = name == NULL ? -1 : open(name, O_RDWR | O_NOCTTY);
  if (slave == -1) {
    int error = errno;
    close(master);
    errno = error;
    uerror("open pty slave", Nothing);
  }
  pair = caml_alloc_tuple(2);
  Store_field(pair, 0, Val_int(master));
  Store_field(pair, 1, Val_int(slave));
  CAMLreturn(pair);
}

CAMLprim value theaterminal_test_set_size(value fd, value rows, value columns)
{
  struct winsize dimensions = {0};
  dimensions.ws_row = Int_val(rows);
  dimensions.ws_col = Int_val(columns);
  if (ioctl(Int_val(fd), TIOCSWINSZ, &dimensions) == -1)
    uerror("TIOCSWINSZ", Nothing);
  return Val_unit;
}
