
#include <stdlib.h>
#include "runtime.h"

unsigned char tl1_mem[256*256];

void p__main(void);

void tl1_stop(void) {
  exit(1);
}

int main(void) {
  tl1_io_init();
  p__main();
  tl1_io_finalize();
  return 0;
}
