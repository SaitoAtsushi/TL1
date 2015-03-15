#include "runtime.h"

unsigned char tl1_eor(unsigned char x, unsigned char y) {
  return x ^ y;
}

unsigned char tl1_or(unsigned char x, unsigned char y) {
  return x | y;
}

unsigned char tl1_and(unsigned char x, unsigned char y) {
  return x & y;
}
