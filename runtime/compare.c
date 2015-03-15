#include "runtime.h"

unsigned char tl1_gt(unsigned char x, unsigned char y) {
  return (signed char)x > (signed char)y ? 255 : 0;
}

unsigned char tl1_lt(unsigned char x, unsigned char y) {
  return (signed char)x < (signed char)y ? 255 : 0;
}

unsigned char tl1_ugt(unsigned char x, unsigned char y) {
  return x>y ? 255 : 0;
}

unsigned char tl1_ult(unsigned char x, unsigned char y) {
  return x<y ? 255 : 0;
}

unsigned char tl1_diff(unsigned char x, unsigned char y) {
  return !tl1_eq(x,y);
}

unsigned char tl1_eq(unsigned char x, unsigned char y) {
  return x==y ? 255 : 0;
}
