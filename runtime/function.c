
#include <stdlib.h>
#include "runtime.h"

unsigned char f_mod_sys(void) {
  return mod;
}

unsigned char f_mhigh_sys(void) {
  return high;
}

unsigned char f_rnd_sys(unsigned char x) {
  return rand()%x+1;
}

unsigned char f_not_sys(unsigned char x) {
  return ~x;
}

unsigned char f_neg_sys(unsigned char x) {
  return -(signed char)x;
}

unsigned char f_com_sys(unsigned char x) {
  return ~x;
}

unsigned char f_lsr_sys(unsigned char x) {
  carry_flag = x%2;
  unsigned char msb = 128&x;
  return (x>>1)|msb;
}

unsigned char f_asr_sys(unsigned char x) {
  carry_flag = x%2;
  return x>>1;
}

unsigned char f_asl_sys(unsigned char x) {
  carry_flag = 128&x;
  return x<<1;
}

unsigned char f_ror_sys(unsigned char x) {
  _Bool temp = carry_flag;
  x >>= 1;
  x |= carry_flag?128:0;
  carry_flag = temp;
  return x;
}

unsigned char f_rol_sys(unsigned char x) {
  _Bool temp = 128&x;
  x <<= 1;
  x |= carry_flag;
  carry_flag = temp;
  return x;
}
