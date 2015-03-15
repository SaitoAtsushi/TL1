
#include "runtime.h"

_Bool carry_flag;
_Bool borrow_flag;
unsigned char mod;
unsigned char high;

                        
unsigned char tl1_adc(unsigned char x, unsigned char y) {
  signed int temp = x+y+carry_flag;
  carry_flag = (_Bool)(temp/256);
  return temp%256;
}

unsigned char tl1_sbc(unsigned char x, unsigned char y) {
  if(x-1>y) return x-y-1;
  else {
    borrow_flag = 1;
    return x+256-y-1;
  }
}

unsigned char tl1_plus(unsigned char x, unsigned char y) {
  unsigned int temp = x+y;
  carry_flag = (_Bool)(temp/256);

  return temp%256;
}

unsigned char tl1_minus(unsigned char x, unsigned char y) {
  if(x>y) return x-y;
  else {
    borrow_flag = 1;
    return x+256-y;
  }
}

unsigned char tl1_mul(unsigned char x, unsigned char y) {
  unsigned int temp = x*y;
  high = temp/256;
  return temp%256;
}

unsigned char tl1_div(unsigned char x, unsigned char y) {
  mod = x%y;
  return x/y;
}
