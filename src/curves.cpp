#include "curves.h"


void hilbert_curve(uint32_t s, int order, uint32_t *x, uint32_t *y) {
  unsigned int state, row;
  state = *x = *y = 0;

  for (int i=2*order-2; i>=0; i-=2) {
    row = 4 * state | ((s >> i) & 3);

    *x = (*x << 1) | ((0x936C >> row) & 1);
    *y = (*y << 1) | ((0x39C6 >> row) & 1);

    state = (0x3E6B94C1 >> 2 * row) & 3;
  }

  // invert y-axis
  unsigned int y_max = (1 << order) - 1;
  *y = y_max - *y;
}


// https://fgiesen.wordpress.com/2009/12/13/decoding-morton-codes/
uint32_t morton_extract(uint32_t x) {
  x &= 0x55555555;
  x = (x ^ (x >>  1)) & 0x33333333;
  x = (x ^ (x >>  2)) & 0x0f0f0f0f;
  x = (x ^ (x >>  4)) & 0x00ff00ff;
  x = (x ^ (x >>  8)) & 0x0000ffff;
  return x;
}

void morton_curve(uint32_t s, int order, uint32_t *x, uint32_t *y) {
  *x = morton_extract(s);
  *y = morton_extract(s >> 1);

  // invert y-axis
  unsigned int y_max = (1 << order) - 1;
  *y = y_max - *y;
}
