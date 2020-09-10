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
  unsigned int y_range = (1 << order) - 1;
  *y = y_range - *y;
}

void morton_curve(uint32_t s, int order, uint32_t *x, uint32_t *y) {
  *x = *y = 0;

  for (int i=2*order-2; i>=0; i-=2) {
    *x = (*x << 1) | ((s >> i) & 1);
    *y = (*y << 1) | ((s >> (i + 1)) & 1);
  }

  // invert y-axis
  unsigned int y_range = (1 << order) - 1;
  *y = y_range - *y;
}
