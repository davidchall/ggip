#ifndef __GGIP_CURVES__
#define __GGIP_CURVES__

inline void encode_hilbert(uint32_t s, int order, uint32_t *x, uint32_t *y) {
  unsigned int state, row;
  state = *x = *y = 0;

  for (int i=2*order-2; i>=0; i-=2) {
    row = 4 * state | ((s >> i) & 3);

    *x = (*x << 1) | ((0x936C >> row) & 1);
    *y = (*y << 1) | ((0x39C6 >> row) & 1);

    state = (0x3E6B94C1 >> 2 * row) & 3;
  }
}

#endif
