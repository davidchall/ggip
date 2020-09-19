#ifndef __GGIP_CURVES__
#define __GGIP_CURVES__

#include <stdint.h>


void hilbert_curve(uint32_t s, int order, uint32_t *x, uint32_t *y);

void morton_curve(uint32_t s, int order, uint32_t *x, uint32_t *y);

#endif
