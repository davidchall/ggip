#ifndef __GGIP_TRANSFORM_IP__
#define __GGIP_TRANSFORM_IP__

#include <algorithm>
#include <ipaddress/bitwise.h>
#include <ipaddress/masking.h>


namespace ipaddress {

struct BoundingBox {
  uint32_t xmin, xmax, ymin, ymax;
};

template<class Address>
uint32_t address_to_pixel_int(const Address &address, unsigned int canvas_prefix, unsigned int bits_pixel2host) {
  Address canvas_hostmask = prefix_to_hostmask<Address>(canvas_prefix);

  Address pixel_address = bitwise_shift_right(bitwise_and(address, canvas_hostmask), bits_pixel2host);
  typename Address::bytes_type pixel_bytes = pixel_address.to_bytes();

  // interpret final 4 bytes as integer
  uint32_t pixel_int;
  std::memcpy(&pixel_int, pixel_bytes.end() - 4, 4);
  pixel_int = ntohl(pixel_int);

  return pixel_int;
}

template<class Transformer>
BoundingBox traverse_bbox_diagonal(uint32_t first_pixel_int, Transformer &&transform, int bits_canvas2network, int bits_network2pixel) {
  BoundingBox bbox;
  uint32_t diag = 0xAAAAAAAA;
  uint32_t x1, x2, y1, y2;
  int curve_order = (bits_canvas2network + bits_network2pixel) / 2;

  if (bits_network2pixel == 0) {  // no area
    transform(first_pixel_int, curve_order, &x1, &y1);

    bbox.xmin = x1;
    bbox.ymin = y1;
    bbox.xmax = x1;
    bbox.ymax = y1;
  } else if ((bits_network2pixel & 1) == 0) {  // square
    transform(first_pixel_int, curve_order, &x1, &y1);
    transform(first_pixel_int | (diag >> bits_canvas2network), curve_order, &x2, &y2);

    bbox.xmin = std::min(x1, x2);
    bbox.ymin = std::min(y1, y2);
    bbox.xmax = std::max(x1, x2);
    bbox.ymax = std::max(y1, y2);
  } else {  // rectangle
    BoundingBox square1 = traverse_bbox_diagonal(
      first_pixel_int,
      transform,
      bits_canvas2network + 1,
      bits_network2pixel - 1
    );
    BoundingBox square2 = traverse_bbox_diagonal(
      first_pixel_int + (1 << (bits_network2pixel - 1)),
      transform,
      bits_canvas2network + 1,
      bits_network2pixel - 1
    );

    bbox.xmin = std::min(square1.xmin, square2.xmin);
    bbox.ymin = std::min(square1.ymin, square2.ymin);
    bbox.xmax = std::max(square1.xmax, square2.xmax);
    bbox.ymax = std::max(square1.ymax, square2.ymax);
  }

  return bbox;
}

template<class Address, class Transformer>
void address_to_xy(
    const Address &address, Transformer &&transform,
    int canvas_prefix, int bits_canvas2pixel, int bits_pixel2host,
    uint32_t *x, uint32_t *y
) {
  int curve_order = bits_canvas2pixel / 2;
  uint32_t pixel_int = address_to_pixel_int(address, canvas_prefix, bits_pixel2host);
  transform(pixel_int, curve_order, x, y);
}

template<class Network, class Transformer>
BoundingBox network_to_bbox(
    const Network &network, Transformer &&transform,
    int canvas_prefix, int bits_canvas2network, int bits_network2pixel, int bits_pixel2host
) {
  uint32_t first_int = address_to_pixel_int(network.address(), canvas_prefix, bits_pixel2host);
  return traverse_bbox_diagonal(first_int, transform, bits_canvas2network, bits_network2pixel);
}

}

#endif
