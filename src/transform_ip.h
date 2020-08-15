#ifndef __GGIP_TRANSFORM_IP__
#define __GGIP_TRANSFORM_IP__

#include <algorithm>
#include <ipaddress/bitwise.h>
#include <ipaddress/masking.h>


namespace ipaddress {

struct AddressMapping {
  unsigned int space_bits, canvas_bits, network_bits, pixel_bits;
};

struct BoundingBox {
  uint32_t xmin, xmax, ymin, ymax;
};

template<class Address>
uint32_t address_to_pixel_int(const Address &address, AddressMapping mapping) {
  Address canvas_hostmask = prefix_to_hostmask<Address>(mapping.space_bits - mapping.canvas_bits);

  Address pixel_address = bitwise_shift_right(bitwise_and(address, canvas_hostmask), mapping.pixel_bits);
  typename Address::bytes_type pixel_bytes = pixel_address.to_bytes();

  // interpret final 4 bytes as integer
  // NOTE: this limits plotting to canvas_bits - pixel_bits <= 32
  uint32_t pixel_int;
  std::memcpy(&pixel_int, pixel_bytes.end() - 4, 4);
  pixel_int = ntohl(pixel_int);

  return pixel_int;
}

template<class Transformer>
BoundingBox traverse_bbox_diagonal(uint32_t first_pixel_int, AddressMapping mapping, Transformer &&transform) {
  BoundingBox bbox;
  uint32_t diag = 0xAAAAAAAA;
  uint32_t x1, x2, y1, y2;
  unsigned int curve_order = (mapping.canvas_bits - mapping.pixel_bits) / 2;

  if ((mapping.network_bits - mapping.pixel_bits) == 0) {  // no area
    transform(first_pixel_int, curve_order, &x1, &y1);

    bbox.xmin = x1;
    bbox.ymin = y1;
    bbox.xmax = x1;
    bbox.ymax = y1;
  } else if (((mapping.network_bits - mapping.pixel_bits) & 1) == 0) {  // square
    transform(first_pixel_int, curve_order, &x1, &y1);
    transform(first_pixel_int | (diag >> (mapping.canvas_bits - mapping.network_bits)), curve_order, &x2, &y2);

    bbox.xmin = std::min(x1, x2);
    bbox.ymin = std::min(y1, y2);
    bbox.xmax = std::max(x1, x2);
    bbox.ymax = std::max(y1, y2);
  } else {  // rectangle
    mapping.network_bits -= 1;

    BoundingBox square1 = traverse_bbox_diagonal(first_pixel_int, mapping, transform);
    BoundingBox square2 = traverse_bbox_diagonal(
      first_pixel_int + (1 << (mapping.network_bits - mapping.pixel_bits - 1)),
      mapping, transform
    );

    bbox.xmin = std::min(square1.xmin, square2.xmin);
    bbox.ymin = std::min(square1.ymin, square2.ymin);
    bbox.xmax = std::max(square1.xmax, square2.xmax);
    bbox.ymax = std::max(square1.ymax, square2.ymax);
  }

  return bbox;
}

template<class Address, class Transformer>
void address_to_xy(const Address &address, AddressMapping mapping, Transformer &&transform, uint32_t *x, uint32_t *y) {
  int curve_order = (mapping.canvas_bits - mapping.pixel_bits) / 2;
  uint32_t pixel_int = address_to_pixel_int(address, mapping);
  transform(pixel_int, curve_order, x, y);
}

template<class Network, class Transformer>
BoundingBox network_to_bbox(const Network &network, AddressMapping mapping, Transformer &&transform) {
  mapping.network_bits = mapping.space_bits - network.prefix_length();
  uint32_t first_int = address_to_pixel_int(network.address(), mapping);
  return traverse_bbox_diagonal(first_int, mapping, transform);
}

}

#endif
