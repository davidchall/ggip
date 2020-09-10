#ifndef __GGIP_MAPPING__
#define __GGIP_MAPPING__

#include <ipaddress/IpNetwork.h>


struct AddressMapping {
  unsigned int space_bits, canvas_bits, network_bits, pixel_bits;
};

inline AddressMapping get_mapping(const ipaddress::IpNetwork &canvas_network, int pixel_prefix) {
  AddressMapping mapping;

  mapping.space_bits = canvas_network.address().n_bits();
  mapping.canvas_bits = mapping.space_bits - canvas_network.prefix_length();
  mapping.pixel_bits = mapping.space_bits - pixel_prefix;

  return mapping;
}

#endif
