#ifndef __GGIP_MAPPING__
#define __GGIP_MAPPING__

#include <ipaddress/IpNetwork.h>


struct AddressMapping {
  unsigned int space_bits, canvas_bits, pixel_bits;
};

AddressMapping setup_mapping(const ipaddress::IpNetwork &canvas_network, int pixel_prefix);

uint32_t address_to_integer(const ipaddress::IpAddress &address, AddressMapping mapping);

#endif
