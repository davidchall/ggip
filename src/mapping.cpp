#include <ipaddress.h>
#include "mapping.h"

using namespace ipaddress;


AddressMapping setup_mapping(const IpNetwork &canvas_network, int pixel_prefix) {
  AddressMapping mapping;

  mapping.space_bits = canvas_network.address().n_bits();
  mapping.canvas_bits = mapping.space_bits - canvas_network.prefix_length();
  mapping.pixel_bits = mapping.space_bits - pixel_prefix;

  return mapping;
}

uint32_t address_to_integer(const IpAddress &address, AddressMapping mapping) {

  // neglect leading bits
  IpAddress canvas_hostmask = prefix_to_hostmask(
    mapping.space_bits - mapping.canvas_bits,
    address.is_ipv6()
  );
  IpAddress reduced_address = bitwise_and(address, canvas_hostmask);

  // neglect trailing bits
  reduced_address = bitwise_shift_right(reduced_address, mapping.pixel_bits);

  // interpret final 4 bytes as integer
  // NOTE: this limits plotting to canvas_bits - pixel_bits <= 32
  uint32_t reduced_integer;
  std::memcpy(&reduced_integer, reduced_address.end() - 4, 4);
  reduced_integer = network_to_host_long(reduced_integer);

  return reduced_integer;
}
