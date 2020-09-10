#include <ipaddress.h>
#include "address_to_pixel_int.h"

using namespace ipaddress;


uint32_t address_to_pixel_int(const IpAddress &address, AddressMapping mapping) {
  IpAddress canvas_hostmask = prefix_to_hostmask(
    mapping.space_bits - mapping.canvas_bits,
    address.is_ipv6()
  );

  IpAddress pixel_address = bitwise_shift_right(bitwise_and(address, canvas_hostmask), mapping.pixel_bits);

  // interpret final 4 bytes as integer
  // NOTE: this limits plotting to canvas_bits - pixel_bits <= 32
  uint32_t pixel_int;
  std::memcpy(&pixel_int, pixel_address.cend() - 4, 4);
  pixel_int = network_to_host_long(pixel_int);

  return pixel_int;
}
