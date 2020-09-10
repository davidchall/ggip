#ifndef __GGIP_ADDRESS_TO_PIXEL_INT__
#define __GGIP_ADDRESS_TO_PIXEL_INT__

#include <ipaddress/IpAddress.h>
#include "AddressMapping.h"


uint32_t address_to_pixel_int(const ipaddress::IpAddress &address, AddressMapping mapping);

#endif
