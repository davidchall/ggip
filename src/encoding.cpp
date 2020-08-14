#include <Rcpp.h>
#include <ipaddress/IpAddressVector.h>
#include <ipaddress/IpNetworkVector.h>
#include <ipaddress/bitwise.h>
#include <ipaddress/masking.h>

using namespace Rcpp;
using namespace ipaddress;


template<class Address, class Network>
uint32_t address_to_pixel_int(const Address &address, const Network &canvas_network, unsigned int pixel_n_bits) {
  Address canvas_hostmask = prefix_to_hostmask<Address>(canvas_network.prefix_length());

  Address pixel_address = bitwise_shift_right(bitwise_and(address, canvas_hostmask), pixel_n_bits);
  typename Address::bytes_type pixel_bytes = pixel_address.to_bytes();

  // interpret final 4 bytes as integer
  uint32_t pixel_int;
  std::memcpy(&pixel_int, pixel_bytes.end() - 4, 4);
  pixel_int = ntohl(pixel_int);

  return pixel_int;
}

void encode_hilbert(uint32_t s, int order, uint32_t *x, uint32_t *y) {
  unsigned int state, row;
  state = *x = *y = 0;

  for (int i=2*order-2; i>=0; i-=2) {
    row = 4 * state | ((s >> i) & 3);

    *x = (*x << 1) | ((0x936C >> row) & 1);
    *y = (*y << 1) | ((0x39C6 >> row) & 1);

    state = (0x3E6B94C1 >> 2 * row) & 3;
  }
}

// [[Rcpp::export]]
DataFrame address_to_cartesian(List address_r, List canvas_network_r, int pixel_prefix) {
  IpAddressVector address(address_r);
  IpNetworkVector canvas_network(canvas_network_r);

  if (canvas_network.size() != 1) {
    stop("'canvas_network' must be an ip_network scalar"); // # nocov
  }

  // initialize output vectors
  std::size_t vsize = address.size();
  IntegerVector out_x(vsize);
  IntegerVector out_y(vsize);

  bool canvas_ipv6 = canvas_network.is_ipv6[0];
  unsigned int canvas_prefix, pixel_n_bits;
  if (canvas_ipv6) {
    canvas_prefix = canvas_network.network_v6[0].prefix_length();
    pixel_n_bits = 128 - pixel_prefix;
  } else {
    canvas_prefix = canvas_network.network_v4[0].prefix_length();
    pixel_n_bits = 32 - pixel_prefix;
  }
  unsigned int curve_order = (pixel_prefix - canvas_prefix) / 2;
  unsigned int range = (1 << curve_order) - 1;

  for (std::size_t i=0; i<vsize; ++i) {
    if (i % 10000 == 0) {
      checkUserInterrupt();
    }

    if (address.is_na[i]) {
      out_x[i] = NA_INTEGER;
      out_y[i] = NA_INTEGER;
    } else if (address.is_ipv6[i] != canvas_ipv6) {
      out_x[i] = NA_INTEGER;
      out_y[i] = NA_INTEGER;
    } else if (address.is_ipv6[i]) {
      if (address_in_network(address.address_v6[i], canvas_network.network_v6[0])) {
        uint32_t x, y;

        uint32_t pixel_int = address_to_pixel_int(address.address_v6[i], canvas_network.network_v6[0], pixel_n_bits);
        encode_hilbert(pixel_int, curve_order, &x, &y);

        out_x[i] = x;
        out_y[i] = range - y;
      } else {
        out_x[i] = NA_INTEGER;
        out_y[i] = NA_INTEGER;
      }
    } else {
      if (address_in_network(address.address_v4[i], canvas_network.network_v4[0])) {
        uint32_t x, y;

        uint32_t pixel_int = address_to_pixel_int(address.address_v4[i], canvas_network.network_v4[0], pixel_n_bits);
        encode_hilbert(pixel_int, curve_order, &x, &y);

        out_x[i] = x;
        out_y[i] = range - y;
      } else {
        out_x[i] = NA_INTEGER;
        out_y[i] = NA_INTEGER;
      }
    }
  }

  return DataFrame::create(
    _["x"] = out_x,
    _["y"] = out_y
  );
}
