#include <Rcpp.h>
#include <ipaddress.h>
#include "mapping.h"
#include "curves.h"

using namespace Rcpp;
using namespace ipaddress;


void address_to_pixel(const IpAddress &address, AddressMapping mapping, bool is_morton, uint32_t *x, uint32_t *y) {
  int curve_order = (mapping.canvas_bits - mapping.pixel_bits) / 2;
  uint32_t pixel_int = address_to_integer(address, mapping);
  if (is_morton) {
    morton_curve(pixel_int, curve_order, x, y);
  } else {
    hilbert_curve(pixel_int, curve_order, x, y);
  }
}

// [[Rcpp::export]]
DataFrame wrap_address_to_cartesian(List address_r, List canvas_network_r, int pixel_prefix, String curve) {
  std::vector<IpAddress> address = decode_addresses(address_r);
  std::vector<IpNetwork> canvas_networks = decode_networks(canvas_network_r);

  if (canvas_networks.size() != 1) {
    stop("'canvas_network' must be an ip_network scalar"); // # nocov
  }
  IpNetwork canvas_network = canvas_networks[0];

  // initialize output vectors
  std::size_t vsize = address.size();
  IntegerVector out_x(vsize);
  IntegerVector out_y(vsize);

  // setup mapping from IP space to plotting canvas
  AddressMapping mapping = setup_mapping(canvas_network, pixel_prefix);

  // setup curve
  bool is_morton = (curve == "morton");

  for (std::size_t i=0; i<vsize; ++i) {
    if (i % 10000 == 0) {
      checkUserInterrupt();
    }

    if (address_in_network(address[i], canvas_network)) {
      uint32_t x, y;
      address_to_pixel(address[i], mapping, is_morton, &x, &y);
      out_x[i] = x;
      out_y[i] = y;
    } else {
      out_x[i] = NA_INTEGER;
      out_y[i] = NA_INTEGER;
    }
  }

  return DataFrame::create(
    _["x"] = out_x,
    _["y"] = out_y
  );
}
