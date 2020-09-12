#include <Rcpp.h>
#include <ipaddress.h>
#include "mapping.h"
#include "curves.h"

using namespace Rcpp;
using namespace ipaddress;


struct BoundingBox {
  uint32_t xmin, xmax, ymin, ymax;
};

BoundingBox network_to_bbox_hilbert(uint32_t first_pixel, unsigned int network_bits, AddressMapping mapping) {
  BoundingBox bbox;
  uint32_t x1, x2, y1, y2;
  unsigned int curve_order = (mapping.canvas_bits - mapping.pixel_bits) / 2;

  if (network_bits <= mapping.pixel_bits) {  // no area
    hilbert_curve(first_pixel, curve_order, &x1, &y1);

    bbox.xmin = x1;
    bbox.ymin = y1;
    bbox.xmax = x1;
    bbox.ymax = y1;
  } else if (((network_bits - mapping.pixel_bits) & 1) == 0) {  // square
    uint32_t diag = 0xAAAAAAAA;
    uint32_t last_pixel = first_pixel | (diag >> (32 - (network_bits - mapping.pixel_bits)));

    hilbert_curve(first_pixel, curve_order, &x1, &y1);
    hilbert_curve(last_pixel, curve_order, &x2, &y2);

    bbox.xmin = std::min(x1, x2);
    bbox.ymin = std::min(y1, y2);
    bbox.xmax = std::max(x1, x2);
    bbox.ymax = std::max(y1, y2);
  } else {  // rectangle
    network_bits--;
    uint32_t mid_pixel = first_pixel | (1 << (network_bits - mapping.pixel_bits));

    BoundingBox square1 = network_to_bbox_hilbert(first_pixel, network_bits, mapping);
    BoundingBox square2 = network_to_bbox_hilbert(mid_pixel, network_bits, mapping);

    bbox.xmin = std::min(square1.xmin, square2.xmin);
    bbox.ymin = std::min(square1.ymin, square2.ymin);
    bbox.xmax = std::max(square1.xmax, square2.xmax);
    bbox.ymax = std::max(square1.ymax, square2.ymax);
  }

  return bbox;
}

BoundingBox network_to_bbox(const IpNetwork &network, AddressMapping mapping, bool is_morton) {
  uint32_t first_pixel = address_to_integer(network.address(), mapping);

  if (is_morton) {
    BoundingBox bbox;
    uint32_t x1, x2, y1, y2;
    unsigned int curve_order = (mapping.canvas_bits - mapping.pixel_bits) / 2;

    uint32_t last_pixel = address_to_integer(broadcast_address(network), mapping);

    morton_curve(first_pixel, curve_order, &x1, &y1);
    morton_curve(last_pixel, curve_order, &x2, &y2);

    bbox.xmin = std::min(x1, x2);
    bbox.ymin = std::min(y1, y2);
    bbox.xmax = std::max(x1, x2);
    bbox.ymax = std::max(y1, y2);

    return bbox;
  } else {
    unsigned int network_bits = mapping.space_bits - network.prefix_length();
    return network_to_bbox_hilbert(first_pixel, network_bits, mapping);
  }
}

// [[Rcpp::export]]
DataFrame wrap_network_to_cartesian(List network_r, List canvas_network_r, int pixel_prefix, String curve) {
  std::vector<IpNetwork> network = decode_networks(network_r);
  std::vector<IpNetwork> canvas_networks = decode_networks(canvas_network_r);

  if (canvas_networks.size() != 1) {
    stop("'canvas_network' must be an ip_network scalar"); // # nocov
  }
  IpNetwork canvas_network = canvas_networks[0];

  // initialize output vectors
  std::size_t vsize = network.size();
  IntegerVector out_xmin(vsize);
  IntegerVector out_ymin(vsize);
  IntegerVector out_xmax(vsize);
  IntegerVector out_ymax(vsize);

  // setup mapping from IP space to plotting canvas
  AddressMapping mapping = setup_mapping(canvas_network, pixel_prefix);

  // setup curve
  bool is_morton = (curve == "morton");

  for (std::size_t i=0; i<vsize; ++i) {
    if (i % 10000 == 0) {
      checkUserInterrupt();
    }

    if (is_subnet(network[i], canvas_network)) {
      BoundingBox bbox = network_to_bbox(network[i], mapping, is_morton);
      out_xmin[i] = bbox.xmin;
      out_ymin[i] = bbox.ymin;
      out_xmax[i] = bbox.xmax;
      out_ymax[i] = bbox.ymax;
    } else {
      out_xmin[i] = NA_INTEGER;
      out_ymin[i] = NA_INTEGER;
      out_xmax[i] = NA_INTEGER;
      out_ymax[i] = NA_INTEGER;
    }
  }

  return DataFrame::create(
    _["xmin"] = out_xmin,
    _["ymin"] = out_ymin,
    _["xmax"] = out_xmax,
    _["ymax"] = out_ymax
  );
}
