#include <Rcpp.h>
#include <ipaddress/IpAddressVector.h>
#include <ipaddress/IpNetworkVector.h>
#include "transform_ip.h"

using namespace Rcpp;
using namespace ipaddress;


template<class Network>
bool is_subnet(const Network &network, const Network &other) {
  return address_in_network(network.address(), other) && (network.prefix_length() >= other.prefix_length());
}


// [[Rcpp::export]]
DataFrame address_to_cartesian(List address_r, List canvas_network_r, int pixel_prefix, String curve) {
  IpAddressVector address(address_r);
  IpNetworkVector canvas_network(canvas_network_r);

  if (canvas_network.size() != 1) {
    stop("'canvas_network' must be an ip_network scalar"); // # nocov
  }

  // initialize output vectors
  std::size_t vsize = address.size();
  IntegerVector out_x(vsize);
  IntegerVector out_y(vsize);

  // setup mapping from IP space to plotting canvas
  bool canvas_ipv6 = canvas_network.is_ipv6[0];
  AddressMapping mapping;
  if (canvas_ipv6) {
    mapping.space_bits = 128;
    mapping.canvas_bits = 128 - canvas_network.network_v6[0].prefix_length();
  } else {
    mapping.space_bits = 32;
    mapping.canvas_bits = 32 - canvas_network.network_v4[0].prefix_length();
  }
  mapping.pixel_bits = mapping.space_bits - pixel_prefix;

  // setup curve
  bool is_morton = (curve == "morton");

  for (std::size_t i=0; i<vsize; ++i) {
    if (i % 10000 == 0) {
      checkUserInterrupt();
    }

    if (address.is_na[i] || (address.is_ipv6[i] != canvas_ipv6)) {
      out_x[i] = NA_INTEGER;
      out_y[i] = NA_INTEGER;
    } else if (address.is_ipv6[i]) {
      if (address_in_network(address.address_v6[i], canvas_network.network_v6[0])) {
        uint32_t x, y;
        address_to_xy(address.address_v6[i], mapping, is_morton, &x, &y);
        out_x[i] = x;
        out_y[i] = y;
      } else {
        out_x[i] = NA_INTEGER;
        out_y[i] = NA_INTEGER;
      }
    } else {
      if (address_in_network(address.address_v4[i], canvas_network.network_v4[0])) {
        uint32_t x, y;
        address_to_xy(address.address_v4[i], mapping, is_morton, &x, &y);
        out_x[i] = x;
        out_y[i] = y;
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


// [[Rcpp::export]]
DataFrame network_to_cartesian(List network_r, List canvas_network_r, int pixel_prefix, String curve) {
  IpNetworkVector network(network_r);
  IpNetworkVector canvas_network(canvas_network_r);

  if (canvas_network.size() != 1) {
    stop("'canvas_network' must be an ip_network scalar"); // # nocov
  }

  // initialize output vectors
  std::size_t vsize = network.size();
  IntegerVector out_xmin(vsize);
  IntegerVector out_xmax(vsize);
  IntegerVector out_ymin(vsize);
  IntegerVector out_ymax(vsize);

  // setup mapping from IP space to plotting canvas
  bool canvas_ipv6 = canvas_network.is_ipv6[0];
  AddressMapping mapping;
  if (canvas_ipv6) {
    mapping.space_bits = 128;
    mapping.canvas_bits = 128 - canvas_network.network_v6[0].prefix_length();
  } else {
    mapping.space_bits = 32;
    mapping.canvas_bits = 32 - canvas_network.network_v4[0].prefix_length();
  }
  mapping.pixel_bits = mapping.space_bits - pixel_prefix;

  // setup curve
  bool is_morton = (curve == "morton");

  for (std::size_t i=0; i<vsize; ++i) {
    if (i % 10000 == 0) {
      checkUserInterrupt();
    }

    if (network.is_na[i] || (network.is_ipv6[i] != canvas_ipv6)) {
      out_xmin[i] = NA_INTEGER;
      out_xmax[i] = NA_INTEGER;
      out_ymin[i] = NA_INTEGER;
      out_ymax[i] = NA_INTEGER;
    } else if (network.is_ipv6[i]) {
      if (is_subnet(network.network_v6[i], canvas_network.network_v6[0])) {
        BoundingBox bbox = network_to_bbox(network.network_v6[i], mapping, is_morton);
        out_xmin[i] = bbox.xmin;
        out_ymin[i] = bbox.ymax;
        out_xmax[i] = bbox.xmax;
        out_ymax[i] = bbox.ymin;
      } else {
        out_xmin[i] = NA_INTEGER;
        out_xmax[i] = NA_INTEGER;
        out_ymin[i] = NA_INTEGER;
        out_ymax[i] = NA_INTEGER;
      }
    } else {
      if (is_subnet(network.network_v4[i], canvas_network.network_v4[0])) {
        BoundingBox bbox = network_to_bbox(network.network_v4[i], mapping, is_morton);
        out_xmin[i] = bbox.xmin;
        out_ymin[i] = bbox.ymax;
        out_xmax[i] = bbox.xmax;
        out_ymax[i] = bbox.ymin;
      } else {
        out_xmin[i] = NA_INTEGER;
        out_xmax[i] = NA_INTEGER;
        out_ymin[i] = NA_INTEGER;
        out_ymax[i] = NA_INTEGER;
      }
    }
  }

  return DataFrame::create(
    _["xmin"] = out_xmin,
    _["xmax"] = out_xmax,
    _["ymin"] = out_ymin,
    _["ymax"] = out_ymax
  );
}
