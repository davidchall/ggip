// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// address_to_cartesian
DataFrame address_to_cartesian(List address_r, List canvas_network_r, int pixel_prefix);
RcppExport SEXP _ggip_address_to_cartesian(SEXP address_rSEXP, SEXP canvas_network_rSEXP, SEXP pixel_prefixSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type address_r(address_rSEXP);
    Rcpp::traits::input_parameter< List >::type canvas_network_r(canvas_network_rSEXP);
    Rcpp::traits::input_parameter< int >::type pixel_prefix(pixel_prefixSEXP);
    rcpp_result_gen = Rcpp::wrap(address_to_cartesian(address_r, canvas_network_r, pixel_prefix));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_ggip_address_to_cartesian", (DL_FUNC) &_ggip_address_to_cartesian, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_ggip(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
