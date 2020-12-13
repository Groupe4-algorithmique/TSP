// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// swap
void swap(NumericVector v, unsigned int i, unsigned int j);
RcppExport SEXP _tspp_swap(SEXP vSEXP, SEXP iSEXP, SEXP jSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type v(vSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type i(iSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type j(jSEXP);
    swap(v, i, j);
    return R_NilValue;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_tspp_swap", (DL_FUNC) &_tspp_swap, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_tspp(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
