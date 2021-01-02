// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// compute_distance_Rcpp
double compute_distance_Rcpp(NumericMatrix G, NumericVector cities, unsigned int start_city);
RcppExport SEXP _tspp_compute_distance_Rcpp(SEXP GSEXP, SEXP citiesSEXP, SEXP start_citySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type G(GSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type cities(citiesSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type start_city(start_citySEXP);
    rcpp_result_gen = Rcpp::wrap(compute_distance_Rcpp(G, cities, start_city));
    return rcpp_result_gen;
END_RCPP
}
// naive_method_Rcpp
List naive_method_Rcpp(NumericMatrix G, NumericVector cities, unsigned int start_city);
RcppExport SEXP _tspp_naive_method_Rcpp(SEXP GSEXP, SEXP citiesSEXP, SEXP start_citySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type G(GSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type cities(citiesSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type start_city(start_citySEXP);
    rcpp_result_gen = Rcpp::wrap(naive_method_Rcpp(G, cities, start_city));
    return rcpp_result_gen;
END_RCPP
}
// held_karp_Rcpp
List held_karp_Rcpp(NumericMatrix G, int n);
RcppExport SEXP _tspp_held_karp_Rcpp(SEXP GSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type G(GSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(held_karp_Rcpp(G, n));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_tspp_compute_distance_Rcpp", (DL_FUNC) &_tspp_compute_distance_Rcpp, 3},
    {"_tspp_naive_method_Rcpp", (DL_FUNC) &_tspp_naive_method_Rcpp, 3},
    {"_tspp_held_karp_Rcpp", (DL_FUNC) &_tspp_held_karp_Rcpp, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_tspp(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
