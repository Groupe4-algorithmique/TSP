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
// get_subsets_Rcpp
List get_subsets_Rcpp(NumericVector set, int p);
RcppExport SEXP _tspp_get_subsets_Rcpp(SEXP setSEXP, SEXP pSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type set(setSEXP);
    Rcpp::traits::input_parameter< int >::type p(pSEXP);
    rcpp_result_gen = Rcpp::wrap(get_subsets_Rcpp(set, p));
    return rcpp_result_gen;
END_RCPP
}
// to_String
String to_String(NumericVector v);
RcppExport SEXP _tspp_to_String(SEXP vSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type v(vSEXP);
    rcpp_result_gen = Rcpp::wrap(to_String(v));
    return rcpp_result_gen;
END_RCPP
}
// construct_C_S_k_Rcpp
List construct_C_S_k_Rcpp(List C, NumericVector Subset, int k, NumericMatrix G);
RcppExport SEXP _tspp_construct_C_S_k_Rcpp(SEXP CSEXP, SEXP SubsetSEXP, SEXP kSEXP, SEXP GSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type C(CSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type Subset(SubsetSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type G(GSEXP);
    rcpp_result_gen = Rcpp::wrap(construct_C_S_k_Rcpp(C, Subset, k, G));
    return rcpp_result_gen;
END_RCPP
}
// search_min_C_S_k_Rcpp
List search_min_C_S_k_Rcpp(List C_S_k);
RcppExport SEXP _tspp_search_min_C_S_k_Rcpp(SEXP C_S_kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type C_S_k(C_S_kSEXP);
    rcpp_result_gen = Rcpp::wrap(search_min_C_S_k_Rcpp(C_S_k));
    return rcpp_result_gen;
END_RCPP
}
// delete_element
NumericVector delete_element(NumericVector vec, int el);
RcppExport SEXP _tspp_delete_element(SEXP vecSEXP, SEXP elSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type vec(vecSEXP);
    Rcpp::traits::input_parameter< int >::type el(elSEXP);
    rcpp_result_gen = Rcpp::wrap(delete_element(vec, el));
    return rcpp_result_gen;
END_RCPP
}
// str_to_int
int str_to_int(String numStr);
RcppExport SEXP _tspp_str_to_int(SEXP numStrSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< String >::type numStr(numStrSEXP);
    rcpp_result_gen = Rcpp::wrap(str_to_int(numStr));
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
    {"_tspp_get_subsets_Rcpp", (DL_FUNC) &_tspp_get_subsets_Rcpp, 2},
    {"_tspp_to_String", (DL_FUNC) &_tspp_to_String, 1},
    {"_tspp_construct_C_S_k_Rcpp", (DL_FUNC) &_tspp_construct_C_S_k_Rcpp, 4},
    {"_tspp_search_min_C_S_k_Rcpp", (DL_FUNC) &_tspp_search_min_C_S_k_Rcpp, 1},
    {"_tspp_delete_element", (DL_FUNC) &_tspp_delete_element, 2},
    {"_tspp_str_to_int", (DL_FUNC) &_tspp_str_to_int, 1},
    {"_tspp_held_karp_Rcpp", (DL_FUNC) &_tspp_held_karp_Rcpp, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_tspp(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
