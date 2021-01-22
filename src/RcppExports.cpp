// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// CreateCache
void CreateCache();
RcppExport SEXP _ogwrangler_CreateCache() {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    CreateCache();
    return R_NilValue;
END_RCPP
}
// ogpost
StringVector ogpost(StringVector postcode, StringVector geo);
RcppExport SEXP _ogwrangler_ogpost(SEXP postcodeSEXP, SEXP geoSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< StringVector >::type postcode(postcodeSEXP);
    Rcpp::traits::input_parameter< StringVector >::type geo(geoSEXP);
    rcpp_result_gen = Rcpp::wrap(ogpost(postcode, geo));
    return rcpp_result_gen;
END_RCPP
}
// ogfind
StringVector ogfind(StringVector text);
RcppExport SEXP _ogwrangler_ogfind(SEXP textSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< StringVector >::type text(textSEXP);
    rcpp_result_gen = Rcpp::wrap(ogfind(text));
    return rcpp_result_gen;
END_RCPP
}
// ogcode
StringVector ogcode(StringVector name, StringVector geo);
RcppExport SEXP _ogwrangler_ogcode(SEXP nameSEXP, SEXP geoSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< StringVector >::type name(nameSEXP);
    Rcpp::traits::input_parameter< StringVector >::type geo(geoSEXP);
    rcpp_result_gen = Rcpp::wrap(ogcode(name, geo));
    return rcpp_result_gen;
END_RCPP
}
// ogwhat
SEXP ogwhat(StringVector code, String property);
RcppExport SEXP _ogwrangler_ogwhat(SEXP codeSEXP, SEXP propertySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< StringVector >::type code(codeSEXP);
    Rcpp::traits::input_parameter< String >::type property(propertySEXP);
    rcpp_result_gen = Rcpp::wrap(ogwhat(code, property));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_ogwrangler_CreateCache", (DL_FUNC) &_ogwrangler_CreateCache, 0},
    {"_ogwrangler_ogpost", (DL_FUNC) &_ogwrangler_ogpost, 2},
    {"_ogwrangler_ogfind", (DL_FUNC) &_ogwrangler_ogfind, 1},
    {"_ogwrangler_ogcode", (DL_FUNC) &_ogwrangler_ogcode, 2},
    {"_ogwrangler_ogwhat", (DL_FUNC) &_ogwrangler_ogwhat, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_ogwrangler(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
