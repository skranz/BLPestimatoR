// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// getSij
NumericMatrix getSij(const NumericMatrix& expmu, const NumericVector& expdelta, const IntegerVector& cdindex);
RcppExport SEXP _BLPestimatoR_getSij(SEXP expmuSEXP, SEXP expdeltaSEXP, SEXP cdindexSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const NumericMatrix& >::type expmu(expmuSEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type expdelta(expdeltaSEXP);
    Rcpp::traits::input_parameter< const IntegerVector& >::type cdindex(cdindexSEXP);
    rcpp_result_gen = Rcpp::wrap(getSij(expmu, expdelta, cdindex));
    return rcpp_result_gen;
END_RCPP
}
// getSjtMod
NumericVector getSjtMod(const NumericMatrix& expmu, const NumericVector& expdelta, const int& nprodt, const int& startpos, const NumericVector& weights);
RcppExport SEXP _BLPestimatoR_getSjtMod(SEXP expmuSEXP, SEXP expdeltaSEXP, SEXP nprodtSEXP, SEXP startposSEXP, SEXP weightsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const NumericMatrix& >::type expmu(expmuSEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type expdelta(expdeltaSEXP);
    Rcpp::traits::input_parameter< const int& >::type nprodt(nprodtSEXP);
    Rcpp::traits::input_parameter< const int& >::type startpos(startposSEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type weights(weightsSEXP);
    rcpp_result_gen = Rcpp::wrap(getSjtMod(expmu, expdelta, nprodt, startpos, weights));
    return rcpp_result_gen;
END_RCPP
}
// getExpMu
NumericMatrix getExpMu(const NumericMatrix& theta2Matrix, const NumericMatrix& qv, const NumericMatrix& Xrandom, const IntegerVector& cdid, const NumericMatrix& demographics);
RcppExport SEXP _BLPestimatoR_getExpMu(SEXP theta2MatrixSEXP, SEXP qvSEXP, SEXP XrandomSEXP, SEXP cdidSEXP, SEXP demographicsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const NumericMatrix& >::type theta2Matrix(theta2MatrixSEXP);
    Rcpp::traits::input_parameter< const NumericMatrix& >::type qv(qvSEXP);
    Rcpp::traits::input_parameter< const NumericMatrix& >::type Xrandom(XrandomSEXP);
    Rcpp::traits::input_parameter< const IntegerVector& >::type cdid(cdidSEXP);
    Rcpp::traits::input_parameter< const NumericMatrix& >::type demographics(demographicsSEXP);
    rcpp_result_gen = Rcpp::wrap(getExpMu(theta2Matrix, qv, Xrandom, cdid, demographics));
    return rcpp_result_gen;
END_RCPP
}
// getDelta
List getDelta(const NumericMatrix& theta2, const NumericVector& deltaOld, const IntegerVector& cdid, const IntegerVector& cdindex, const NumericMatrix& Xrandom, const NumericVector& obsshare, const double& innerCrit, const int& innerMaxit, const int& printLevel, const NumericMatrix& indices, const NumericMatrix& nodesRcMktShape, const NumericMatrix& nodesDemMktShape, const NumericVector& weights);
RcppExport SEXP _BLPestimatoR_getDelta(SEXP theta2SEXP, SEXP deltaOldSEXP, SEXP cdidSEXP, SEXP cdindexSEXP, SEXP XrandomSEXP, SEXP obsshareSEXP, SEXP innerCritSEXP, SEXP innerMaxitSEXP, SEXP printLevelSEXP, SEXP indicesSEXP, SEXP nodesRcMktShapeSEXP, SEXP nodesDemMktShapeSEXP, SEXP weightsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const NumericMatrix& >::type theta2(theta2SEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type deltaOld(deltaOldSEXP);
    Rcpp::traits::input_parameter< const IntegerVector& >::type cdid(cdidSEXP);
    Rcpp::traits::input_parameter< const IntegerVector& >::type cdindex(cdindexSEXP);
    Rcpp::traits::input_parameter< const NumericMatrix& >::type Xrandom(XrandomSEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type obsshare(obsshareSEXP);
    Rcpp::traits::input_parameter< const double& >::type innerCrit(innerCritSEXP);
    Rcpp::traits::input_parameter< const int& >::type innerMaxit(innerMaxitSEXP);
    Rcpp::traits::input_parameter< const int& >::type printLevel(printLevelSEXP);
    Rcpp::traits::input_parameter< const NumericMatrix& >::type indices(indicesSEXP);
    Rcpp::traits::input_parameter< const NumericMatrix& >::type nodesRcMktShape(nodesRcMktShapeSEXP);
    Rcpp::traits::input_parameter< const NumericMatrix& >::type nodesDemMktShape(nodesDemMktShapeSEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type weights(weightsSEXP);
    rcpp_result_gen = Rcpp::wrap(getDelta(theta2, deltaOld, cdid, cdindex, Xrandom, obsshare, innerCrit, innerMaxit, printLevel, indices, nodesRcMktShape, nodesDemMktShape, weights));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_BLPestimatoR_getSij", (DL_FUNC) &_BLPestimatoR_getSij, 3},
    {"_BLPestimatoR_getSjtMod", (DL_FUNC) &_BLPestimatoR_getSjtMod, 5},
    {"_BLPestimatoR_getExpMu", (DL_FUNC) &_BLPestimatoR_getExpMu, 5},
    {"_BLPestimatoR_getDelta", (DL_FUNC) &_BLPestimatoR_getDelta, 13},
    {NULL, NULL, 0}
};

RcppExport void R_init_BLPestimatoR(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
