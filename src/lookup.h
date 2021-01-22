#include <vector>
#include <string>
#include <regex>
#include <Rcpp.h>

class Lookup
{
public:
    Lookup(Rcpp::DataFrame& d, Rcpp::DataFrame& d_code_names, Rcpp::DataFrame& d_metrics);

    uint32_t LookUpKey(uint32_t key, const std::string& geo);

    Rcpp::StringVector FindName(const std::regex& r);
    std::vector<Rcpp::String> GetCodes(const std::string& geo);
    Rcpp::String MatchName(const std::string& name, const std::string& geo);

    Rcpp::StringVector GetNames(const std::vector<uint32_t>& mapped_codes);
    Rcpp::NumericVector GetMetrics(const std::vector<uint32_t>& mapped_codes, const Rcpp::String& metric);
    Rcpp::StringVector TranslateGeo(const std::vector<uint32_t>& mapped_codes, const Rcpp::String& geo_to);
    SEXP GetProperties(const Rcpp::StringVector& code, const Rcpp::String& property);

private:
    template <typename Value>
    struct lpair {
        uint32_t c;
        Value v;

        lpair() : c(), v() { }
        bool operator<(const lpair& rhs) const { return (c == rhs.c) ? v < rhs.v : c < rhs.c; }
        bool operator<(const uint32_t& rhs) const { return c < rhs; }
    };

    template <typename Value>
    friend bool operator<(const uint32_t& lhs, const Lookup::lpair<Value>& rhs);

    typedef lpair<uint32_t> coderow;
    typedef lpair<std::string> codename;

    std::vector<std::string> geos;              // geos[geo_i]
    std::vector<std::vector<uint32_t>> codes;   // codes[geo_i][row]
    std::vector<std::vector<coderow>> crmap;    // crmap[geo_i][ordered] -> c,v ONS codes (c) mapping to rows (r) of codes; one-to-many
    std::vector<codename> names;                // names[ordered] -> c,v ONS codes (c) mapping to name (v) of code; one-to-one
    std::vector<std::string> metric_names;      // metric_names[metric_i]
    std::vector<std::vector<double>> metrics;   // metrics[metric_i][row]
};

template <typename Value>
bool operator<(const uint32_t& lhs, const Lookup::lpair<Value>& rhs)
{
    return lhs < rhs.c;
}
