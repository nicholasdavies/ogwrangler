#include <vector>
#include <string>
#include <Rcpp.h>

class PostcodeMap
{
public:
    PostcodeMap(Rcpp::StringVector& p, Rcpp::StringVector& k);

    std::vector<uint32_t> Key(std::vector<std::string>& p);

private:
    struct postkey {
        uint32_t p;
        uint32_t k;

        postkey() : p(0), k(0) { }
        bool operator<(const postkey& rhs) const { return p < rhs.p; }
        bool operator<(const uint32_t& rhs) const { return p < rhs; }
    };

    std::vector<postkey> pkmap;  // Postcode codes mapping to key codes
};
