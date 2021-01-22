// [[Rcpp::plugins(cpp11)]]

#include <string>

// Map an ONS code string of type C01a/b or C11 to a 32-bit unsigned integer.
uint32_t map_ONS(const std::string& s);

// Unmap a mapped ONS code.
std::string unmap_ONS(uint32_t b);

// Map a postcode to a 32-bit unsigned integer.
uint32_t map_postcode(const std::string& s);

// Unmap a mapped potcode.
std::string unmap_postcode(uint32_t b);

// Test for match in name
bool name_match(const std::string& query, const std::string& text);
