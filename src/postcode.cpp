#include "postcode.h"
#include "code.h"
#include <algorithm>
#include <Rcpp.h>
using namespace std;
using namespace Rcpp;

PostcodeMap::PostcodeMap(StringVector& p, StringVector& k)
{
    if (p.length() != k.length())
        stop("PostcodeMap constructor: p (%d) and k (%d) must have same length.", p.length(), k.length());

    pkmap.assign(p.length(), postkey());

    for (unsigned int i = 0; i < p.length(); ++i)
    {
        // TODO make map_postcode and map_ONS function based on String, not string.
        string ps = as<string>(p[i]);
        string ks = as<string>(k[i]);
        pkmap[i].p = map_postcode(ps);
        pkmap[i].k = map_ONS(ks);
    }

    sort(pkmap.begin(), pkmap.end());
}

vector<uint32_t> PostcodeMap::Key(vector<string>& p)
{
    vector<uint32_t> k(p.size(), 0);
    for (unsigned int i = 0; i < p.size(); ++i)
    {
        auto b = map_postcode(p[i]);
        auto l = lower_bound(pkmap.begin(), pkmap.end(), b);
        if (l == pkmap.end()) {
            stop("No mapping for postcode %s.", p[i]);
        } else {
            k[i] = pkmap[l - pkmap.begin()].k;
        }
    }
    return k;
}
