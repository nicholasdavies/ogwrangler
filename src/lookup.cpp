#include "lookup.h"
#include "code.h"
#include <algorithm>
#include <numeric>
#include <Rcpp.h>
using namespace std;
using namespace Rcpp;

Lookup::Lookup(Rcpp::DataFrame& d, Rcpp::DataFrame& d_code_names, Rcpp::DataFrame& d_metrics)
{
    // Set geo names
    geos = as<vector<string>>(d.names());

    // Set codes and crmap
    codes.assign(d.size(), vector<uint32_t>(d.nrows(), 0));
    crmap.assign(d.size(), vector<coderow>(d.nrows(), coderow()));
    for (unsigned int col = 0; col < d.size(); ++col)
    {
        StringVector s = d[col];
        for (unsigned int row = 0; row < s.size(); ++row)
        {
            // TODO map_ONS on String instead of string
            codes[col][row] = map_ONS(as<string>(s[row]));
            crmap[col][row].c = codes[col][row];
            crmap[col][row].v = row;
        }

        // sort crmap for this column
        sort(crmap[col].begin(), crmap[col].end());
    }

    // Set code names
    names.assign(d_code_names.nrows(), codename());
    StringVector code_col = d_code_names[0];
    StringVector name_col = d_code_names[1];
    for (unsigned int row = 0; row < names.size(); ++row)
    {
        names[row].c = map_ONS(as<string>(code_col[row]));
        names[row].v = as<string>(name_col[row]);
    }
    sort(names.begin(), names.end());

    // Set metrics
    if (d_metrics.nrows() != codes[0].size())
        stop("d and d_metrics must have same number of rows.");
    code_col = d_metrics[0];
    for (unsigned int row = 0; row < code_col.size(); ++row)
        if (codes[0][row] != map_ONS(as<string>(code_col[row])))
            stop("d_metrics not in same row order as d.");

    metric_names = as<vector<string>>(d_metrics.names());
    metric_names.erase(metric_names.begin());

    for (unsigned int col = 1; col < d_metrics.size(); ++col)
        metrics.push_back(as<vector<double>>(d_metrics[col]));
}

uint32_t Lookup::LookUpKey(uint32_t key, const string& geo)
{
    // Find geo column to index
    auto column = find(geos.begin(), geos.end(), geo);
    if (column == geos.end())
        stop("Could not find geo %s in lookup.", geo.c_str());

    // Find lookup row
    auto row = lower_bound(crmap[0].begin(), crmap[0].end(), key);
    if (row == crmap[0].end() || row->c != key)
        stop("Could not find key %d in lookup.", key);

    // Return requested code
    return codes[column - geos.begin()][row - crmap[0].begin()];
}

StringVector Lookup::FindName(const regex& r)
{
    StringVector results;
    for (auto n = names.begin(); n != names.end(); ++n)
        if (regex_search(n->v, r))
            results.push_back(unmap_ONS(n->c));

    return results;
}

vector<String> Lookup::GetCodes(const std::string& geo)
{
    // Locate requested geography
    auto geo_it = find(geos.begin(), geos.end(), geo);
    if (geo_it == geos.end())
        stop("Could not find geo %s.", geo.c_str());
    size_t geo_i = geo_it - geos.begin();

    // Get codes
    // TODO Again, another great reason for having a lookup of codes by geo.
    vector<uint32_t> contents(1, crmap[geo_i][0].c);
    for (size_t j = 1; j < crmap[geo_i].size(); ++j)
        if (crmap[geo_i][j].c != contents.back())
            contents.push_back(crmap[geo_i][j].c);

    vector<String> results(contents.size(), "");
    for (size_t j = 0; j < contents.size(); ++j)
        results[j] = unmap_ONS(contents[j]);

    return results;
}

String Lookup::MatchName(const std::string& name, const std::string& geo)
{
    // Locate requested geography
    auto geo_it = find(geos.begin(), geos.end(), geo);
    if (geo_it == geos.end())
        stop("Could not find geo %s.", geo.c_str());
    size_t geo_i = geo_it - geos.begin();

    for (auto n = names.begin(); n != names.end(); ++n)
    {
        // TODO this is horribly inefficient.
        // Should be storing names by geo rather than all in one vector.
        if (name_match(n->v, name))
        {
            auto x = lower_bound(crmap[geo_i].begin(), crmap[geo_i].end(), n->c);
            if (x != crmap[geo_i].end() && x->c == n->c)
                return unmap_ONS(n->c);
        }
    }

    stop("Could not find match for name %s in geo %s.", name.c_str(), geo.c_str());

    return String();
}

StringVector Lookup::GetNames(const vector<uint32_t>& mapped_codes)
{
    // TODO overhaul nomenclature of mapped_codes argument versus codes class member.
    // TODO class member should not be called codes.
    StringVector results(mapped_codes.size(), "");

    for (unsigned int i = 0; i < mapped_codes.size(); ++i)
    {
        auto x = lower_bound(names.begin(), names.end(), mapped_codes[i]);
        if (x == names.end() || x->c != mapped_codes[i])
            stop("Could not find code %d in lookup.", mapped_codes[i]);
        results[i] = x->v;
    }

    return results;
}

NumericVector Lookup::GetMetrics(const vector<uint32_t>& mapped_codes, const String& metric)
{
    NumericVector results(mapped_codes.size(), 0);

    // Locate requested metric
    string metric_str = metric;
    auto metric_it = find(metric_names.begin(), metric_names.end(), metric_str);
    if (metric_it == metric_names.end())
        stop("Could not find metric %s.", metric_str.c_str());
    size_t metric_i = metric_it - metric_names.begin();

    // Accumulate metrics for all codes
    // TODO make more efficient?
    for (unsigned int i = 0; i < mapped_codes.size(); ++i)
    {
        for (unsigned int j = 0; j < geos.size(); ++j)
        {
            auto range = equal_range(crmap[j].begin(), crmap[j].end(), mapped_codes[i]);
            if (range.first < range.second)
            {
                results[i] = accumulate(metrics[metric_i].begin() + (range.first - crmap[j].begin()),
                    metrics[metric_i].begin() + (range.second - crmap[j].begin()), 0.0);
                break;
            }

            if (j == geos.size() - 1)
                stop("Could not find code %s in lookup.", unmap_ONS(mapped_codes[i]));
        }
    }

    return results;
}

StringVector Lookup::TranslateGeo(const std::vector<uint32_t>& mapped_codes, const String& geo_to)
{
    StringVector results(mapped_codes.size(), "");

    // Locate requested geography
    string geo_str = geo_to;
    auto geo_it = find(geos.begin(), geos.end(), geo_str);
    if (geo_it == geos.end())
        stop("Could not find geo %s.", geo_str.c_str());
    size_t geo_i = geo_it - geos.begin();

    // Translate codes
    // TODO make more efficient
    for (unsigned int i = 0; i < mapped_codes.size(); ++i)
    {
        for (unsigned int j = 0; j < geos.size(); ++j)
        {
            auto range = equal_range(crmap[j].begin(), crmap[j].end(), mapped_codes[i]);
            if (range.first < range.second)
            {
                results[i] = unmap_ONS(codes[geo_i][range.first->v]);
                break;
            }

            if (j == geos.size() - 1)
                stop("Could not find code %s in lookup.", unmap_ONS(mapped_codes[i]));
        }
    }

    return results;
}

SEXP Lookup::GetProperties(const StringVector& code, const String& property)
{
    // Map codes
    vector<uint32_t> mapped_codes(code.size(), 0);
    for (unsigned int i = 0; i < code.size(); ++i)
        mapped_codes[i] = map_ONS(as<string>(code[i]));
    string property_str = property;

    if (property_str == "name")
        return wrap(GetNames(mapped_codes));
    else if (find(metric_names.begin(), metric_names.end(), property_str) != metric_names.end())
        return wrap(GetMetrics(mapped_codes, property));
    else if (find(geos.begin(), geos.end(), property_str) != geos.end())
        return wrap(TranslateGeo(mapped_codes, property));

    stop("Could not find requested property %s.", property_str.c_str());
    return wrap(R_NilValue);
}
