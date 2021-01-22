// [[Rcpp::plugins(cpp11)]]

#include <vector>
#include <string>
#include <cstdio>
#include <Rcpp.h>
#include <cctype>
#include <string>
#include "postcode.h"
#include "lookup.h"
using namespace Rcpp;
using namespace std;

// Kinds of codes:
// C01a: 12AA34[B5]
// C01b: AB123
// C11:  A12345678

// Map an ONS code string of type C01a/b or C11 to a 32-bit unsigned integer.
uint32_t map_ONS(const string& s)
{
    // Read a letter from position p of s
    auto letter = [&](int p) -> uint32_t
    {
        char c = s[p];
        if (!isalpha(c))
            stop("Expected letter at position %d.", p);
        return (c & 31) - 1;
    };

    // Read digits from positions p0 to p1 inclusive of s
    auto digits = [&](unsigned int p0, unsigned int p1) -> uint32_t
    {
        uint32_t x = 0;
        for (unsigned int p = p0; p <= p1; ++p)
        {
            x *= 10;
            char c = s[p];
            if (!isdigit(c))
                stop("Expected digits at positions %d-%d.", p0, p1);
            x += c - '0';
        }
        return x;
    };

    // Read code
    uint32_t b;
    switch (s.length())
    {
        case 5: // C01b code
            b = 0xffe00000 | (letter(0) << 15) | (letter(1) << 10) | digits(2, 4);
            break;

        case 6: // C01a short code
            if (letter(2) != letter(3))
                stop("Expected letters in positions 2 and 3 to be the same.");
            b = 0xf00003ff | (digits(0, 1) << 21) | (letter(2) << 16) | (digits(4, 5) << 9);
            break;

        case 8: // C01a long code
            if (letter(2) != letter(3))
                stop("Expected letters in positions 2 and 3 to be the same.");
            b = 0xf0000000 | (digits(0, 1) << 21) | (letter(2) << 16) | (digits(4, 5) << 9) | (letter(6) << 4) | digits(7, 7);
            break;

        case 9: // C11 code
            b = (letter(0) << 27) | (digits(1, 2) << 20) | digits(3, 8);
            break;

        default:
            stop("Code not of supported length.");
    }

    return b;
}

// Unmap a mapped ONS code.
string unmap_ONS(uint32_t b)
{
    const uint32_t C01_mask  = 0xF0000000;
    const uint32_t C01b_mask = 0x0FE00000;

    const uint32_t C01a_digits1_mask = 0x0fe00000;
    const uint32_t C01a_digits1_shift = 21;
    const uint32_t C01a_letter2_mask = 0x001f0000;
    const uint32_t C01a_letter2_shift = 16;
    const uint32_t C01a_digits3_mask = 0x0000fe00;
    const uint32_t C01a_digits3_shift = 9;
    const uint32_t C01a_letter4_mask = 0x000001f0;
    const uint32_t C01a_letter4_shift = 4;
    const uint32_t C01a_digit5_mask  = 0x0000000f;

    const uint32_t C01b_letter1_mask = 0x000f8000;
    const uint32_t C01b_letter1_shift = 15;
    const uint32_t C01b_letter2_mask = 0x00007c00;
    const uint32_t C01b_letter2_shift = 10;
    const uint32_t C01b_digits3_mask = 0x000003ff;

    const uint32_t C11_letter_mask = 0xF8000000;
    const uint32_t C11_letter_shift = 27;
    const uint32_t C11_level_mask = 0x07F00000;
    const uint32_t C11_level_shift = 20;
    const uint32_t C11_unit_mask = 0x000FFFFF;

    auto validate_letter = [](char c)
    {
        if (c < 'A' || c > 'Z')
            stop("Malformed letter.");
    };

    auto validate_digits = [](int d, int max)
    {
        if (d < 0 || d > max)
            stop("Malformed digits.");
    };

    string s;

    // C01 code
    if ((b & C01_mask) == C01_mask)
    {
        // C01b code
        if ((b & C01b_mask) == C01b_mask)
        {
            char letter1 = 'A' + ((b & C01b_letter1_mask) >> C01b_letter1_shift);
            char letter2 = 'A' + ((b & C01b_letter2_mask) >> C01b_letter2_shift);
            int digits = b & C01b_digits3_mask;

            validate_letter(letter1);
            validate_letter(letter2);
            validate_digits(digits, 999);

            s.assign(6, ' ');
            snprintf(&s[0], 6, "%c%c%03d", letter1, letter2, digits);
        }
        // C01a code
        else
        {
            int digits1 = (b & C01a_digits1_mask) >> C01a_digits1_shift;
            char letter2 = 'A' + ((b & C01a_letter2_mask) >> C01a_letter2_shift);
            int digits3 = (b & C01a_digits3_mask) >> C01a_digits3_shift;
            char letter4 = 'A' + ((b & C01a_letter4_mask) >> C01a_letter4_shift);
            int digit5 = b & C01a_digit5_mask;

            validate_digits(digits1, 99);
            validate_letter(letter2);
            validate_digits(digits3, 99);

            // C01a long code
            if (isalpha(letter4))
            {
                validate_digits(digit5, 9);
                s.assign(9, ' ');
                snprintf(&s[0], 9, "%02d%c%c%02d%c%d", digits1, letter2, letter2, digits3, letter4, digit5);
            }
            // C01a short code
            else
            {
                s.assign(7, ' ');
                snprintf(&s[0], 7, "%02d%c%c%02d", digits1, letter2, letter2, digits3);
            }
        }
    }
    // C11 code
    else
    {
        char letter = 'A' + ((b & C11_letter_mask) >> C11_letter_shift);
        int level = (b & C11_level_mask) >> C11_level_shift;
        int unit = b & C11_unit_mask;

        validate_letter(letter);
        validate_digits(level, 99);
        validate_digits(unit, 999999);

        s.assign(10, ' ');
        snprintf(&s[0], 10, "%c%02d%06d", letter, level, unit);
    }

    // Remove null character
    s.resize(s.length() - 1);

    return s;
}

// Map a postcode to a 32-bit unsigned integer.
uint32_t map_postcode(const string& s)
{
    // Read a letter from position p of s
    auto letter = [&](int p) -> uint32_t
    {
        char c = s[p];
        if (!isalpha(c))
            stop("Expected letter at position %d (%s).", p, s.c_str());
        return (c & 31) - 1;
    };

    // Read digit from position p of s
    auto digit = [&](unsigned int p) -> uint32_t
    {
        char c = s[p];
        if (!isdigit(c))
            stop("Expected digits at position %d (%s).", p, s.c_str());
        return c - '0';
    };
    // area -- letters up to first number (A or AA)
    // district -- first number up to end minus 3 (9 or 9A or 99)
    // sector + unit -- last 3 characters (9AA)

    uint32_t area, district, secunit;

    // Checks
    size_t len = s.length();
    if (len < 5)
        stop("Postcode too short.");

    // Read area
    bool area2 = isalpha(s[1]);
    if (area2)
        area = letter(0) * 26 + letter(1);
    else
        area = 26 * 26 + letter(0);

    // Read district
    if (isblank(s[2 + area2]) || 2 + area2 >= len - 3)
        district = digit(1 + area2) * 37 + 36;
    else
        district = digit(1 + area2) * 37 + (isdigit(s[2 + area2]) ? digit(2 + area2) : letter(2 + area2) + 10);

    // Read sector and unit
    secunit = digit(len - 3) * 26 * 26 + letter(len - 2) * 26 + letter(len - 1);

    return (area << 22) | (district << 13) | secunit;
}

// Unmap a mapped potcode.
string unmap_postcode(uint32_t b)
{
    const uint32_t area_mask     = 0xffc00000;
    const uint32_t area_shift     = 22;
    const uint32_t district_mask = 0x003fe000;
    const uint32_t district_shift = 13;
    const uint32_t secunit_mask  = 0x00001fff;

    // Extract large scale components
    uint32_t area = (b & area_mask) >> area_shift;
    uint32_t district = (b & district_mask) >> district_shift;
    uint32_t secunit = b & secunit_mask;

    // Extract individual components
    uint32_t a1 = area / 26;
    uint32_t a2 = area % 26;
    uint32_t d1 = district / 37;
    uint32_t d2 = district % 37;
    uint32_t s1 = secunit / (26 * 26);
    uint32_t u1 = (secunit % (26 * 26)) / 26;
    uint32_t u2 = secunit % 26;

    if (a1 >= 27) stop("Malformed area.");
    if (d1 >= 36) stop("Malformed district.");
    if (s1 >= 10) stop("Malformed sector.");

    // Reform post code string
    string s(6 + (a1 != 26) + (d2 != 36), ' ');
    size_t p = 0;

    if (a1 != 26)
        s[p++] = 'A' + a1;
    s[p++] = 'A' + a2;
    s[p++] = '0' + d1;
    if (d2 < 10)
        s[p++] = '0' + d2;
    else if (d2 < 36)
        s[p++] = 'A' + d2 - 10;
    ++p; // Space
    s[p++] = '0' + s1;
    s[p++] = 'A' + u1;
    s[p++] = 'A' + u2;

    return s;
}

// Name matching
// Tests if string query matches string text, using the following rules:
// Character in text                 Match in query
// -----------------                 --------------
// Letter                            Same letter (case insensitive)
// Digit                             Same digit
// Apostrophe                        Apostrophe or missing
// "and" or "&"                      "and" (whole word only) or "&"
// Any other consecutive punctuation Any other consecutive punctuation or lowercase-to-uppercase/digit word boundary
bool name_match(const string& query, const string& text)
{
    auto q = query.begin(), t = text.begin();
    char qc = '\0', tc = '\0';
    while (q != query.end() && t != text.end())
    {
        bool q_prev_islower = islower(qc);

        qc = *q;
        tc = *t;

        // Letter or digit in text: match to same letter (case-insensitive) or digit, or to & if letter starts the word 'and'
        if (isalnum(tc))
        {
            if (tolower(qc) == tolower(tc)) {
                ++t;
                ++q;
            } else if (qc == '&' && (t == text.begin() || !isalpha(*(t-1))) &&
                        t < text.end() - 2 && tolower(tc) == 'a' && tolower(*(t+1)) == 'n' && tolower(*(t+2)) == 'd') {
                t += 3;
                ++q;
            } else {
                break;
            }
        }
        // Apostrophe in text: match to apostrophe or nothing
        else if (tc == '\'')
        {
            ++t;
            if (qc == '\'')
                ++q;
        }
        // Ampersand in text: match to ampersand or "and"
        else if (tc == '&')
        {
            if (qc == '&') {
                ++t;
                ++q;
            } else if (tolower(qc) == 'a' && q < query.end() - 2 && tolower(*(q+1)) == 'n' && tolower(*(q+1)) == 'd') {
                ++t;
                q += 3;
            } else {
                break;
            }
        }
        // Any other punctuation in text: match all consecutive punctuation characters (not '&) to
        // consecutive punctuation characters (not '&), or lowercase-uppercase/digit word boundary.
        else if (ispunct(tc) || isspace(tc))
        {
            if (q_prev_islower && (isupper(qc) || isdigit(qc))) {
                do tc = *++t; while (t != text.end() && (ispunct(tc) || isspace(tc)) && tc != '\'' && tc != '&');
                continue;
            } else if ((ispunct(qc) || isspace(qc)) && qc != '\'' && qc != '&') {
                do tc = *++t; while (t != text.end()  && (ispunct(tc) || isspace(tc)) && tc != '\'' && tc != '&');
                do qc = *++q; while (q != query.end() && (ispunct(qc) || isspace(qc)) && qc != '\'' && qc != '&');
            } else {
                break;
            }
        }
        else
        {
            stop("Unrecognised character %c in text string %s", tc, text.c_str());
        }
    }

    if (q == query.end())
        while (t != text.end() && (ispunct(*t) || isspace(*t)))
            ++t;

    return q == query.end() && t == text.end();
}


// //' Map string to id
// //' @export
// // [[Rcpp::export]]
// void TestMap(String str)
// {
//     string s = str;
//
//     Rcout << s << "\n";
//
//     uint32_t b = map_ONS(s);
//     Rcout << b << "\n";
//
//     string ss = unmap_ONS(b);
//     Rcout << ss << "\n";
// }

// //' Map postcode to id
// //' @export
// // [[Rcpp::export]]
// void TestPostcode(String pc)
// {
//     string p = pc;
//
//     Rcout << p << "\n";
//
//     uint32_t b = map_postcode(p);
//     Rcout << b << "\n";
//
//     string ss = unmap_postcode(b);
//     Rcout << ss << "\n";
// }

// //' Something else
// //' @export
// // [[Rcpp::export]]
// void Test2(Rcpp::StringVector p, Rcpp::StringVector k, Rcpp::StringVector p2)
// {
//     PostcodeMap pcm(p, k);
//
//     vector<string> v = as<vector<string>>(p2);
//     vector<uint32_t> u = pcm.Key(v);
//     for (unsigned int i = 0; i < u.size(); ++i) {
//         Rcout << v[i] << " " << u[i] << "   ";
//     }
// }


// //' Something else
// //' @export
// // [[Rcpp::export]]
// void Test3(Rcpp::DataFrame d, Rcpp::DataFrame code_names, Rcpp::DataFrame metrics)
// {
//     Lookup lu(d, code_names, metrics);
// }

//' Cache data for use in ogwrangler
//' @export
// [[Rcpp::export]]
void CreateCache()
{
    // Get ogwrangler cache environment
    Environment env = Environment::namespace_env("ogwrangler");
    SEXP ogc = env.get("ogcache");
    Environment cache(ogc);

    // Create new postcode map and lookup from built-in package data
    DataFrame pcl = env.get("pcl");
    DataFrame lookup = env.get("lookup");
    DataFrame code_names = env.get("code_names");
    DataFrame metrics = env.get("metrics");

    StringVector pcl1 = as<StringVector>(pcl["pcd"]);
    StringVector pcl2 = as<StringVector>(pcl["sbb11"]);

    XPtr<PostcodeMap> pcm(new PostcodeMap(pcl1, pcl2), true);
    XPtr<Lookup> lu(new Lookup(lookup, code_names, metrics), true);

    cache["pcm"] = pcm;
    cache["lu"] = lu;
}

// Get postcode map
PostcodeMap* GetPostcodeMap()
{
    // Get ogwrangler cache environment
    Environment env = Environment::namespace_env("ogwrangler");
    SEXP ogc = env.get("ogcache");
    Environment cache(ogc);

    // Look for postcode map
    auto pcm = cache.get("pcm");
    if (pcm == R_NilValue)
        stop("No postcode map found.");

    return as<XPtr<PostcodeMap>>(pcm);
}

// Get lookup
Lookup* GetLookup()
{
    // Get ogwrangler cache environment
    Environment env = Environment::namespace_env("ogwrangler");
    SEXP ogc = env.get("ogcache");
    Environment cache(ogc);

    // Look for lookup
    auto lu = cache.get("lu");
    if (lu == R_NilValue)
        stop("No lookup found.");

    return as<XPtr<Lookup>>(lu);
}

//' Get code from geo for each postcode
//' @export
// [[Rcpp::export]]
StringVector ogpost(StringVector postcode, StringVector geo)
{
    PostcodeMap* pcm = GetPostcodeMap();
    Lookup* lu = GetLookup();

    // Get keys associated with each postcode
    auto vpostcode = as<vector<string>>(postcode);
    auto keys = pcm->Key(vpostcode);

    // Create output vector
    size_t length_out = max(postcode.length(), geo.length());
    StringVector out(length_out, "");

    // Map each postcode to a geography
    if (postcode.length() > 0 && geo.length() > 0)
    {
        for (size_t i = 0, p = 0, g = 0; i < length_out; ++i, ++p, ++g)
        {
            if (p == postcode.length()) p = 0;
            if (g == geo.length()) g = 0;

            out[i] = unmap_ONS(lu->LookUpKey(keys[p], as<string>(geo[g])));
        }
    }

    return out;
}

//' Find text in names of geographies
//' @export
// [[Rcpp::export]]
StringVector ogfind(StringVector text)
{
    Lookup* lu = GetLookup();
    regex r(as<string>(text[0]), regex_constants::icase);
    return lu->FindName(r);
}

//' Find named entities in geography geo
//' @export
// [[Rcpp::export]]
StringVector ogcode(StringVector name, StringVector geo)
{
    Lookup* lu = GetLookup();

    // Get all codes for requested geos
    if (name.length() == 1 && name[0] == "*")
    {
        vector<String> codes;
        for (size_t g = 0; g < geo.length(); ++g)
        {
            vector<String> c = lu->GetCodes(as<string>(geo[g]));
            codes.insert(codes.end(), c.begin(), c.end());
        }
        return StringVector(codes.begin(), codes.end());
    }

    // Create output vector
    size_t length_out = max(name.length(), geo.length());
    StringVector out(length_out, "");

    // Map each name to a code
    if (name.length() > 0 && geo.length() > 0)
    {
        for (size_t i = 0, n = 0, g = 0; i < length_out; ++i, ++n, ++g)
        {
            if (n == name.length()) n = 0;
            if (g == geo.length()) g = 0;

            out[i] = lu->MatchName(as<string>(name[n]), as<string>(geo[g]));
        }
    }

    return out;
}

//' Get property of a code
//' @export
// [[Rcpp::export]]
SEXP ogwhat(StringVector code, String property = "name")
{
    Lookup* lu = GetLookup();
    return lu->GetProperties(code, property);
}

