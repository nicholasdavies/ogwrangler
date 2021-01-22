#' @useDynLib ogwrangler
#' @importFrom Rcpp sourceCpp
NULL

# terminology
# entity is the general class of code e.g. E01 is an entity with name LSOA
# instance is the specific code within an entity e.g. E01000001	is an instance with the name "City of London 001A"
# geography code is the general name for the 9-character codes
# geography name is the name corresponding with the code
# note, H codes are organisation data service codes (for health and social care)

### Helper functions ###

# For each element of x, return x[i] if x[i] is a valid R variable name, otherwise return paste0("V", i)
as_var_name = function(x)
{
    ifelse(stringr::str_detect(x, "^([a-zA-Z][a-zA-Z0-9._]*|\\.|\\.[a-zA-Z._][a-zA-Z0-9._]*)$"), x, paste0("V", seq_along(x)))
}

### Package cache ###
ogcache = new.env();

#' ### Wrangling functions ###
#'
#' # TODO proportion should be rate
#'
#' #' Transform count or proportion data from one set of geography codes to another
#' #'
#' #' This function takes data that are mapped to one set of geography codes and redistributes
#' #' those data to another set of geography codes, proportionally to either the population
#' #' or the surface area of the regions referred to by the geography codes.
#' #'
#' #' \code{from} and \code{to} can either be a single string specifying the entity being mapped
#' #' from/to, or a named character vector where the name specifies a regex for matching a subset
#' #' of the geography codes in \code{key} and the value specifies the entity to map the codes in
#' #' \code{from}.
#' #'
#' #' @param x numerical data to wrangle
#' #' @param key code associated with each element of x
#' #' @param from rules for what to interpret codes in key as
#' #' @param to rules for what to translate codes in key to
#' #' @param by unit over which to distribute parts of x
#' #' @param measure "count" if x is count data, "proportion" if x is proportion data
#' #' @return Transformed data
#' #'
#' #' @examples
#' #'
#' #' # Map prevalence data from English admin
#' #' # regions to NHS England regions
#' #' data = fread(
#' #' "RGN19CD	prev
#' #' E12000001	4.6
#' #' E12000002	4.9
#' #' E12000003	3.2
#' #' E12000004	4.2
#' #' E12000005	5.1
#' #' E12000006	6.2
#' #' E12000007	11.9
#' #' E12000008	4.8
#' #' E12000009	2.3")
#' #' data[, ogwrangle(prev, RGN19CD,
#' #'                  "e.reg", "e.nhser20",
#' #'                  "pop2018", "proportion")];
#' #'
#' #' # Map data from countries to regional health boards.
#' #' # This only provides a "best guess" to count data
#' #' # within these smaller regions, since of course
#' #' # the actual distribution of these count data
#' #' # within countries is not known!
#' #' data = fread(
#' #' "country    n
#' #' E92000001   1000
#' #' N92000002   100
#' #' S92000003   250
#' #' W92000004   200")
#' #' data[, ogwrangle(
#' #'           n, country, "country",
#' #'           c("E" = "e.nhser20", "N" = "n.hsct",
#' #'             "S" = "s.hb", "W" = "w.lhb"),
#' #'           "pop2018", "count")];
#' #'
#' #' @export
#' ogwrangle = function(x, key, from, to, by = "pop2018", measure = "count")
#' {
#'     # TODO
#'     # Check for duplicates in key.
#'     # Check for different levels in key (output by level?)
#'     # Auto-detect from.
#'     # Verbose mode.
#'     # Option for output by names, output by codes, or both.
#'     # More options for by -- projected pops for 2019 and 2020?
#'
#'     # Name for result column
#'     name = as_var_name(deparse(substitute(x)));
#'
#'     # Create data.table for working
#'     work = data.table(x = x, a = key);
#'
#'     # Attach middle-key (b)
#'     if (is.null(names(from))) {
#'         work = merge(work, og_lookup[, .(a = get(from), b = code)], by = "a");
#'     } else {
#'         look2 = NULL;
#'         for (i in seq_along(from)) {
#'             look2 = rbind(look2, og_lookup[get(from[i]) %like% names(from)[i], .(a = get(from[i]), b = code)]);
#'         }
#'         work = merge(work, look2[, .(a, b)], by = "a");
#'     }
#'
#'     # Attach to-key (c)
#'     if (is.null(names(to))) {
#'         work = merge(work, og_lookup[, .(b = code, c = get(to))], by = "b");
#'     } else {
#'         look2 = NULL;
#'         for (i in seq_along(to)) {
#'             look2 = rbind(look2, og_lookup[get(from) %like% names(to)[i], .(b = code, c = get(to[i]))]);
#'         }
#'         work = merge(work, look2[!is.na(b), .(b, c)], by = "b");
#'     }
#'
#'     # Attach middle-weight (w), from-weight (W)
#'     work = merge(work, og_metrics[, .(b = code, w = get(by))], by = "b");
#'     work = merge(work, work[, .(W = sum(w)), by = a], by = "a");
#'
#'     if (measure == "count") {
#'         # Redistribute x by relative middle-weight
#'         work = work[, x * w / W, by = c][, sum(V1), by = c];
#'     } else if (measure == "proportion") {
#'         # If using proportion instead of count
#'         work = work[, weighted.mean(x, w), by = c];
#'     } else {
#'         stop("measure must be either count or proportion.");
#'     }
#'
#'     # Set names and return
#'     if (length(to) == 1) {
#'         setnames(work, c(to, name));
#'     } else {
#'         setnames(work, c("code", name));
#'     }
#'
#'     work
#' }
#'
#' #' Get measure data for a set of geographical entities
#' #'
#' #' @param key code for measures; pass "*" for all
#' #' @param from rules for what to interpret codes in key as
#' #' @param metric metric to return
#' #'
#' #' @examples
#' #'
#' #' # Get population for English admin regions
#' #' ogmetric("*", "e.reg", "pop2018")
#' #' @export
#' ogmetric = function(key, from, metric = "pop2018")
#' {
#'     # Get LSOA codes needed
#'     if (length(key) == 1 && key == "*") {
#'         codes = og_lookup[!is.na(get(from)), code];
#'     } else {
#'         codes = og_lookup[get(from) %in% key, code];
#'     }
#'
#'     # Assemble metrics
#'     m = og_metrics[code %in% codes, .(code, get(metric))];
#'     names(m)[2] = metric;
#'
#'     # Assemble and return results
#'     m = merge(m, og_lookup[, .(code, amalgamation = get(from))], by = "code");
#'     m = m[, sum(get(metric)), keyby = amalgamation];
#'     names(m) = c(from, metric);
#'
#'     m
#' }
#'
#' ### Functions operating on names ###
#'
#' #' Search for a name in the master reference list
#' #' @export
#' ogfind = function(name, nresults = 10)
#' {
#'     indices = og_coderef[, .(.I, distance = stringdist::stringdist(str_to_lower(name), str_to_lower(geognm), method = "cosine"))][frank(distance, ties.method = "min") <= nresults][order(distance)]$I;
#'     return (og_coderef[indices])
#' }
#'
#' #' Translate instance name to instance code
#' #'
#' #' Not implemented: use ogfind instead.
#' #'
#' #' @export
#' ogcode = function(name, entity = NULL, language = "english")
#' {
#'     stop("Not implemented");
#'     # TODO if entity is null, give possible types for name
#'     # TODO if neither is null, translate name to type specified by to
#' }
#'
#' ### Functions operating on codes ###
#'
#' #' describe code
#' #' @export
#' ogwhat = function(code)
#' {
#'     # TODO this is garbage. needs to be descriptive.
#'     ifelse(nchar(code) == 9, og_coderef[code, .(code = geogcd, name = geogname)], og_entityref[code, .(code = entitycd, name = entitynm)])
#' }
#'
#' #' translates code into english or welsh; if suffix true append some kinda descriptor
#' #' @export
#' ogname = function(code, language = "english", suffix = F)
#' {
#'     if (suffix) {
#'         if (language == "english") {
#'             return (og_coderef[code, paste(geognm, og_entityref[str_sub(code, 1, 3), entityabv])])
#'         } else if (language == "welsh") {
#'             return (og_coderef[code, paste(ifelse(is.na(geognmw), geognm, geognmw), og_entityref[str_sub(code, 1, 3), entityabv])])
#'         } else {
#'             stop('Languages supported are "english" or "welsh".');
#'         }
#'     }
#'
#'     if (language == "english") {
#'         return (og_coderef[code, geognm])
#'     } else if (language == "welsh") {
#'         return (og_coderef[code, ifelse(is.na(geognmw), geognm, geognmw)])
#'     } else {
#'         stop('Languages supported are "english" or "welsh".');
#'     }
#' }
