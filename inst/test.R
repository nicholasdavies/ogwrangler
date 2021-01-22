# Test ogwrangler
library(ggplot2)
library(ogwrangler)

CreateCache()

ogcode("*", "gmlad")

ggplot(code_names[geogcd %like% "^X60", .(geognm, area = ogwhat(geogcd, "area"), pop = ogwhat(geogcd, "pop2019"))]) +
    geom_point(aes(x = area, y = pop)) +
    geom_text(aes(x = area, y = pop, label = geognm))

ggplot(code_names[geogcd %like% "^X61", .(geognm, area = ogwhat(geogcd, "area"), pop = ogwhat(geogcd, "pop2019"))]) +
    geom_point(aes(x = area, y = pop)) +
    geom_text(aes(x = area, y = pop, label = geognm))



View(master[geognm %like% "Penryn"])
View(master[geognm %like% "Falmouth"])





#usethis::use_data(og_coderef, og_entityref, og_metrics, og_lookup, overwrite = TRUE, internal = TRUE);

lookup

missing
View(missing)

b = setdiff(master[geogcd %like% "^[A-Z]00" & status == "live", geogcd], onspd[, unique(oa11)]);
b

TestPostcode("WC1E7HT")
TestMap("E08000165")
TestMap("ZB001")
TestMap("95AA99C4")

# exploring the postcode directories

# National Statistics Postcode Lookup
nspl = fread("~/Downloads/NSPL_AUG_2020_UK/Data/NSPL_AUG_2020_UK.csv")
nspl

# NSPCL
nspcl = fread("./data-raw/NSPCL_NOV20_UK_LU.csv")
nspcl

# Postcode to output area hierarchy lookup
pcoa = fread("./data-raw/PCD_OA_LSOA_MSOA_LAD_NOV20_UK_LU.csv")
pcoa

# output area centroids, updated July 2020
# do not include NI or Scotland.
cent = fread("./data-raw/Output_Areas__December_2011__Population_Weighted_Centroids.csv")
cent[OA11CD %like% "S"]

oa = master[geogcd %like% "^[ESNW]00" & status == "live", unique(geogcd)]
oa2 = onspd[, unique(oa11)]
oa2 = nspl[, unique(oa11)]
oa2 = onspd0516[, unique(oa11)]
oa2 = nspcl[, unique(oa11cd)]
oa2 = pcoa[is.na(doterm), unique(oa11cd)]
oa2 = metrics[, unique(code)]
oa2 = cent[, unique(OA11CD)]
setdiff(oa, oa2)


onspd0516 = fread("~/Downloads/ONSPD_MAY_2016_UK/ONSPD_MAY_2016_UK.csv")
rm(onspd0516)

oaccg1 = onspd[, unique(data.table(oa11, ccg))]
oaccg0 = onspd0516[, unique(data.table(oa11, ccg))]

bbb = merge(oaccg0, oaccg1, by = "oa11", all = T)
bbb[ccg.x != ccg.y]

# ok so for health hierarchies we want
# England: ccg, stp, nhser, country
# Wales: lhb, country
# Scotland: hb, country
# Northern Ireland: lcg, country

onspd[, uniqueN(pcd)]
onspd[, uniqueN(oa11)]
onspd[, uniqueN(oa11)]
onspd[, unique(ccg), by = lsoa11][, lsoa11[duplicated(lsoa11)]]

# NHS Postcode Directory
nhspd = fread("~/Downloads/NHSPD_AUG_2020_UK_FULL/Data/nhg20aug.csv")
# nhspd1 = fread("~/Downloads/NHSPD_AUG_2020_UK_FULL/Data/aug20y63.csv") # extracts of NHSPD

nhspd
rm(nhspd1)
nhspd[, table(V21)]
rm(nhspd)


# Special codes for Northern Ireland
onspd[pcd %like% "^BT"]
onspd[pcd %like% "^BT", unique(statsward)]
sum(onspd[pcd %like% "^BT", sort(unique(oa01))] %like% "^95([A-Z])\\1[0-9]{6}")
onspd[pcd %like% "^BT", sort(unique(casward))] %like% "^95([A-Z])\\1[0-9][0-9]$"

oa01onspd[pcd %like% "^BT", sort(unique(oa01))]

ni_lsoa = setdiff(onspd[pcd %like% "^BT", unique(lsoa11)], "")
sum(ni_lsoa %like% "^95([A-Z])\\1")
onspd[pcd %like% "^BT", unique(oshlthau)]
onspd[pcd %like% "^BT", unique(ccg)]





# area measurements
# AREAEHECT: full extent
# AREACHECT: full extent excluding sea (to coastline)
# AREAIHECT: area of inland water
# AREALHECT: land to coastline, excluding inland water. Recommended by Eurostat for population density measures.

rgc




# testing
library(ogwrangler)
TestMap(9999999999)



# testing postcode lookup
pcl = onspd[, .(pcd, oa11)]
pcl2 = pcl[oa11 != ""]
pcl2 = pcl2[!pcd %like% "NPT"]
rm(pcl)
Test2(pcl2$pcd, pcl2$oa11, pcl2$pcd[1:100])

library(readxl)
excel_sheets("./data-raw/Register_of_Geographic_Codes_(April_2020)_UK/RGC_APR_2020_UK.xlsx")
rgc = read_excel("./data-raw/Register_of_Geographic_Codes_(April_2020)_UK/RGC_APR_2020_UK.xlsx", "E00_OA")
setDT(rgc)

rgc[GEOGNM %like% "[^A-Za-z0-9 ]", unique(GEOGNM)]
rgc[GEOGNM %like% "'", unique(GEOGNM)]
rm(onspd)

rgc

oa_p = onspd[, unique(oa11)]
oa_lp = onspd[is.na(doterm), unique(oa11)]
oa_m = metrics[, unique(code)]
oa_ma = master[status == "live" & geogcd %like% "^(E|N|S|W)00", geogcd]
tail(setdiff(oa_m, oa_ma))


setdiff(oa_m, oa_p)
setdiff(oa_lp, oa_m)

lookup_test = unique(onspd[is.na(doterm), .(oa11, ccg, stp, nhser, oslaua)])
lookup_test = lookup_test[oa11 != ""]
code_names = master[status == "live", .(geogcd, geognm)]
Test3(lookup_test, code_names, metrics)
lookup_test




# test of how a spatial model might work...
library(mgcv)
library(ggplot2)
dt = data.table(x = rep(0:100, each = 101), y = rep(0:100, 101))
dt[, z := sin(x %/% 10) + cos(0.2 * (y %/% 10))]
ggplot(dt) + geom_raster(aes(x = x, y = y, fill = z))

model = gam(z ~ s(x, y), data = dt)
#plot(model)
dt[, z0 := predict(model, newdata = .SD)]
ggplot(dt) + geom_raster(aes(x = x, y = y, fill = z0))



# idea of beta-binomial (later expand to dirichlet-multinomial) spatial model
# suppose there is one geography AB which splits a country into two areas, A and B.
# there is another geography CD which splits the country into two different areas, C and D.
# q measures the overlap, q is the probability that a given point in A is also in C.
# (for now imagine that AB and CD both split the country in half... think about how to generalise later)

# counts in each of A and B, our job is to estimate counts in C and D
A = 1000
B = 0
q = 0.9

# f is Wright's genetic distance between populations (which ones?)
f = 0.333
alpha = q * (1 - f) / f
beta = (1 - q) * (1 - f) / f

# or, v is sample size
v = 10000
alpha = q * v
beta = (1 - q) * v

# distribution of possible values for C...
samples = 10000
ps = rbeta(samples, alpha, beta)
Cest = rbinom(samples, A, ps) + rbinom(samples, B, 1 - ps)
hist(Cest, breaks = 0:(A+B))

