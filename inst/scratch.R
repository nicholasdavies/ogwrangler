library(ogwrangler)

# calculate seroprevalence from REACT-2 by NHS region

data = fread(
"RGN19CD	prev
E12000001	4.6
E12000002	4.9
E12000003	3.2
E12000004	4.2
E12000005	5.1
E12000006	6.2
E12000007	11.9
E12000008	4.8
E12000009	2.3")

data[, ogwrangle(prev, RGN19CD, "e.reg", "e.nhser20", "pop2018")]
data[, ogwrangle(prev, RGN19CD, "e.reg", "e.nhser20", "area")]


data = fread(
"country    n
E92000001   1000
N92000002   100
S92000003   250
W92000004   200")
data[, ogwrangle(n, country, "country", c("E" = "e.nhser20", "N" = "n.hsct", "S" = "s.hb", "W" = "w.lhb"), "pop2018", "count")];



# test of S3
print.foo = function(x)
{
    cat(paste0("The number is ", as.numeric(x), "\n"));
}

x = 2
class(x) = c("foo", "numeric")
print(x)
x
class(x)
