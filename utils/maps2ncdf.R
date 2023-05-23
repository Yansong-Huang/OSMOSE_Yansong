library(nctools)
library(kali)
library(fields)
file = "fishing/fishing-distrib.csv"
sep = ";"
na.strings = "NA"
x = osmose:::.readthis(x=csv, sep=";", na.strings = NA)

out = as.matrix(read.csv(file, sep=sep, header=FALSE, na.strings = na.strings))
out = osmose:::.rotate(out)
image.plot(out)
dim(out) = c(dim(out), 1)
dims = setNames(lapply(dim(out), seq_len), nm=c("x", "y", "time"))

write_ncdf(x=out, varid="fishing_area", filename = "fishing/fishing-distrib.nc",
           dim = dims)

nc = nc_open("fishing/fishing-distrib.nc")

xx = ncvar_get(nc, "fishing_area")

ncg = nc_open("eec_grid-mask.nc")
grid = ncvar_get(ncg, "")
