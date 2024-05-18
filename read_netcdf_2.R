library(ncdf4)
thornback_ray <- nc_open("osmose-eec_v4.4_yansong/input/maps_nc/thornbackRay.nc")
real_map <- ncvar_get(thornback_ray, varid = "stage0")
test_map <- ncvar_get(thornback_ray, varid = "stage0")-1
ncvar_put(thornback_ray,test_map)

dimLongitude <- ncdim_def( "longitude", "degree", seq(-1.95,2.45,0.1))
dimLatitude <- ncdim_def( "latitude", "degree", seq(49.05,51.15,0.1))
dimTime <- ncdim_def( "time", "15 days", seq(1,48,1), unlim=TRUE,calendar = "360_day")

var_1 <- ncvar_def("test","null", list(dimLongitude,dimLatitude,dimTime), missval=NULL, prec="double")

nc_test <- nc_create("test_2.nc",var_1)
# from year 1 to year 10, use 1 to fill the map
for (i in 1:10){
  ncvar_put(nc_test,var_1,real_map, start = c(1,1,24*(i-1)+1), count = c(-1,-1,24))   
}
# from year 1 to year 10, use 0 to fill the map
for (j in 1:10){
  ncvar_put(nc_test,var_1,test_map, start = c(1,1,24*(j-1)+241), count = c(-1,-1,24))  
}                 
 

results_test <- ncvar_get(nc_test, varid = "test")
nc_close(nc_test)
