library('ncdf4')
nc_ersem_1 <- nc_open("osmose-eec_v4.4_yansong/input/ERSEM_nc/hist/interpolated_CERES_NorthSea_2d_monthly_hist_2005.nc")
nc_ersem_2 <- nc_open("osmose-eec_v4.4_yansong/input/ERSEM_nc/interpolated_CERESsigma_NorthSea_2d_monthly_hist_2005.nc")
nc_ersem_3 <- nc_open("osmose-eec_v4.4_yansong/input/ERSEM_nc/2000-2020/interpolated_CERES_NorthSea_2d_monthly_rcp85_2006.nc")
nc_eco <- nc_open("osmose-eec_v4.4_yansong/input/eec_ltlbiomassTons.nc")
nc_peru_1 <- nc_open("osmose-eec_v4.4_yansong/input/ERSEM_nc/ROMS-PISCES_climatological_ltl-osmose_humboldt-n_15days_2006_2100.nc")
nc_peru_2 <- nc_open("osmose-eec_v4.4_yansong/input/ERSEM_nc/ROMS-PISCES_historical_ltl-osmose_humboldt-n_15days_1992_2008.nc")

# modifier les noms des fichiers
list_nc <- list.files("osmose-eec_v4.4_yansong/input/ERSEM_nc/rcp585/","interpolated_CERES_NorthSea_2d_monthly\\.rcp85\\.",full.names = TRUE)

for(i in 1:length(list_nc)){
  new_name_nc <- gsub("\\.rcp85\\.","_rcp85_",list_nc[i])
  file.rename(list_nc[i],new_name_nc)
}

# modifier les noms des variables
list_nc_2 <- list.files("osmose-eec_v4.4_yansong/input/ERSEM_nc/rcp585/","interpolated_CERES_NorthSea_2d_monthly_rcp85_",full.names = TRUE)

for(i in 1:length(list_nc_2)){
  nc_file <- nc_open(list_nc_2[i],write = TRUE)
  nc_file <- ncvar_rename(nc_file, "benthic_deposit", "benthicDeposit")
  nc_file <- ncvar_rename(nc_file, "benthic_filter", "benthicFilter")
  nc_file <- ncvar_rename(nc_file, "benthic_meio", "benthicMeio")
}

# combiner les fichiers avec cdo
# list_nc <- list.files("osmose-eec_v4.4_yansong/input/ERSEM_nc/","interpolated_CERES_NorthSea_2d_monthly_hist_.*",full.names = TRUE)
# temp <- tempfile("interpolated_CERES_NorthSea_2d_monthly_hist",fileext=".nc")
# merged_nc <- mergeNC(files=list_nc, outfile=temp)
# terra::rast("interpolated_CERES_NorthSea_2d_monthly_hist.nc") 



######## 2. combiner les fichiers avec ncdf4 #######
library('ncdf4')
list_benthos <- list.files("osmose-eec_v4.4_yansong/input/ERSEM_nc/2000-2020","interpolated_CERES_NorthSea_2d_monthly.*",full.names = TRUE)
list_planktons <- list.files("osmose-eec_v4.4_yansong/input/ERSEM_nc/2000-2020","interpolated_CERESsigma_NorthSea_2d_monthly.*",full.names = TRUE)


# définir les dimensions
dimLongitude <- ncdim_def( "longitude", "degree", seq(-1.95,2.45,0.1))
dimLatitude <- ncdim_def( "latitude", "degree", seq(49.05,51.15,0.1))
dimTime <- ncdim_def( "time", "months since 2000-01-01", seq(1,480,1), unlim=FALSE,calendar = "standard")

# créer les variables
varBenthicDeposit <- ncvar_def("benthicDeposit", "biomass_t", list(dimLongitude,dimLatitude,dimTime), 
                               missval=NULL, prec="double")
varBenthicFilter <- ncvar_def("benthicFilter", "biomass_t", list(dimLongitude,dimLatitude,dimTime), 
                               missval=NULL, prec="double")
varBenthicMeio <- ncvar_def("benthicMeio", "biomass_t", list(dimLongitude,dimLatitude,dimTime), 
                               missval=NULL, prec="double")
varP1c <- ncvar_def("P1c", "biomass_t", list(dimLongitude,dimLatitude,dimTime), 
                               missval=NULL, prec="double")
varP2c <- ncvar_def("P2c", "biomass_t", list(dimLongitude,dimLatitude,dimTime), 
                    missval=NULL, prec="double")
varP3c <- ncvar_def("P3c", "biomass_t", list(dimLongitude,dimLatitude,dimTime), 
                    missval=NULL, prec="double")
varP4c <- ncvar_def("P4c", "biomass_t", list(dimLongitude,dimLatitude,dimTime), 
                    missval=NULL, prec="double")
varZ4c <- ncvar_def("Z4c", "biomass_t", list(dimLongitude,dimLatitude,dimTime), 
                    missval=NULL, prec="double")
varZ5c <- ncvar_def("Z5c", "biomass_t", list(dimLongitude,dimLatitude,dimTime), 
                    missval=NULL, prec="double")
varZ6c <- ncvar_def("Z6c", "biomass_t", list(dimLongitude,dimLatitude,dimTime), 
                    missval=NULL, prec="double")

nc_ersem_combined <-
  nc_create(
    "interpolated_CERES_NorthSea_2d_monthly_2000_2019.nc",
    list(
      varBenthicDeposit,
      varBenthicFilter,
      varBenthicMeio,
      varP1c,
      varP2c,
      varP3c,
      varP4c,
      varZ4c,
      varZ5c,
      varZ6c
    ),
    force_v4 = FALSE
  )

for(i in 1:length(list_benthos)){
  nc_benthos_temp <- nc_open(list_benthos[i])
  nc_planktons_temp <- nc_open(list_planktons[i])
  
  benthicDeposit_temp <- ncvar_get(nc_benthos_temp, varid = "benthicDeposit")
  benthicFilter_temp <- ncvar_get(nc_benthos_temp, varid = "benthicFilter")
  benthicMeio_temp <- ncvar_get(nc_benthos_temp, varid = "benthicMeio")
  P1c_temp <- ncvar_get(nc_planktons_temp, varid = "P1c")
  P2c_temp <- ncvar_get(nc_planktons_temp, varid = "P2c")
  P3c_temp <- ncvar_get(nc_planktons_temp, varid = "P3c")
  P4c_temp <- ncvar_get(nc_planktons_temp, varid = "P4c")
  Z4c_temp <- ncvar_get(nc_planktons_temp, varid = "Z4c")
  Z5c_temp <- ncvar_get(nc_planktons_temp, varid = "Z5c")
  Z6c_temp <- ncvar_get(nc_planktons_temp, varid = "Z6c")
  
  # year i january-june
  ncvar_put(nc_ersem_combined, varBenthicDeposit, benthicDeposit_temp, start=c(1,1,(i*2-2)*12+1), count=c(-1,-1,12))
  ncvar_put(nc_ersem_combined, varBenthicFilter, benthicFilter_temp, start=c(1,1,(i*2-2)*12+1), count=c(-1,-1,12))
  ncvar_put(nc_ersem_combined, varBenthicMeio, benthicMeio_temp, start=c(1,1,(i*2-2)*12+1), count=c(-1,-1,12))
  ncvar_put(nc_ersem_combined, varP1c, P1c_temp, start=c(1,1,(i*2-2)*12+1), count=c(-1,-1,12))
  ncvar_put(nc_ersem_combined, varP2c, P2c_temp, start=c(1,1,(i*2-2)*12+1), count=c(-1,-1,12))
  ncvar_put(nc_ersem_combined, varP3c, P3c_temp, start=c(1,1,(i*2-2)*12+1), count=c(-1,-1,12))
  ncvar_put(nc_ersem_combined, varP4c, P4c_temp, start=c(1,1,(i*2-2)*12+1), count=c(-1,-1,12))
  ncvar_put(nc_ersem_combined, varZ4c, Z4c_temp, start=c(1,1,(i*2-2)*12+1), count=c(-1,-1,12))
  ncvar_put(nc_ersem_combined, varZ4c, Z5c_temp, start=c(1,1,(i*2-2)*12+1), count=c(-1,-1,12))
  ncvar_put(nc_ersem_combined, varZ4c, Z6c_temp, start=c(1,1,(i*2-2)*12+1), count=c(-1,-1,12))
  
  # year i july-december
  ncvar_put(nc_ersem_combined, varBenthicDeposit, benthicDeposit_temp, start=c(1,1,(i*2-1)*12+1), count=c(-1,-1,12))
  ncvar_put(nc_ersem_combined, varBenthicFilter, benthicFilter_temp, start=c(1,1,(i*2-1)*12+1), count=c(-1,-1,12))
  ncvar_put(nc_ersem_combined, varBenthicMeio, benthicMeio_temp, start=c(1,1,(i*2-1)*12+1), count=c(-1,-1,12))
  ncvar_put(nc_ersem_combined, varP1c, P1c_temp, start=c(1,1,(i*2-1)*12+1), count=c(-1,-1,12))
  ncvar_put(nc_ersem_combined, varP2c, P2c_temp, start=c(1,1,(i*2-1)*12+1), count=c(-1,-1,12))
  ncvar_put(nc_ersem_combined, varP3c, P3c_temp, start=c(1,1,(i*2-1)*12+1), count=c(-1,-1,12))
  ncvar_put(nc_ersem_combined, varP4c, P4c_temp, start=c(1,1,(i*2-1)*12+1), count=c(-1,-1,12))
  ncvar_put(nc_ersem_combined, varZ4c, Z4c_temp, start=c(1,1,(i*2-1)*12+1), count=c(-1,-1,12))
  ncvar_put(nc_ersem_combined, varZ4c, Z5c_temp, start=c(1,1,(i*2-1)*12+1), count=c(-1,-1,12))
  ncvar_put(nc_ersem_combined, varZ4c, Z6c_temp, start=c(1,1,(i*2-1)*12+1), count=c(-1,-1,12))
}

nc_close(nc_ersem_combined)
nc_combined <- nc_open("interpolated_CERES_NorthSea_2d_monthly_2000_2019.nc")
benthic_deposit <- ncvar_get(nc_combined, varid = "benthicDeposit")
time_nc <- ncvar_get(nc_combined, varid = "time")













