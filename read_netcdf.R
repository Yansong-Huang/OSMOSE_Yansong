library('ncdf4')
nc_ersem_1 <- nc_open("osmose-eec_v4.4_yansong/input/ERSEM_nc/hist/interpolated_CERES_NorthSea_2d_monthly_hist_2005.nc")
nc_ersem_2 <- nc_open("osmose-eec_v4.4_yansong/input/ERSEM_nc/interpolated_CERESsigma_NorthSea_2d_monthly_hist_2005.nc")
nc_eco <- nc_open("osmose-eec_v4.4_yansong/input/largeBenthos_veryLargeBenthos.nc")

# modifier les noms des fichiers
list_nc <- list.files("osmose-eec_v4.4_yansong/input/ERSEM_nc/2002-2022/","interpolated_CERESsigma_NorthSea_2d_monthly\\.rcp85\\.",full.names = TRUE)

for(i in 1:length(list_nc)){
  new_name_nc <- gsub("\\.rcp85\\.","_rcp85_",list_nc[i])
  file.rename(list_nc[i],new_name_nc)
}

# modifier les noms des variables
list_nc_2 <- list.files("osmose-eec_v4.4_yansong/input/ERSEM_nc/2002-2022/","interpolated_CERES_NorthSea_2d_monthly_rcp85_",full.names = TRUE)

for(i in length(list_nc_2)-1:length(list_nc_2)){
  nc_file <- nc_open(list_nc_2[i],write = TRUE)
  nc_file <- ncvar_rename(nc_file, "benthic_deposit", "benthicDeposit")
  nc_file <- ncvar_rename(nc_file, "benthic_filter", "benthicFilter")
  nc_file <- ncvar_rename(nc_file, "benthic_meio", "benthicMeio")
}


######## 2. combiner les fichiers ERSEM #######
library('ncdf4')
list_benthos <- list.files("osmose-eec_v4.4_yansong/input/ERSEM_nc/2002-2022/","interpolated_CERES_NorthSea_2d_monthly.*",full.names = TRUE)
list_planktons <- list.files("osmose-eec_v4.4_yansong/input/ERSEM_nc/2002-2022/","interpolated_CERESsigma_NorthSea_2d_monthly.*",full.names = TRUE)


# définir les dimensions
dimLongitude <- ncdim_def( "longitude", "degree", seq(-1.95,2.45,0.1))
dimLatitude <- ncdim_def( "latitude", "degree", seq(49.05,51.15,0.1))
dimTime <- ncdim_def( "time", "months since 2002-01-01", seq(1,240,1), unlim=FALSE,calendar = "standard")

# créer les variables
varBenthicDeposit <- ncvar_def("depositBenthos", "carbon_ton", list(dimLongitude,dimLatitude,dimTime), 
                               missval=NULL, prec="double")
varBenthicFilter <- ncvar_def("suspensionBenthos", "carbon_ton", list(dimLongitude,dimLatitude,dimTime), 
                              missval=NULL, prec="double")
varBenthicMeio <- ncvar_def("meioBenthos", "carbon_ton", list(dimLongitude,dimLatitude,dimTime), 
                            missval=NULL, prec="double")
varP1c <- ncvar_def("diatoms", "carbon_ton", list(dimLongitude,dimLatitude,dimTime), 
                    missval=NULL, prec="double")
varP2c <- ncvar_def("nanoPhytoplankton", "carbon_ton", list(dimLongitude,dimLatitude,dimTime), 
                    missval=NULL, prec="double")
varP3c <- ncvar_def("picoPhytoplankton", "carbon_ton", list(dimLongitude,dimLatitude,dimTime), 
                    missval=NULL, prec="double")
varP4c <- ncvar_def("microPhytoplankton", "carbon_ton", list(dimLongitude,dimLatitude,dimTime), 
                    missval=NULL, prec="double")
varZ4c <- ncvar_def("mesoZooplankton", "carbon_ton", list(dimLongitude,dimLatitude,dimTime), 
                    missval=NULL, prec="double")
varZ5c <- ncvar_def("microZooplankton", "carbon_ton", list(dimLongitude,dimLatitude,dimTime), 
                    missval=NULL, prec="double")
varZ6c <- ncvar_def("heterotrophicFlagellates", "carbon_ton", list(dimLongitude,dimLatitude,dimTime), 
                    missval=NULL, prec="double")



nc_ersem_combined <-
  nc_create(
    "interpolated_CERES_NorthSea_2d_monthly_2002_2022.nc",
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

for(i in 1:length(list_benthos)){ # year i
  nc_benthos_temp <- nc_open(list_benthos[i])
  nc_planktons_temp <- nc_open(list_planktons[i])
  
  benthicDeposit_temp <- ncvar_get(nc_benthos_temp, varid = "benthicDeposit")/1e+09 # convert from mg to t
  benthicFilter_temp <- ncvar_get(nc_benthos_temp, varid = "benthicFilter")/1e+09
  benthicMeio_temp <- ncvar_get(nc_benthos_temp, varid = "benthicMeio")/1e+09
  P1c_temp <- ncvar_get(nc_planktons_temp, varid = "P1c")/1e+09 # convert from mg to t
  P2c_temp <- ncvar_get(nc_planktons_temp, varid = "P2c")/1e+09
  P3c_temp <- ncvar_get(nc_planktons_temp, varid = "P3c")/1e+09
  P4c_temp <- ncvar_get(nc_planktons_temp, varid = "P4c")/1e+09
  Z4c_temp <- ncvar_get(nc_planktons_temp, varid = "Z4c")/1e+09
  Z5c_temp <- ncvar_get(nc_planktons_temp, varid = "Z5c")/1e+09
  Z6c_temp <- ncvar_get(nc_planktons_temp, varid = "Z6c")/1e+09
  
  for(j in 1:12){# month j of year i
    ncvar_put(nc_ersem_combined, varBenthicDeposit, benthicDeposit_temp[,,j], start=c(1,1,(i-1)*12+j), count=c(-1,-1,1))
    ncvar_put(nc_ersem_combined, varBenthicFilter, benthicFilter_temp[,,j], start=c(1,1,(i-1)*12+j), count=c(-1,-1,1))
    ncvar_put(nc_ersem_combined, varBenthicMeio, benthicMeio_temp[,,j], start=c(1,1,(i-1)*12+j), count=c(-1,-1,1))
    ncvar_put(nc_ersem_combined, varP1c, P1c_temp[,,j], start=c(1,1,(i-1)*12+j), count=c(-1,-1,1))
    ncvar_put(nc_ersem_combined, varP2c, P2c_temp[,,j], start=c(1,1,(i-1)*12+j), count=c(-1,-1,1))
    ncvar_put(nc_ersem_combined, varP3c, P3c_temp[,,j], start=c(1,1,(i-1)*12+j), count=c(-1,-1,1))
    ncvar_put(nc_ersem_combined, varP4c, P4c_temp[,,j], start=c(1,1,(i-1)*12+j), count=c(-1,-1,1))
    ncvar_put(nc_ersem_combined, varZ4c, Z4c_temp[,,j], start=c(1,1,(i-1)*12+j), count=c(-1,-1,1))
    ncvar_put(nc_ersem_combined, varZ5c, Z5c_temp[,,j], start=c(1,1,(i-1)*12+j), count=c(-1,-1,1))
    ncvar_put(nc_ersem_combined, varZ6c, Z6c_temp[,,j], start=c(1,1,(i-1)*12+j), count=c(-1,-1,1))
      } 
}

nc_close(nc_ersem_combined)
nc_combined <- nc_open("interpolated_CERES_NorthSea_2d_monthly_2002_2022.nc")

###### 3. vérifier les masques ######
require(ncdf4)
nc_eco <- nc_open("osmose-eec_v4.4_yansong/input/largeBenthos_veryLargeBenthos.nc")
nc_ERSEM <- nc_open("osmose-eec_v4.4_yansong/input/ERSEM_nc/interpolated_CERES_NorthSea_2d_monthly_2002_2022.nc")

large_benthos <- ncvar_get(nc_eco, varid = "largeBenthos")
benthic_deposit <- ncvar_get(nc_ERSEM, varid = "depositBenthos")

mask_eco <- ifelse(is.na(large_benthos[,,1]),0,1)
mask_ERSEM <- ifelse(is.na(benthic_deposit[,,1]),0,1)
mask_comparison <- mask_eco - mask_ERSEM
sum(mask_comparison)


###### 4.adapt the nc file of Morgane ######

library(ncdf4)
nc_eco <- nc_open("osmose-eec_v4.4_yansong/input/eec_ltlbiomassTons.nc")
largeBenthos_temp <- ncvar_get(nc_eco, varid = "LargeBenthos")[,,1] # no temporal variation
veryLargeBenthos_temp <- ncvar_get(nc_eco, varid = "VLBVeryLargeBenthos")[,,1]

# définir les dimensions
dimLongitude <- ncdim_def( "longitude", "degree", seq(-1.95,2.45,0.1))
dimLatitude <- ncdim_def( "latitude", "degree", seq(49.05,51.15,0.1))
dimTime <- ncdim_def( "time", "unitless", seq(1,1,1), unlim=FALSE,calendar = "standard")

# créer les variables
varLargeBenthos <- ncvar_def("largeBenthos", "ton", list(dimLongitude,dimLatitude,dimTime), 
                             missval=NULL, prec="double")
varVeryLargeBenthos <- ncvar_def("veryLargeBenthos", "ton", list(dimLongitude,dimLatitude,dimTime), 
                                 missval=NULL, prec="double")

# créer nc fichier
nc_morgane <-
  nc_create(
    "largeBenthos_veryLargeBenthos.nc",
    list(
      varLargeBenthos,
      varVeryLargeBenthos
    ),
    force_v4 = FALSE
  )

# write values
  ncvar_put(nc_morgane, varLargeBenthos, largeBenthos_temp, start=c(1,1,1), count=c(-1,-1,1))
  ncvar_put(nc_morgane, varVeryLargeBenthos, veryLargeBenthos_temp, start=c(1,1,1), count=c(-1,-1,1))

nc_close(nc_morgane)

# check if values are correct
nc_morgane_modified <- nc_open("largeBenthos_veryLargeBenthos.nc")
largeBenthos_temp <- ncvar_get(nc_morgane_modified, varid = "largeBenthos")

####### 5.mask for survey #######
library('ncdf4')
survey_example <- nc_open("osmose-eec_v4.4_yansong/input/fishing/allmaps.nc")
map_eec <- nc_open("osmose-eec_v4.4_yansong/input/maps_nc/thornbackRay.nc")

# définir les dimensions
dimLongitude <- ncdim_def( "longitude", "degree", seq(-1.95,2.45,0.1))
dimLatitude <- ncdim_def( "latitude", "degree", seq(49.05,51.15,0.1))
dimTime <- ncdim_def( "time", "time dimension", 1, unlim=TRUE)

# créer les variables
varDemersal <- ncvar_def("demersal", "probability", list(dimLongitude,dimLatitude,dimTime), 
                             missval=NA, prec="float")
# créer nc fichier
map_CGFS <-
  nc_create(
    "survey_maps.nc",
    list(
      varDemersal
    ),
    force_v4 = TRUE
  )

# put values
survey_presence <- ncvar_get(map_eec, varid = "stage0")[,,1]
ncvar_put(map_CGFS, varDemersal, survey_presence)

nc_close(map_CGFS)
# verify the values
map_CGFS <- nc_open("osmose-eec_v4.4_yansong/input/fishing/survey_maps.nc")
presence_CGFS <- ncvar_get(map_CGFS, varid = "demersal")





