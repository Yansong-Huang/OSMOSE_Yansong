library("osmose")
library("ncdf4")
setwd("/home/barrier/Codes/osmose/git-eec-config")

filename = '/home/barrier/Codes/osmose/old_svn/svn-osmose/branches/config/eec/eec_v3u2/eec_ltlbiomass.nc'
filename = 'morgane-biomass-datarmor.nc'
varlon = 'longitude'
varlat = 'latitude'
varname = 'ltl_biomass'
vartime = 'time'
absolute = TRUE
outputfile = 'corrected-ltl-file.nc'

outvarnames = c(
'Dinoflagellates',
 'Diatoms',
 'Microzoo',
 'Mesozoo',
 'Macrozoo',
 'VSBVerySmallBenthos',
 'SmallBenthos',
 'MediumBenthos',
 'LargeBenthos',
 'VLBVeryLargeBenthos')

input = "/home/barrier/Codes/osmose/old_svn/svn-osmose/branches/config/eec/eec_v3u2/eec_all-parameters.csv"

# Reads the CSV parameter files
param = osmose:::readOsmoseConfiguration(input, absolute=absolute)
if(is.null(filename)) {
  filename = param$ltl$netcdf$file
  if(is.null(filename)) {
    warning("LTL filename not found in configuration file, nothing to do.")
    return(invisible())
  }
}

# recovers the index for plk as 
pltindex = names(param$plankton$name)

# Opens the old netcdf file
ncin = nc_open(filename)

# Recover spatial coordinates
if(length(ncin$var[[varlon]]$varsize) == 2) {
  # if coordinates are 2D, then extract data as 1D
  lon = ncvar_get(ncin, varid=varlon)
  lat = ncvar_get(ncin, varid=varlat)
} else {
  # If longitudes are 1D, nothing to do.
  lon = ncvar_get(ncin, varid=varlon)
  lat = ncvar_get(ncin, varid=varlat)
}

shape = dim(lon)
nx = shape[1]
ny = shape[2]

# Recovers the original time coordinates and attributes (for units)
if(is.null(ncin$var[[vartime]])) {
  attr_time = ncatt_get(ncin, vartime)
  time = ncin$dim[[vartime]]$vals
} else {
  attr_time = ncatt_get(ncin, vartime)
  time = ncvar_get(ncin, varid=vartime)
}

# recovers the number of plankton within the file
n_ltl_file = ncin$dim$ltl$len

# biomass is of size (lon, lat, ltl, time)
biomass_units = ncatt_get(ncin, varname)$units
biomass = ncvar_get(ncin, varid=varname)

# Creates the output dimensions (lon, lat, time)
dim_time = ncdim_def("time", attr_time$units, time)
dim_lon = ncdim_def("x", "", 1:nx)
dim_lat = ncdim_def("y", "", 1:ny)
dims = list(dim_lon=dim_lon, dim_lat=dim_lat, dim_time=dim_time)

# Loop over all the plankton classes to initialise variables in the NetCDF
list_vars = c()
for(i in 1:n_ltl_file) {
  ltl_var = param$plankton$name[[pltindex[i]]]
  ltl_var = outvarnames[i]
  var_nc = ncvar_def(ltl_var, biomass_units, dims, longname=ltl_var)
  list_vars[[i]] = var_nc
}

dimcoords = list(dim_lon=dim_lon, dim_lat=dim_lat)
list_vars[[n_ltl_file + 1]] = ncvar_def("longitude", "", dimcoords, longname="longitude")
list_vars[[n_ltl_file + 2]] = ncvar_def("latitude", "", dimcoords, longname="latitude")

# Opens the output NetCDF file
ncout = nc_create(outputfile, list_vars)

# loops over all the LTL classes and write data into 
# the file
for(i in 1:n_ltl_file) {
  ltl_var = param$plankton$name[[pltindex[i]]]
  ltl_var = outvarnames[i]
  ncvar_put(ncout, ltl_var, biomass[, , i, ])
}

ncvar_put(ncout, "longitude", lon)
ncvar_put(ncout, "latitude", lat)

# Add the calendar attribute to the time variable
if(!is.null(attr_time$calendar)) {
  ncatt_put(ncout, "time", "calendar", attr_time$calendar)
}

nc_close(ncout)
nc_close(ncin)
