import numpy as np
import xarray as xr

data = xr.open_dataset('save_eec_ltlbiomassTons.nc')
factors = [
9.5,
9.5,
11.88,
55.44,
1,
2,
2,
2,
2,
2]

names=[
'Dinoflagellates',
'Diatoms',
'Microzoo',
'Mesozoo',
'Macrozoo',
'VSBVerySmallBenthos',
'SmallBenthos',
'MediumBenthos',
'LargeBenthos',
'VLBVeryLargeBenthos']

for p in range(len(factors)):
    var = names[p]
    data[var] = data[var] * factors[p]
    data[var].attrs['description'] = 'Original value multiplied by %f' %factors[p]
data.to_netcdf('eec_ltlbiomassTons.nc')
