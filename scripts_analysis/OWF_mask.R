library(sf)

# Read OWF cells
OWF_1_cells <- st_read(file.path("OWF_grid", "OWF_1.shp"))$id
OWF_2_cells <- st_read(file.path("OWF_grid", "OWF_2.shp"))$id

# Define a general function to create a mask
create_mask <- function(cells, grid_dim = c(45, 22)) {
  lon <- cells %% grid_dim[1]
  lat <- cells %/% grid_dim[1] + 1
  
  # Manual correction. OWF Dunkirk is on the corner of the map. 
  lon[8] <- 45
  lat[8] <- 22
  
  # Generate latitude-longitude combinations
  OWF_combinations <- matrix(c(lat, lon), ncol = 2)
  
  # Create a mask and select cells
  mask <- array(FALSE, dim = grid_dim)
  for (i in 1:nrow(OWF_combinations)) {
    mask[OWF_combinations[i, 2], OWF_combinations[i, 1]] <- TRUE
  }
  return(mask)
}

# Read each dataset and apply the general function
manual_correction <- list(index = 8, lon = 45, lat = 22)

# Mask for balance
balance_cells <- st_read(file.path("OWF_grid", "OWF_equilibre.shp"))$id
mask_OWF_equilibre <- create_mask(c(c(OWF_1_cells, OWF_2_cells, balance_cells)))

# Mask for cost
cost_cells <- st_read(file.path("OWF_grid", "OWF_cout.shp"))$id
mask_OWF_cout <- create_mask(c(c(OWF_1_cells, OWF_2_cells, cost_cells)))

# Mask for protection
protection_cells <- st_read(file.path("OWF_grid", "OWF_protection.shp"))$id
mask_OWF_protection <- create_mask(c(c(OWF_1_cells, OWF_2_cells, protection_cells)))

# Mask for distance
distance_cells <- st_read(file.path("OWF_grid", "OWF_loin.shp"))$id
mask_OWF_loin <- create_mask(c(c(OWF_1_cells, OWF_2_cells, distance_cells)))
