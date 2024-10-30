# Emplacement des parcs éoliens
# Auteur : Yansong Huang
# Date de création : 2024-10-28

library(sf)

# calvados_cells <- c(195,196)
# Fecamp_cells <- c(383,428)
# Dieppe_cells <- c(526,527)
# Dunkerque_cells <- c(989,990)
# Centre_manche_1_cells <- c(372,373,418)
# Centre_manche_2_cells <- c(374,375,419)
# Rampion_cells <- c(735:740)

OWF_locations <- list(
  calvados = list(lon = c(15,16), lat = c(5,5)),
  fecamp = list(lon = c(23,23), lat = c(9,10)),
  dieppe = list(lon=c(31,32), lat = c(12,12)),
  dunkerque = list(lon=c(44,45), lat=c(22,22)),
  manche_1 = list(lon=c(12,13,13),lat=c(9,9,10)),
  manche_2 = list(lon=c(14,15,14),lat=c(9,9,10)),
  rampion = list(lon=c(15:20),lat=rep(17,6)),
  future = list(lon=c(rep(16,3),rep(20:24,2),25:27,25,26,29,30),
                lat=c(9,10,rep(11,6),rep(12,8),rep(13,4)))
)

# cout_cells <- st_read(file.path("OWF_grid",paste("OWF_cout.shp",sep = "")))$id
# protection_cells <- st_read(file.path("OWF_grid",paste("OWF_protection.shp",sep = "")))$id
# loin_cells <- st_read(file.path("OWF_grid",paste("OWF_loin.shp",sep = "")))$id
# equilibre_cells <- st_read(file.path("OWF_grid",paste("OWF_equilibre.shp",sep = "")))$id