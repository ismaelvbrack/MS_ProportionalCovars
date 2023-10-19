
library(sf)

#* Import BBS routes centroids shape
rte.cen <- read_sf(file.path("BBS_example","GIS","BBSrte_MBRrba_BCR_MBRrbaLocations.shp"))

#* Import BBS richness data for native bird species
bbs.r <- read.csv(file.path("BBS_example","data","spp_richness_native_species.csv"))

# Keep only native bird richness estimate for 2013
R2013 <- bbs.r[,c("rteno","est_species_2013")]

# exclude missing data
R2013 <- R2013[-which(is.na(R2013$est_species_2013)),]

#!! Some routes do not have centroids...
# R2013[which(!R2013$rteno %in% rte.cen$rteno),"rteno"]
# Exclude these routes
R2013 <- R2013[-which(!R2013$rteno %in% rte.cen$rteno),]

hist(R2013$est_species_2013,40)

write.csv(R2013,file.path("BBS_example","data","BBS_NativeBirds_Richness2013.csv"),row.names=F)
