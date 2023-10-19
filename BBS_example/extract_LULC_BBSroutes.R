
library(sf)
library(terra)

terraOptions(progress=0) # no progress bar

# Import LULC raster from National Landcover Database for 2013
lulc <- rast(file.path("BBS_example","GIS","nlcd_2013_land_cover_l48_20210604.img"))

plot(lulc)

# Import BBS routes centroids
rte.cen <- read_sf(file.path("BBS_example","GIS","BBSrte_MBRrba_BCR_MBRrbaLocations.shp"))

rte.cen <- st_transform(rte.cen,st_crs(lulc))

# make polygons using routes centroids and a buffer of 20km
rte.buf <- st_buffer(rte.cen,20000)

plot(st_geometry(rte.buf),add=T)

# Organize table with summarized classes
classes <- as.data.frame(matrix(
  c( 
    "Unclassified",                 NA,
    "Open Water",                   "Water",
    "Perennial Snow/Ice",           "Water",
    "Developed, Open Space",        "Developed", 
    "Developed, Low Intensity",     "Developed",
    "Developed, Medium Intensity",  "Developed",
    "Developed, High Intensity",    "Developed",
    "Barren Land",                  "Open",
    "Deciduous Forest",             "Forest",
    "Evergreen Forest",             "Forest",
    "Mixed Forest",                 "Forest",
    "Shrub/Scrub",                  "Open",
    "Herbaceous",                   "Open",
    "Hay/Pasture",                  "Crop",
    "Cultivated Crops",             "Crop",
    "Woody Wetlands",               "Wetland",
    "Emergent Herbaceous Wetlands", "Wetland"
  ),ncol=2,byrow=T,dimnames=list(NULL,c("orig.class","class"))
))

# Trasform to factors
classes$orig.class <- as.factor(classes$orig.class)
classes$class <- as.factor(classes$class)


# Extract the proportion of each LULC class for each route buffer ---------

# Create empty object to receive proportions
rte.lulc <- as.data.frame(cbind(rteno=rte.buf$rteno,
                                matrix(NA,ncol=length(levels(classes$class)),nrow=nrow(rte.buf),
                                       dimnames=list(NULL,levels(classes$class)))
))

# Loop over each route buffer polygon
  #!!! took ~8h

for(w in 1:nrow(rte.lulc)){
  cat("\nRoute",w,"/",nrow(rte.lulc))
  # extract LULC pixels with weights
  tmp <- extract(lulc,vect(rte.buf[w,]),weights=T)
  
  # get summarized class
  tmp$class <- factor(classes[match(tmp$`NLCD Land Cover Class`,classes$orig.class),"class"],
                      levels=sort(unique(classes$class)))
  
  # calculate the proportion and fill the data.frame
  props <- tapply(tmp$weight,tmp$class, sum, na.rm=T) / sum(tmp$weight[!is.na(tmp$class)])
  
  rte.lulc[w,2:ncol(rte.lulc)] <- props
} #w

#

write.csv(rte.lulc,file.path("BBS_example","data","LULC20km_BBS-routes_2013.csv"),row.names=F)

# Check..
any(rte.lulc$rteno != rte.buf$rteno) # FALSE
 
any(duplicated(rte.lulc$rteno)) # FALSE
 
sum(rowSums(rte.lulc[,2:7],na.rm=T)) != nrow(rte.lulc) # FALSE
