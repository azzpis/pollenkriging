library(dismo)
library(dplyr)
library(ggplot2)
library(raster)
library(rgdal)
library(terra)
library(sf)
library(sp)

####please check the directory of the data before starting!!

### I have saved and grouped the CHELSA Trace21k data by bioclimatic variable. 
# thus each bioclimatic variables (e.g. bi01) has a folder with all the raster files from CHELSA Trace21k in it.

for(m in c('bio01','bio02','bio03','bio04','bio05','bio06','bio07','bio08',
            'bio09',
            'bio10',
            'bio11',
            'bio12',
            'bio13','bio14','bio15','bio16','bio17',
           'bio18','bio19','tas','pr','tasmin','tasmax')){
  
  grid <- vect("grid_cropped.shp") #the extent of the gridded data 
  
  
  # #load all the layers
  rastlist <- list.files(path = paste0("E:/CHELSA/",m), pattern='.tif', all.files=TRUE, full.names=T)
  
  #produce a grid for each size. Choose the gridcell size you need.
  for (j in c(10,25,50,75,100)){
      rast_exp = rast("with_covariates/Covariates/CHELSA_TraCE21k_bio08_-100_V1.0.tif")
      rescale = aggregate(rast_exp, fact = j)
      lake_env_2 <- crop(rescale, grid)
      map_climate_sf = raster(lake_env_2)
      polys1 = rasterToPolygons(map_climate_sf)
      shapefile(polys1, paste0("with_covariates/polygon_",j,".shp"))

  }
  #Aggregate data for each size. Be careful the resulting data can be very big. Choose the gridcell size you need.
  for (j in c(10,25,50,75,100)){
    df_all_year = data.frame()
    for (i in 1:length(rastlist)){
      print(i)
      rescale = aggregate(rast(rastlist[i]), fact = j)
      lake_env_2 <- crop(rescale, grid)
      map_climate_sf = raster(lake_env_2)
      test <- try(as(map_climate_sf, "SpatialPixelsDataFrame"))
      if(class(test) %in% 'try-error') {next} 
      else {
        z <- as(map_climate_sf, "SpatialPixelsDataFrame")
        prova = as.data.frame(z)
        prova$run = paste(names(prova[1]))
        names(prova) = c('glacier_elevation', 'x', 'y', 'run') 
        df_all_year = rbind(df_all_year, prova)
      }
    }
    
    write.table(df_all_year, paste0('E:/CHELSA/',m,'/',m,'_',j,'res.txt'))
  }
  
}


